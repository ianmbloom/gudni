{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveGeneric              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Color
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- This is mainly a wrapper around Data.Colour that allows for easy manipulation of alpha transparency.

module Graphics.Gudni.Figure.Substance.Color
  ( Color(..)
  , unColor
  , rgbaColor
  , hslColor

  , cRed
  , cGreen
  , cBlue
  , cAlpha

  , pureRed, pureGreen, pureBlue
  , red, orange, yellow, green, cyan, blue, purple
  , black, gray, white
  , clearBlack
  , opaqueWhite

  , isOpaque
  , isClear
  , composite
  , saturate
  , lighten
  , light
  , dark
  , veryDark
  , transparent
  , mixColor

  , redish, orangeish, yellowish, greenish, blueish, purpleish
  , word8ToCFloat
  , colorToRGBA8
  , colourToColor
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.StorableM

import Control.Monad.Random

import Data.Word
import Data.Bits
import Data.List
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import Data.Foldable

import Foreign.C.Types
import qualified Foreign as F
import Linear.V4
import Linear.Vector

import System.Random
import qualified Data.Colour as C
import qualified Data.Colour.RGBSpace.HSL as C
import qualified Data.Colour.RGBSpace as C
import qualified Data.Colour.SRGB as C
import qualified Data.Colour.Names as N

import Control.Lens

-- | Wrapper for 'Data.Colour.Colour' that adds simple alpha.
newtype Color s = Color {_unColor :: V4 s} deriving (Eq, Ord, Generic)
makeLenses ''Color

instance Show s => Show (Color s) where
  show (Color v4) = "Color " ++ foldl1 (\x y -> x++" "++y) (fmap show v4)

instance Out s => Out (Color s) where
  doc (Color v4) = text "Color" <+> foldl1 (<+>) (fmap doc v4)
  docPrec _ = doc

instance Space s => HasSpace (Color s) where
  type SpaceOf (Color s) = s

instance Space s => HasDefault (Color s) where
  defaultValue = clearBlack

cRed   :: Lens' (Color s) s
cGreen :: Lens' (Color s) s
cBlue  :: Lens' (Color s) s
cAlpha :: Lens' (Color s) s
cRed   = unColor . _x
cGreen = unColor . _y
cBlue  = unColor . _z
cAlpha = unColor . _w

colorToColour :: (Space s) => Color s -> (C.Colour s, s)
colorToColour color = (C.sRGB (color ^. cRed) (color ^. cGreen) (color ^. cBlue), color ^. cAlpha)

colourToColor :: (Space s) => s -> C.Colour s -> Color s
colourToColor alpha colour = Color $ C.uncurryRGB V4 (C.toSRGB colour) alpha

isOpaque :: (Space s) => Color s -> Bool
isOpaque color = color ^. cAlpha > 0.999

isClear :: (Space s) => Color s -> Bool
isClear color = color ^. cAlpha < 0.001

-- | Red based on simple rgb values.
pureRed :: Num s => Color s
pureRed   = rgbaColor 1 0 0 1
-- | Green based on simple rgb values.
pureGreen :: Num s => Color s
pureGreen = rgbaColor 0 1 0 1
-- | Blue based on simple rgb values.
pureBlue :: Num s => Color s
pureBlue  = rgbaColor 0 0 1 1
-- | Transparent color with rgb values of zero.
clearBlack :: Num s => Color s
clearBlack = rgbaColor 0 0 0 0
-- | Completely opaque color with maximum values on each channell
opaqueWhite :: Num s => Color s
opaqueWhite = rgbaColor 1 1 1 1

-- | Generate a 'Color' from hue saturation and lightness values.
hslColor :: (Space s) => s -> s -> s -> Color s
hslColor hue saturation lightness = colourToColor 1 (C.uncurryRGB C.sRGB $ C.hsl hue saturation lightness)

-- | Generate a 'Color' from simple RGB and alpha values.
rgbaColor :: s -> s -> s -> s -> Color s
rgbaColor r g b a = Color $ V4 r g b a

composite :: (Space s) => Color s -> Color s -> Color s
composite f b =
  let alphaOut = f ^. cAlpha + (b ^. cAlpha * (1 - f ^. cAlpha))
  in
  if alphaOut > 0
  then Color $ ((f ^. unColor ^* f ^. cAlpha) ^+^ (b ^. unColor ^* (b ^. cAlpha * (1 - f ^. cAlpha)))) ^* (1 / alphaOut)
  else clearBlack

-- | Generate a 'Color' based on the input color by multiplying the saturation by a factor.
saturate :: (Space s) => s -> Color s -> Color s
saturate sat color =
    let (colour, alpha) = colorToColour color
        (h, s, l) = C.hslView . C.toSRGB $ colour
        c = C.uncurryRGB C.sRGB $ C.hsl h (clamp 0 1.0 $ sat * s) l
    in  colourToColor alpha c

-- | Generate a 'Color' based on the input color by multiplying the lightness by a factor.
lighten :: (Space s) => s -> Color s -> Color s
lighten light color =
  let (colour, alpha) = colorToColour color
      (h, s, l) = C.hslView . C.toSRGB $ colour
      c = C.uncurryRGB C.sRGB $ C.hsl h s (clamp 0 1.0 $ light * l)
  in  colourToColor alpha c

-- | Make a slightly lighter version of the color.
light :: (Space s) => Color s -> Color s
light = saturate 0.8  . lighten 1.25

-- | Make a slightly darker version of the color.
dark :: (Space s) => Color s -> Color s
dark  = saturate 1.25 . lighten 0.75

-- | Make a much darker version of the color.
veryDark :: (Space s) => Color s -> Color s
veryDark  = saturate 1.25 . lighten 0.5

-- | Mix two colors and return the alpha value of the first color.
mixColor :: (Space s) => Color s -> Color s -> Color s
mixColor a b =
  let (colourA, alphaA) = colorToColour a
      (colourB, alphaB) = colorToColour b
      blended = C.blend 0.5 colourA colourB
  in  colourToColor alphaA blended

-- | Blend two colors by pushing the hue value toward the second color.
influenceHue :: (Space s) => s -> Color s -> Color s -> Color s
influenceHue amount b a =
  let (color,   alpha) = colorToColour a
      (blender,    _ ) = colorToColour b
      blended = C.blend amount blender color
      (h,s,_) = C.hslView . C.toSRGB $ blended
      (_,_,l) = C.hslView . C.toSRGB $ color
  in  transparent alpha (hslColor h s l)

-- | The amount to mix in a color to ish it.
ishAmount :: Space s => s
ishAmount = 0.05

-- | Make a slightly redder version of the color.
redish    :: (Space s) => Color s -> Color s
-- | Make a slightly oranger version of the color.
orangeish :: (Space s) => Color s -> Color s
-- | Make a slightly yellower version of the color.
yellowish :: (Space s) => Color s -> Color s
-- | Make a slightly greener version of the color.
greenish  :: (Space s) => Color s -> Color s
-- | Make a slightly bluer version of the color.
blueish   :: (Space s) => Color s -> Color s
-- | Make a slightly purpler version of the color.
purpleish :: (Space s) => Color s -> Color s
redish    = influenceHue ishAmount red
orangeish = influenceHue ishAmount orange
yellowish = influenceHue ishAmount yellow
greenish  = influenceHue ishAmount green
blueish   = influenceHue ishAmount blue
purpleish = influenceHue ishAmount purple

-- | Replace the alpha value of a color.
transparent :: (Space s) => s -> Color s -> Color s
transparent a = set cAlpha a


instance (Space s, Storable s) => Storable (Color s) where
    sizeOf _    = sizeOf (undefined :: V4 CFloat)
    alignment _ = alignment (undefined :: V4 CFloat)
    peek ptr = Color . fmap (realToFrac :: CFloat -> s) <$> peek (F.castPtr ptr)
    poke ptr = poke (F.castPtr ptr) . fmap (realToFrac :: s -> CFloat) . view unColor


mAXcHANNELsPACE :: Space s => s
mAXcHANNELsPACE = 255.0

mAXcHANNELcfLOAT :: CFloat
mAXcHANNELcfLOAT = 255.0

word8ToCFloat :: Word8 -> CFloat
word8ToCFloat word = realToFrac word / mAXcHANNELcfLOAT;

colorToRGBA8 :: Space s => Color s -> Color Word8
colorToRGBA8 = Color . fmap (round . (* mAXcHANNELsPACE)) . view unColor

-- | Wrapped colors
red    :: Space s => Color s
orange :: Space s => Color s
yellow :: Space s => Color s
green  :: Space s => Color s
cyan   :: Space s => Color s
blue   :: Space s => Color s
purple :: Space s => Color s
white  :: Space s => Color s
gray   :: Space s => Color s
black  :: Space s => Color s
red    = colourToColor 1 N.red
orange = colourToColor 1 N.orange
yellow = colourToColor 1 N.yellow
green  = colourToColor 1 N.green
cyan   = colourToColor 1 N.cyan
blue   = colourToColor 1 N.blue
purple = colourToColor 1 N.purple
white  = colourToColor 1 N.white
gray   = colourToColor 1 N.gray
black  = colourToColor 1 C.black
