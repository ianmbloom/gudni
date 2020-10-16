{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
  , rgbaColor
  , hslColor

  , pureRed, pureGreen, pureBlue
  , red, orange, yellow, green, blue, purple
  , black, gray, white
  , clearBlack

  , isOpaque
  , composite
  , saturate
  , lighten
  , light
  , dark
  , veryDark
  , transparent
  , mixColor

  , redish, orangeish, yellowish, greenish, blueish, purpleish
  , colorToRGBA8
  , colourToColor
  )
where

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
newtype Color = Color {_unColor :: V4 Float} deriving (Eq, Ord, Show)
makeLenses ''Color

instance HasSpace Color where
  type SpaceOf Color = SubSpace

cRed   :: Lens' Color Float
cGreen :: Lens' Color Float
cBlue  :: Lens' Color Float
cAlpha :: Lens' Color Float
cRed   = unColor . _x
cGreen = unColor . _y
cBlue  = unColor . _z
cAlpha = unColor . _w

colorToColour :: Color -> (C.Colour Float, Float)
colorToColour color = (C.sRGB (color ^. cRed) (color ^. cGreen) (color ^. cBlue), color ^. cAlpha)

colourToColor :: Float -> C.Colour Float -> Color
colourToColor alpha colour = Color $ C.uncurryRGB V4 (C.toSRGB colour) alpha

isOpaque color = color ^. cAlpha > 0.999

-- | Red based on simple rgb values.
pureRed :: Color
pureRed   = rgbaColor 1 0 0 1
-- | Green based on simple rgb values.
pureGreen :: Color
pureGreen = rgbaColor 0 1 0 1
-- | Blue based on simple rgb values.
pureBlue :: Color
pureBlue  = rgbaColor 0 0 1 1

-- | Transparent color with rgb values of zero.
clearBlack :: Color
clearBlack = rgbaColor 0 0 0 0

-- | Generate a 'Color' from hue saturation and lightness values.
hslColor :: Float -> Float -> Float -> Color
hslColor hue saturation lightness = colourToColor 1 (C.uncurryRGB C.sRGB $ C.hsl hue saturation lightness)

-- | Generate a 'Color' from simple RGB and alpha values.
rgbaColor :: Float -> Float -> Float -> Float -> Color
rgbaColor r g b a = Color $ V4 r g b a

composite :: Color -> Color -> Color
composite f b =
  let alphaOut = f ^. cAlpha + b ^. cAlpha * (1 - f ^. cAlpha)
  in
  if alphaOut > 0
  then Color $ (((f ^. unColor) ^* (f ^. cAlpha)) ^+^ ((b ^. unColor) ^* ((b ^. cAlpha) * (1 - (f ^. cAlpha))))) ^* (1 / alphaOut)
  else clearBlack

-- | Generate a 'Color' based on the input color by multiplying the saturation by a factor.
saturate :: Float -> Color -> Color
saturate sat color =
    let (colour, alpha) = colorToColour color
        (h, s, l) = C.hslView . C.toSRGB $ colour
        c = C.uncurryRGB C.sRGB $ C.hsl h (clamp 0 1.0 $ sat * s) l
    in  colourToColor alpha c

-- | Generate a 'Color' based on the input color by multiplying the lightness by a factor.
lighten :: Float -> Color ->  Color
lighten light color =
  let (colour, alpha) = colorToColour color
      (h, s, l) = C.hslView . C.toSRGB $ colour
      c = C.uncurryRGB C.sRGB $ C.hsl h s (clamp 0 1.0 $ light * l)
  in  colourToColor alpha c

-- | Make a slightly lighter version of the color.
light :: Color -> Color
light = saturate 0.8  . lighten 1.25

-- | Make a slightly darker version of the color.
dark :: Color -> Color
dark  = saturate 1.25 . lighten 0.75

-- | Make a much darker version of the color.
veryDark :: Color -> Color
veryDark  = saturate 1.25 . lighten 0.5

-- | Mix two colors and return the alpha value of the first color.
mixColor :: Color -> Color -> Color
mixColor a b =
  let (colourA, alphaA) = colorToColour a
      (colourB, alphaB) = colorToColour b
      blended = C.blend 0.5 colourA colourB
  in  colourToColor alphaA blended

-- | Blend two colors by pushing the hue value toward the second color.
influenceHue :: Float -> Color -> Color -> Color
influenceHue amount b a =
  let (color,   alpha) = colorToColour a
      (blender,    _ ) = colorToColour b
      blended = C.blend amount blender color
      (h,s,_) = C.hslView . C.toSRGB $ blended
      (_,_,l) = C.hslView . C.toSRGB $ color
  in  transparent alpha (hslColor h s l)

-- | The amount to mix in a color to ish it.
ishAmount = 0.05

-- | Make a slightly redder version of the color.
redish    :: Color -> Color
-- | Make a slightly oranger version of the color.
orangeish :: Color -> Color
-- | Make a slightly yellower version of the color.
yellowish :: Color -> Color
-- | Make a slightly greener version of the color.
greenish  :: Color -> Color
-- | Make a slightly bluer version of the color.
blueish   :: Color -> Color
-- | Make a slightly purpler version of the color.
purpleish :: Color -> Color
redish    = influenceHue ishAmount red
orangeish = influenceHue ishAmount orange
yellowish = influenceHue ishAmount yellow
greenish  = influenceHue ishAmount green
blueish   = influenceHue ishAmount blue
purpleish = influenceHue ishAmount purple

-- | Replace the alpha value of a color.
transparent :: Float -> Color -> Color
transparent a = set cAlpha a


instance Storable Color where
    sizeOf _    = sizeOf (undefined :: V4 CFloat)
    alignment _ = alignment (undefined :: V4 CFloat)
    peek ptr = Color . fmap (realToFrac :: CFloat -> Float) <$> peek (F.castPtr ptr)
    poke ptr = poke (F.castPtr ptr) . fmap (realToFrac :: Float -> CFloat) . view unColor


mAXcHANNELfLOAT :: Float
mAXcHANNELfLOAT = 255.0

colorToRGBA8 :: Color -> V4 Word8
colorToRGBA8 = fmap (round . (* mAXcHANNELfLOAT)) . view unColor

-- | Wrapped colors
red    = colourToColor 1 N.red
orange = colourToColor 1 N.orange
yellow = colourToColor 1 N.yellow
green  = colourToColor 1 N.green
blue   = colourToColor 1 N.blue
purple = colourToColor 1 N.purple
white  = colourToColor 1 N.white
gray   = colourToColor 1 N.gray
black  = colourToColor 1 C.black
