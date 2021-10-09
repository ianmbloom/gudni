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
  , makeColor
  , rgbaColor

  , cRed
  , cGreen
  , cBlue
  , cAlpha

  , isOpaque
  , isClear
  , composite

  , transparent
  , blend
  , mixColor
  , colourToColor

  , pureRed, pureGreen, pureBlue
  , red, orange, yellow, green, cyan, blue, purple
  , black, gray, white
  , clearBlack
  , opaqueWhite
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
import qualified Data.Colour.RGBSpace.HSV as C
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

makeColor :: s -> s -> s -> s -> Color s
makeColor r g b a = Color $ V4 r g b a

-- | Generate a 'Color' from simple RGB and alpha values.
rgbaColor :: s -> s -> s -> s -> Color s
rgbaColor = makeColor

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

composite :: (Space s) => Color s -> Color s -> Color s
composite f b =
  let alphaOut = f ^. cAlpha + (b ^. cAlpha * (1 - f ^. cAlpha))
  in
  if alphaOut > 0
  then set cAlpha alphaOut $ Color $ ((f ^. unColor ^* f ^. cAlpha) ^+^ (b ^. unColor ^* (b ^. cAlpha * (1 - f ^. cAlpha)))) ^* (1 / alphaOut)
  else clearBlack

dissolve :: Space s => s -> Color s -> Color s
dissolve s (Color v4) = Color $ fmap (*s) v4

rgbaAdd :: Space s => Color s -> Color s -> Color s
rgbaAdd (Color v0) (Color v1) = Color $ v0 + v1

blend :: (Space s) => s -> Color s -> Color s -> Color s
blend weight c1 c2 =
   rgbaAdd (dissolve (1-weight) c1) (dissolve weight c2)

-- | Mix two colors and return the alpha value of the first color.
mixColor :: (Space s) => Color s -> Color s -> Color s
mixColor a b = blend 0.5 a b

-- | Replace the alpha value of a color.
transparent :: (Space s) => s -> Color s -> Color s
transparent a = set cAlpha a


instance (Space s, Storable s) => Storable (Color s) where
    sizeOf _    = sizeOf (undefined :: V4 CFloat)
    alignment _ = alignment (undefined :: V4 CFloat)
    peek ptr = Color . fmap (realToFrac :: CFloat -> s) <$> peek (F.castPtr ptr)
    poke ptr = poke (F.castPtr ptr) . fmap (realToFrac :: s -> CFloat) . view unColor

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
