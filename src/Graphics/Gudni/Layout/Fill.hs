{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Fill
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Typeclass for assigning textures to masks.

module Graphics.Gudni.Layout.Fill
  ( CanFill(..)
  , withColor
  , withTexture
  , withRadialGradient
  , withLinearGradient
  )
where

import Graphics.Gudni.Figure
import Control.Lens
import Graphics.Gudni.Raster.TraverseShapeTree

class (HasSpace a) => CanFill a where
    type UnFilled a :: *
    withFill :: Substance NamedTexture (SpaceOf a) -> UnFilled a -> a

withColor :: (CanFill a) => Color -> UnFilled a -> a
withColor color = withFill (Solid color)

withTexture :: (CanFill a) => NamedTexture -> UnFilled a -> a
withTexture texture = withFill (Texture texture)

withRadialGradient :: (CanFill a) => Point2 (SpaceOf a) -> SpaceOf a -> Color -> SpaceOf a -> Color -> UnFilled a -> a
withRadialGradient center innerRadius innerColor outerRadius outerColor =
  withFill (Radial $ RadialGradient center innerRadius outerRadius innerColor outerColor)

withLinearGradient :: (CanFill a) => Point2 (SpaceOf a) -> Color -> Point2 (SpaceOf a) -> Color -> UnFilled a -> a
withLinearGradient start startColor end endColor =
  withFill (Linear $ LinearGradient start end startColor endColor)

instance (Space s) => CanFill (ShapeTree token textureLabel s) where
    type UnFilled (ShapeTree token textureLabel s) = CompoundTree s
    withFill substance = SLeaf . Just . SRep defaultValue substance

instance HasToken (ShapeTree token textureLabel s) where
  type TokenOf (ShapeTree token textureLabel s) = token

instance Tokenized (ShapeTree token s) where
  overToken fToken = mapSTree (fmap (over sRepToken fToken))
