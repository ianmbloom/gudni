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

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Layout.Token


import Control.Lens

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

instance (Space s) => CanFill (ShapeTree token s) where
    type UnFilled (ShapeTree token s) = CompoundTree s
    withFill substance = ShapeTree . SLeaf . SItem . Just . SRep defaultValue substance

instance Show token => HasToken (ShapeTree token s) where
  type TokenOf (ShapeTree token s) = token

instance Show token => Tokenized (ShapeTree token s) where
  overToken fToken = overShapeTree (mapSItem (fmap (over sRepToken fToken)))
