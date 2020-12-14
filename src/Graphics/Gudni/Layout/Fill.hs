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
import Graphics.Gudni.Layout.Token

import Control.Lens

class (HasSpace a) => CanFill a where
    type UnFilled a :: *
    withFill :: Substance NamedTexture (SpaceOf a) -> UnFilled a -> a

withColor :: (CanFill a) => Color (SpaceOf a) -> UnFilled a -> a
withColor color = withFill (Solid color)

withTexture :: (CanFill a) => NamedTexture -> UnFilled a -> a
withTexture texture = withFill (Texture texture)

withRadialGradient :: (CanFill a) => Point2 (SpaceOf a) -> SpaceOf a -> Color (SpaceOf a) -> SpaceOf a -> Color (SpaceOf a) -> UnFilled a -> a
withRadialGradient center innerRadius innerColor outerRadius outerColor =
  withFill (Radial $ RadialGradient center innerRadius outerRadius innerColor outerColor)

withLinearGradient :: (CanFill a) => Point2 (SpaceOf a) -> Color (SpaceOf a) -> Point2 (SpaceOf a) -> Color (SpaceOf a) -> UnFilled a -> a
withLinearGradient start startColor end endColor =
  withFill (Linear $ LinearGradient start end startColor endColor)
