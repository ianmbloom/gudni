{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Transform.Transformable
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- TypeClass for transformation of objects.

module Graphics.Gudni.Figure.Transform.Transformable
 ( Transformable(..)
 , Projectable(..)
 , scaleBy
 , translateByXY
 , deltaOnAxis
 , translateOnAxis
)
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Shape

import Graphics.Gudni.Util.Debug

import Control.Monad.Random
import Control.Applicative
import Control.Lens


class (HasSpace t) => Transformable t where
  translateBy :: Point2 (SpaceOf t) -> t -> t
  stretchBy   :: Point2 (SpaceOf t) -> t -> t
  rotateBy    :: Angle (SpaceOf t) -> t -> t

class (Transformable t) => Projectable t where
  projectOnto :: OpenCurve (SpaceOf t) -> t -> t

scaleBy :: Transformable t => SpaceOf t -> t -> t
scaleBy s = stretchBy (pure s)

translateByXY :: (Transformable t) => Ax Horizontal (SpaceOf t) -> Ax Vertical (SpaceOf t) -> t -> t
translateByXY x y = translateBy $ makePoint x y

deltaOnAxis :: forall s axis . (Space s, Axis axis) => axis -> Along axis s -> Point2 s
deltaOnAxis axis s = set (along axis) s zeroPoint

translateOnAxis :: forall t axis . (Axis axis, Transformable t) => axis -> Along axis (SpaceOf t) -> t -> t
translateOnAxis axis s = translateBy (deltaOnAxis axis s)
