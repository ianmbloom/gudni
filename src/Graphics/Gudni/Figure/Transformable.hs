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
-- Module      :  Graphics.Gudni.Figure.Transformable
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- TypeClass for transformation of objects.

module Graphics.Gudni.Figure.Transformable
 ( SimpleTransformable(..)
 , Transformable(..)
 , Projectable(..)
 , scaleBy
 , translateByXY
 , deltaOnAxis
 , translateOnAxis
)
where

import Graphics.Gudni.Figure.HasDefault
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Axis
import Graphics.Gudni.Figure.OpenCurve

import Graphics.Gudni.Util.Debug

import Control.Monad.Random
import Control.Applicative
import Control.Lens
import Control.DeepSeq

import Data.Traversable
import Data.Hashable
import Data.Either

import System.Random

class (HasSpace t) => SimpleTransformable t where
  translateBy :: Point2 (SpaceOf t) -> t -> t
  stretchBy   :: Point2 (SpaceOf t) -> t -> t

class (SimpleTransformable t) => Transformable t where
  rotateBy    :: Angle (SpaceOf t) -> t -> t

class (Transformable t) => Projectable t where
  projectOnto :: OpenCurve (SpaceOf t) -> t -> t

scaleBy :: SimpleTransformable t => SpaceOf t -> t -> t
scaleBy s = stretchBy (pure s)

translateByXY :: (HasSpace t, SimpleTransformable t) => SpaceOf t -> SpaceOf t -> t -> t
translateByXY x y = translateBy $ makePoint x y

deltaOnAxis :: forall s axis . (Space s, Axis axis) => axis -> s -> Point2 s
deltaOnAxis axis s = set (with axis) s zeroPoint

translateOnAxis :: forall t axis . (Axis axis, SimpleTransformable t) => axis -> SpaceOf t -> t -> t
translateOnAxis axis s = translateBy (deltaOnAxis axis s)
