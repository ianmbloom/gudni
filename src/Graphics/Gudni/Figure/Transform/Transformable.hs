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

module Graphics.Gudni.Figure.Transform.Transformable
 ( SimpleTransformable(..)
 , Transformable(..)
 , Projectable(..)
 , scaleBy
 , translateByXY
 , deltaOnAxis
 , translateOnAxis
)
where

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Figure.Transform.Transformer

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
  simpleTransformWith :: SimpleTransformer (SpaceOf t) -> t -> t


class (SimpleTransformable t) => Transformable t where
  rotateBy    :: Angle (SpaceOf t) -> t -> t
  transformWith :: Transformer (SpaceOf t) -> t -> t

class (Transformable t) => Projectable t where
  projectOnto :: OpenCurve (SpaceOf t) -> t -> t

scaleBy :: SimpleTransformable t => SpaceOf t -> t -> t
scaleBy s = stretchBy (pure s)

translateByXY :: (HasSpace t, SimpleTransformable t) => Ax Horizontal (SpaceOf t) -> Ax Vertical (SpaceOf t) -> t -> t
translateByXY x y = translateBy $ makePoint x y

deltaOnAxis :: forall s axis . (Space s, Axis axis) => axis -> Along axis s -> Point2 s
deltaOnAxis axis s = set (along axis) s zeroPoint

translateOnAxis :: forall t axis . (Axis axis, SimpleTransformable t) => axis -> Along axis (SpaceOf t) -> t -> t
translateOnAxis axis s = translateBy (deltaOnAxis axis s)

instance (Space s) => SimpleTransformable (Transformer s) where
    translateBy p = CombineTransform (Simple $ Translate p)
    stretchBy   p = CombineTransform (Simple $ Stretch p)
    simpleTransformWith t = CombineTransform (Simple t)

instance (Space s) => Transformable (Transformer s) where
    rotateBy    a = CombineTransform (Rotate a)
    transformWith t = CombineTransform t

instance (Space s) => Projectable (Transformer s) where
    projectOnto path = CombineTransform (Project path)
