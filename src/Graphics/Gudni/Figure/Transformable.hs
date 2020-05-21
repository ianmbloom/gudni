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
 , translateByXY
)
where

import Graphics.Gudni.Figure.HasDefault
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Point

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
  scaleBy     :: SpaceOf t -> t -> t
  stretchBy   :: Point2 (SpaceOf t) -> t -> t

class (SimpleTransformable t) => Transformable t where
  rotateBy    :: Angle (SpaceOf t) -> t -> t

translateByXY :: (HasSpace t, SimpleTransformable t) => SpaceOf t -> SpaceOf t -> t -> t
translateByXY x y = translateBy $ makePoint x y

instance (Space s) => SimpleTransformable (Point2 s) where
    translateBy = (^+^)
    scaleBy     = flip (^*)
    stretchBy   = liftA2 (*)
instance (Space s) => Transformable (Point2 s) where
    rotateBy    = rotate

instance (SimpleTransformable a) => SimpleTransformable [a] where
    translateBy v = map (translateBy v)
    scaleBy     s = map (scaleBy s)
    stretchBy   p = map (stretchBy p)
instance (Transformable a) => Transformable [a] where
    rotateBy    a = map (rotateBy a)

instance {-# OVERLAPPABLE #-} (HasSpace t, Space (SpaceOf t), PointContainer t) => SimpleTransformable t where
    translateBy p = mapOverPoints (translateBy p)
    scaleBy     s = mapOverPoints (scaleBy s)
    stretchBy   p = mapOverPoints (stretchBy p)
instance {-# OVERLAPPABLE #-} (HasSpace t, Space (SpaceOf t), PointContainer t) => Transformable t where
    rotateBy    a = mapOverPoints (rotateBy a)
