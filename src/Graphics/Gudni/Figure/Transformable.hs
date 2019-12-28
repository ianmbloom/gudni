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

class SimpleTransformable t => Transformable t where
  rotateBy    :: Angle (SpaceOf t) -> t -> t

translateByXY :: (HasSpace t, SimpleTransformable t) => X (SpaceOf t) -> Y (SpaceOf t) -> t -> t
translateByXY x y = translateBy $ makePoint x y

instance (SimpleSpace s) => SimpleTransformable (Point2 s) where
    translateBy = (^+^)
    scaleBy     = flip (^*)
    stretchBy   = liftA2 (*)

instance (Space s) => Transformable (Point2 s) where
    rotateBy    = rotate

instance (SimpleTransformable a) => SimpleTransformable [a] where
    translateBy v = map (translateBy v)
    scaleBy s = map (scaleBy s)
    stretchBy s = map (stretchBy s)

instance (Transformable a) => Transformable [a] where
    rotateBy a = map (rotateBy a)

instance {-# Overlappable #-} (HasSpace a, HasSpace (f a), SpaceOf a ~ SpaceOf (f a), Functor f, SimpleTransformable a) => SimpleTransformable (f a) where
    translateBy v = fmap (translateBy v)
    scaleBy s = fmap (scaleBy s)
    stretchBy p = fmap (stretchBy p)

instance {-# Overlappable #-} (HasSpace a, HasSpace (f a), SpaceOf a ~ SpaceOf (f a), Functor f, Transformable a) => Transformable (f a) where
    rotateBy a = fmap (rotateBy a)
