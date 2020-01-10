{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Point
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines a point structure which has orthoganal components.

module Graphics.Gudni.Figure.Point
  ( Point2 (..)
  , pattern Point2
  , pattern P
  , zeroPoint
  , pX
  , pY
  , makePoint
  , (^+^)
  , (^-^)
  , (^*)
  , (^/)
  , lerp
  , negated
  , normalize
  , norm
  , pointArea
  , taxiDistance
  , mid
  , isLeftOf
  , isRightOf
  , isAbove
  , isBelow
)
where

import Graphics.Gudni.Figure.Space

import Data.Hashable

import Linear
import Linear.Affine

import System.Random

import Foreign.Storable
import Foreign.Ptr

import Control.Monad.Random
import Control.DeepSeq
import Control.Lens

type Point2 = Point V2
pattern Point2 x y = P (V2 x y)

instance (SimpleSpace s) => HasSpace (Point2 s) where
  type SpaceOf (Point2 s) = s

zeroPoint :: Num s => Point2 s
zeroPoint = Point2 0 0
-- | Lens for the x element of a point.
pX :: Lens' (Point2 s) (X s)
pX elt_fn (Point2 x y) = (\x' -> Point2 (unOrtho x') y) <$> (elt_fn . Ortho $ x)

-- | Lens for the y element of a point.
pY :: Lens' (Point2 s) (Y s)
pY elt_fn (Point2 x y) = (\y' -> Point2 x (unOrtho y')) <$> (elt_fn . Ortho $ y)

{-# INLINE makePoint #-}
-- | Make a point from two orthogonal dimensions.
makePoint :: X s -> Y s -> Point2 s
makePoint (Ortho x) (Ortho y) = P (V2 x y)

-- | Calculate the area of a box defined by the origin and this point.
pointArea :: Num s => Point2 s -> s
pointArea (Point2 x y) = x * y

instance Random s => Random (Point2 s) where
  random = runRand $ do x <- getRandom; y <- getRandom; return (Point2 x y)
  randomR (Point2 x0 y0, Point2 x1 y1) = runRand $ do x <- getRandomR (x0, x1)
                                                      y <- getRandomR (y0, y1)
                                                      return (Point2 x y)

-- Convenience functions for reasoning about points

taxiDistance :: (Space s) => Point2 s -> Point2 s -> s
taxiDistance v0 v1 =
  abs(unOrtho $ v1 ^. pX - v0 ^. pX) + abs(unOrtho $ v1 ^. pY - v0 ^. pY)


-- | Make a mid point from two points.
mid :: (Fractional s, Num s) => Point2 s -> Point2 s -> Point2 s
mid = lerp 0.5

-- | Return true if a is left of b in screen axis space.
isLeftOf :: (Space s) => Point2 s -> Point2 s -> Bool
a `isLeftOf` b  = a ^. pX < b ^. pX

-- | Return true if a is right of b in screen axis space.
isRightOf :: (Space s) => Point2 s -> Point2 s -> Bool
a `isRightOf` b = a ^. pX > b ^. pX

-- | Return true if a is above b in screen axis space.
isAbove :: (Space s) => Point2 s -> Point2 s -> Bool
a `isAbove` b  = a ^. pY < b ^. pY

-- | Return true if a is below b in screen axis space.
isBelow :: (Space s) => Point2 s -> Point2 s -> Bool
a `isBelow` b = a ^. pY > b ^. pY
