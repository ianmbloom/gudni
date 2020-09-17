{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
  ( PointContainer(..)
  , Point2 (..)
  , pattern Point2
  , pattern P
  , zeroPoint
  , pX
  , pY
  , makePoint
  , by
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
import Graphics.Gudni.Util.Chain

import Data.Hashable

import Linear
import Linear.Affine

import Foreign.Storable
import Foreign.Ptr

import Control.DeepSeq
import Control.Lens
import Data.Kind
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

type Point2 = Point V2
pattern Point2 x y = P (V2 x y)
instance (Out s) => Out (V2 s)
instance (Out s) => Out (Point2 s)
   where
    doc (Point2 x y) = parens $ doc x <+> comma <+> doc y
    docPrec _ = doc

instance Space s => HasSpace (Point2 s) where
  type SpaceOf (Point2 s) = s

class (HasSpace t) => PointContainer t where
   mapOverPoints   :: (Point2 (SpaceOf t) -> Point2 (SpaceOf t)) -> t -> t

instance Space s => PointContainer (Point2 s) where
   mapOverPoints = ($)

zeroPoint :: Num s => Point2 s
zeroPoint = Point2 0 0
-- | Lens for the x element of a point.
pX :: Lens' (Point2 s) s
pX elt_fn (Point2 x y) = (\x' -> Point2 x' y) <$> (elt_fn x)

-- | Lens for the y element of a point.
pY :: Lens' (Point2 s) s
pY elt_fn (Point2 x y) = (\y' -> Point2 x y') <$> (elt_fn y)

-- | Make a point from two orthogonal dimensions.
{-# INLINE makePoint #-}
makePoint :: s -> s -> Point2 s
makePoint x y = P (V2 x y)

by :: s -> s -> Point2 s
by = makePoint

-- | Calculate the area of a box defined by the origin and this point.
pointArea :: Num s => Point2 s -> s
pointArea (Point2 x y) = x * y

-- Convenience functions for reasoning about points

taxiDistance :: (Space s) => Point2 s -> Point2 s -> s
taxiDistance v0 v1 =
  abs(v1 ^. pX - v0 ^. pX) + abs(v1 ^. pY - v0 ^. pY)

-- | Make a mid point from two points.
mid :: (Fractional s, Num s) => Point2 s -> Point2 s -> Point2 s
mid a b = (a + b) / 2

-- | Return true if a is left of b in screen axis space.
isLeftOf :: (Space s) => Point2 s -> Point2 s -> Bool
a `isLeftOf` b  = a ^. pX - b ^. pX < 0

-- | Return true if a is right of b in screen axis space.
isRightOf :: (Space s) => Point2 s -> Point2 s -> Bool
a `isRightOf` b = a ^. pX - b ^. pX > 0

-- | Return true if a is above b in screen axis space.
isAbove :: (Space s) => Point2 s -> Point2 s -> Bool
a `isAbove` b  = a ^. pY - b ^. pY < 0

-- | Return true if a is below b in screen axis space.
isBelow :: (Space s) => Point2 s -> Point2 s -> Bool
a `isBelow` b = a ^. pY - b ^. pY > 0
