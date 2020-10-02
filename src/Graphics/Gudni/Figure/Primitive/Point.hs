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
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Rank2Types            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Primitive.Point
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Defines a point structure which has orthoganal components.

module Graphics.Gudni.Figure.Primitive.Point
  ( PointContainer(..)
  , Point2 (..)
  , Axis(..)
  , athwart
  , pointAlongAxis
  , pattern Point2
  , pattern P
  , pX
  , pY
  , zeroPoint
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

import Graphics.Gudni.Figure.Primitive.Space
import Graphics.Gudni.Figure.Primitive.Axis
import Graphics.Gudni.Base.Chain

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

class (IsAxis axis, Axis (PerpendicularTo axis)) => Axis axis where
  along :: axis -> Lens' (Point2 s) (Along axis s)

athwart :: (Axis axis)
        => axis
        -> Lens' (Point2 s) (Athwart axis s)
athwart = along . perpendicularTo

pointAlongAxis :: (Axis axis, Space s) => axis -> Along axis s -> Athwart axis s -> Point2 s
pointAlongAxis axis parentLine parentCut =
    set (along   axis) parentLine .
    set (athwart axis) parentCut  $
    zeroPoint

-- | Lens for the x element of a point.
pX :: Lens' (Point2 s) (Ax Horizontal s)
pX elt_fn (Point2 x y) = (\(Ax x') -> Point2 x' y) <$> (elt_fn (Ax x))

-- | Lens for the y element of a point.
pY :: Lens' (Point2 s) (Ax Vertical s)
pY elt_fn (Point2 x y) = (\(Ax y') -> Point2 x y') <$> (elt_fn (Ax y))

instance Axis Horizontal where
  along Horizontal = pX

instance Axis Vertical where
  along Vertical = pY

zeroPoint :: Num s => Point2 s
zeroPoint = Point2 0 0

-- | Make a point from two orthogonal dimensions.
{-# INLINE makePoint #-}
makePoint :: Ax Horizontal s
          -> Ax Vertical   s
          -> Point2 s
makePoint x y = P (V2 (fromAlong Horizontal x) (fromAlong Vertical y))

by :: Ax Horizontal s
   -> Ax Vertical s
   -> Point2 s
by = makePoint

-- | Calculate the area of a box defined by the origin and this point.
pointArea :: Num s => Point2 s -> s
pointArea (Point2 x y) = x * y

-- Convenience functions for reasoning about points

taxiDistance :: (Space s) => Point2 s -> Point2 s -> s
taxiDistance v0 v1 =
    abs(fromAlong Horizontal (v1 ^. pX) - fromAlong Horizontal (v0 ^. pX))
  + abs(fromAlong Vertical   (v1 ^. pY) - fromAlong Vertical   (v0 ^. pY))

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
