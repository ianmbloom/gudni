{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Angle
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Mainly a wrapper around Diagrams.Angle with some additional functions.

module Graphics.Gudni.Figure.Principle.Angle
  ( Angle
  , negateAngle
  , cosA
  , sinA
  , tanA
  , (@@)
  , deg
  , rad
  , turn
  , fullTurn
  , halfTurn
  , quarterTurn
  , normalizeAngle
  , rotate
  , rotate90
  , rotate180
  , rotate270
  , flipH
  , flipV
  , angleBetween
  )
where

import Graphics.Gudni.Figure.Principle.Axis
import Graphics.Gudni.Figure.Principle.Point

import Diagrams.Angle (Angle, cosA, sinA, tanA, (@@), deg, rad, turn, fullTurn, halfTurn, quarterTurn, normalizeAngle, angleBetween)
import Data.Hashable
import Control.Lens

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

instance Show s => Out (Angle s) where
    doc a = (text . show $ a)
    docPrec _ = doc

negateAngle :: Num s => Angle s -> Angle s
negateAngle = fmap negate
-- -----------------------------------------------------------------------------
-- Simple rotation functions.

-- | Arbitarily rotate a point around the origin.
rotate :: (Floating s) => Angle s -> Point2 s -> Point2 s
rotate a (Point2 x y) = Point2 (x * cosA a - y * sinA a) (y * cosA a + x * sinA a)

-- | Fast 90 degree rotation around the origin.
rotate90 :: (Num s) => Point2 s -> Point2 s
rotate90 v = makePoint (perpendicular $ v ^. pY) (perpendicular . negate $ v ^. pX)

-- | Fast 180 degree rotation around the origin.
rotate180 :: (Num s) => Point2 s -> Point2 s
rotate180 v = makePoint (negate $ v ^. pX) (negate $ v ^. pY )

-- | Fast 270 degree rotation around the origin.
rotate270 :: (Num s) => Point2 s -> Point2 s
rotate270 v = makePoint (perpendicular . negate $ v ^. pY) (perpendicular $ v ^. pX)

-- -----------------------------------------------------------------------------
-- Simple mirroring functions∘

flipH :: (Num s) => Point2 s -> Point2 s
flipH = over pX negate

flipV :: (Num s) => Point2 s -> Point2 s
flipV = over pY negate

-- -----------------------------------------------------------------------------
-- Instances

instance Hashable s => Hashable (Angle s) where
  hashWithSalt s = hashWithSalt s . view rad
