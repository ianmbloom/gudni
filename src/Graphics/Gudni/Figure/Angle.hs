{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

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

module Graphics.Gudni.Figure.Angle
  ( Angle
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

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point

import Diagrams.Angle (Angle, cosA, sinA, tanA, (@@), deg, rad, turn, fullTurn, halfTurn, quarterTurn, normalizeAngle, angleBetween)
import Data.Hashable
import Control.DeepSeq
import Control.Lens


-- -----------------------------------------------------------------------------
-- Simple rotation functions.

-- | Arbitarily rotate a point around the origin.
rotate :: (Floating s) => Angle s -> Point2 s -> Point2 s
rotate a (Point2 x y) = Point2 (x * cosA a - y * sinA a) (y * cosA a + x * sinA a)

-- | Fast 90 degree rotation around the origin.
rotate90 :: (Num s) => Point2 s -> Point2 s
rotate90 v = makePoint (v ^. pY) (negate $ v ^. pX)

-- | Fast 180 degree rotation around the origin.
rotate180 :: (Num s) => Point2 s -> Point2 s
rotate180 v = makePoint (negate $ v ^. pX) (negate $ v ^. pY )

-- | Fast 270 degree rotation around the origin.
rotate270 :: (Num s) => Point2 s -> Point2 s
rotate270 v = makePoint (negate $ v ^. pY) (v ^. pX)

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

instance NFData s => NFData (Angle s) where
  rnf = rnf . view rad
