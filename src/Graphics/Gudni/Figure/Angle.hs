{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

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
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point

import Diagrams.Angle (Angle, cosA, sinA, tanA, (@@), deg, rad, turn, fullTurn, halfTurn, quarterTurn, normalizeAngle)
import Data.Hashable
import Control.DeepSeq
import Control.Lens

-- | Arbitarily rotate a point around the origin.
rotate :: (Floating s) => Angle s -> Point2 s -> Point2 s
rotate a (Point2 x y) = Point2 (x * cosA a - y * sinA a) (y * cosA a + x * sinA a)

-- | Fast 90 degree rotation around the origin.
rotate90 :: (Num s) => Point2 s -> Point2 s
rotate90 v = makePoint (orthoganal $ v ^. pY) (orthoganal $ negate $ v ^. pX)

-- | Fast 180 degree rotation around the origin.
rotate180 :: (Num s) => Point2 s -> Point2 s
rotate180 v = makePoint (negate $ v ^. pX) (negate $ v ^. pY )

-- | Fast 270 degree rotation around the origin.
rotate270 :: (Num s) => Point2 s -> Point2 s
rotate270 v = makePoint (orthoganal $ negate $ v ^. pY) (orthoganal $ v ^. pX)

instance Hashable s => Hashable (Angle s) where
  hashWithSalt s = hashWithSalt s . view rad

instance NFData s => NFData (Angle s) where
  rnf = rnf . view rad
