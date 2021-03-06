{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Constants
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for removing "knobs" which are vertical curves that cannot be easily
-- rasterized.

module Graphics.Gudni.Raster.Deknob
  ( Bezier (..)
  , replaceKnobs
  )
where

import Linear
import Graphics.Gudni.Figure
import qualified Data.Vector as V

import Control.Lens
import Control.Loop

-- * Remove Knobs
-- Knobs are segments of a curve where the curve point is outside of the horizontal range of the anchor points.
-- In other words the curve bulges out in the x direction.
-- It must be split into two curves that do not bulge out by finding the on curve point that is as close as possible
-- to the verticle tangent point.

-- | Constant for bifercating exactly in half.
sPLIT :: (Fractional s, Num s) => s
sPLIT = 1 / 2

-- | Find the point along the line from v0 v1 with the distance proportional by t.
between :: Num s => s -> Point2 s -> Point2 s -> Point2 s
between t v0 v1 = (v0 ^* (1-t)) ^+^ (v1 ^* t)

-- | Given two onCurve points and a controlPoint. Find two control points and an on-curve point between them
-- by bifercating according to the parameter t.
curvePoint :: Num s => s -> Point2 s -> Point2 s -> Point2 s -> Bezier s
curvePoint t v0 control v1 =
  let mid0     = between t v0      control
      mid1     = between t control v1
      onCurve  = between t mid0    mid1
  in  (Bez mid0 onCurve mid1)

-- | Return true if a is left of b in screen space.
isLeftOf  :: (Show s, Num s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Bool
a `isLeftOf` b  = a ^. pX < b ^. pX

-- | Return true if a is right of b in screen space.
isRightOf :: (Show s, Num s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Bool
a `isRightOf` b = a ^. pX > b ^. pX

-- | Find a new onCurve point and two new control points that divide the curve based on the convex function.
findSplit :: forall s . (Show s, Fractional s, Ord s, Num s, Iota s) => (Point2 s -> Point2 s -> Bool) -> s -> Bezier s -> Bezier s
findSplit staysRelativeTo t (Bez v0 control v1) = search 0.0 1.0 t
  where
  -- | Given a range of parameters along the curve determine if the are close enough to split the curve.
  search :: s -> s -> s -> Bezier s
  search bottom top t
    -- So if the top and bottom parameters are close enough, return the points to divide the curve.
    | top - bottom <= iota = Bez mid0 onCurve mid1
    -- Otherwise if the mid0 control point is still convex move the paramters to toward the top parameter and split again.
    | mid1 `staysRelativeTo` onCurve =
          search t top topSplit -- search closer to v0
    -- Otherwise if the mid1 control point is still convex move the paramters to toward the bottom parameter and split again.
    | mid0 `staysRelativeTo` onCurve = -- if the mid1 control point is still convex.
          search bottom t bottomSplit -- search closer to v1
    -- Otherwise it's not convex anymore so split it
    | otherwise = Bez mid0 onCurve mid1
    where (Bez mid0 onCurve mid1) = curvePoint t v0 control v1
          topSplit    = (t + ((top - t) / 2))
          bottomSplit = (bottom + ((t - bottom) / 2))

vector2 :: a -> a -> V.Vector a
vector2 a b = V.singleton a `V.snoc` b

-- | If a curve is a knob, split it.
fixKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => Bezier s -> V.Vector (Bezier s)
fixKnob (Bez v0 control v1) =
    -- If both sides are convex in the left direction.
    if control `isLeftOf` v0 && control `isLeftOf` v1
    then-- This is a left bulging knob. Split it while one part maintains that bulge.
        let (Bez mid0 onCurve mid1) = findSplit isLeftOf sPLIT (Bez v0 control v1)
        -- And return the two resulting curves.
        in vector2 (Bez v0 mid0 onCurve) (Bez onCurve mid1 v1)
    else-- Else if both sides are convex in the right direction
        if control `isRightOf` v0 && control `isRightOf` v1
        then-- This is a right bulging knob. Split it while one part maintains that bulge direction.
            let (Bez mid0  onCurve  mid1) = findSplit isRightOf sPLIT  (Bez v0 control v1)
            -- And return the two resulting curves.
            in vector2 (Bez v0 mid0 onCurve) (Bez onCurve mid1 v1)
        else -- Otherwise return the curve unharmed.
             V.singleton (Bez v0 control v1)


replaceKnobs :: (Show s, Fractional s, Ord s, Num s, Iota s)
             => V.Vector (Bezier s) -> V.Vector (Bezier s)
replaceKnobs triples =
  let len = V.length triples

  in  V.concatMap fixKnob triples
