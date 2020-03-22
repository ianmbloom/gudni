{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Split
  ( findSplit
  , splitBezierX
  , splitBezierY
  , splitClosestControl
  )
where

import Linear

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Util.Debug
import qualified Data.Vector as V

import Control.Lens
import Control.Applicative

-- * Remove Knobs
-- Knobs are segments of a curve where the curve point is outside of the horizontal range of the anchor points.
-- In other words the curve bulges out in the x direction.
-- It must be split into two curves that do not bulge out by finding the on curve point that is as close as possible
-- to the verticle tangent point.

-- | Constant for bifercating exactly in half.
sPLIT :: (Fractional s, Num s) => s
sPLIT = 1 / 2

-- | Find a new onCurve point and two new control points that divide the curve based on the convex function.
findSplit :: forall s . (Show s, Fractional s, Ord s, Num s, Iota s) => (s -> s) -> s
findSplit f = search 0.0 1.0 sPLIT
  where
  -- | Given a range of parameters along the curve determine if the are close enough to split the curve.
  search :: s -> s -> s -> s
  search lower upper t
    -- So if the upper and lower parameters are close enough, return the points to divide the curve.
    | upper - lower <= iota = t
    -- Otherwise if the mid point is further than the cut point search the upper half
    | f t > 0 =
          search t upper upperSplit -- search closer to v0
    -- Otherwise if the mid point is further than the cut point search the upper half
    | f t < 0 = -- if the mid1 control point is still convex.
          search lower t lowerSplit -- search closer to v1
    -- Otherwise it's not convex anymore so split it
    | otherwise = t
    where upperSplit = (t + ((upper - t) / 2))
          lowerSplit = (lower + ((t - lower) / 2))



-- | Split a curve across a vertical axis
splitBezierX :: (Show s, Space s) => s -> Bezier s -> (Bezier s, Bezier s)
splitBezierX cut bezier =
  let splitT =
          if (bezier ^. bzStart . pX <= bezier ^. bzEnd . pX)
          then findSplit (\t -> cut - (view (bzControl . pX) . insideBezier t $ bezier))
          else findSplit (\t -> negate (cut - (view (bzControl . pX) . insideBezier t $ bezier)))
      (left,right) = splitBezier splitT bezier
      -- correct the split to be exactly the x of the cutpoint
 in   (set (bzEnd . pX) cut left, set (bzStart . pX) cut right)

splitBezierY :: (Space s) => s -> Bezier s -> (Bezier s, Bezier s)
splitBezierY cut bezier =
  let splitT =
          if (bezier ^. bzStart . pY <= bezier ^. bzEnd . pY)
          then findSplit (\t -> cut - (view (bzControl . pY) . insideBezier t $ bezier))
          else findSplit (\t -> negate (cut - (view (bzControl . pY) . insideBezier t $ bezier)))
      (left,right) = splitBezier splitT bezier
      -- correct the split to be exactly the y of the cutpoint
 in   (set (bzEnd . pY) cut left, set (bzStart . pY) cut right)

-- | Split a bezier at the point on the curve that is closest to the control point using taxicab distance
splitClosestControl :: (Space s) => Bezier s -> (Bezier s, Bezier s)
splitClosestControl bezier =
   let c = bezier ^. bzControl
       compareDistance (Bez mid0 _ mid1) = taxiDistance mid0 c - taxiDistance mid1 c
       splitT = findSplit (\t -> compareDistance . insideBezier t $ bezier)
   in  splitBezier splitT bezier
