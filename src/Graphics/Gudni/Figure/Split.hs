{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Split
  ( splitBezierX
  , splitBezierY
  )
where

import Linear

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import qualified Data.Vector as V

import Control.Lens
import Control.Loop
import Control.Applicative

-- * Remove Knobs
-- Knobs are segments of a curve where the curve point is outside of the horizontal range of the anchor points.
-- In other words the curve bulges out in the x direction.
-- It must be split into two curves that do not bulge out by finding the on curve point that is as close as possible
-- to the verticle tangent point.

-- | Constant for bifercating exactly in half.
sPLIT :: (Fractional s, Num s) => s
sPLIT = 1 / 2

-- | Given two onCurve points and a controlPoint. Find two control points and an on-curve point between them
-- by bifercating according to the parameter t.
curvePoint :: Num s => s -> Bezier s -> Bezier s
curvePoint t (Bez v0 control v1) =
  let mid0     = between t v0      control
      mid1     = between t control v1
      onCurve  = between t mid0    mid1
  in  (Bez mid0 onCurve mid1)


-- | Find a new onCurve point and two new control points that divide the curve based on the convex function.
findSplit :: forall f s . (Alternative f, Show s, Fractional s, Ord s, Num s, Iota s) => (Point2 s -> s) -> s -> Bezier s -> f (Bezier s)
findSplit axis cut bezier@(Bez v0 control v1) = search 0.0 1.0 0.5
  where
  -- | Given a range of parameters along the curve determine if the are close enough to split the curve.
  search :: s -> s -> s -> f (Bezier s)
  search lower upper t
    -- So if the upper and lower parameters are close enough, return the points to divide the curve.
    | upper - lower <= iota = final
    -- Otherwise if the mid point is further than the cut point search the upper half
    | axis onCurve > cut =
          search t upper upperSplit -- search closer to v0
    -- Otherwise if the mid point is further than the cut point search the upper half
    | axis onCurve < cut = -- if the mid1 control point is still convex.
          search lower t lowerSplit -- search closer to v1
    -- Otherwise it's not convex anymore so split it
    | otherwise = final
    where innerBezier@(Bez mid0 onCurve mid1) = curvePoint t bezier
          upperSplit = (t + ((upper - t) / 2))
          lowerSplit = (lower + ((t - lower) / 2))
          final = pure (Bez v0 mid0 onCurve) <|> pure (Bez onCurve mid1 v1)


-- | Split a curve across a vertical axis
splitBezierX :: (Alternative f, Space s) => s -> Bezier s -> f (Bezier s)
splitBezierX = findSplit (view _x)

splitBezierY :: (Alternative f, Space s) => s -> Bezier s -> f (Bezier s)
splitBezierY  = findSplit (view _y)
