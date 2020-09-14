{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Figure.Split
  ( hALF
  , findSplit
  , splitClosestControl
  , maybeKnobSplitPoint
  , isKnob
  )
where

import Linear

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Util.Debug
import qualified Data.Vector as V
import Data.Maybe

import Control.Lens
import Control.Applicative

-- * Remove Knobs
-- Knobs are segments of a curve where the curve point is outside of the horizontal range of the anchor points.
-- In other words the curve bulges out in the x direction.
-- It must be split into two curves that do not bulge out by finding the on curve point that is as close as possible
-- to the verticle tangent point.

-- | Constant for bifercating exactly in half.
hALF :: (Fractional s, Num s) => s
hALF = 1 / 2

-- | Find a new onCurve point and two new control points that divide the curve based on the convex function.
findSplit :: forall s . (Show s, Fractional s, Ord s, Num s, Iota s) => (s -> s) -> s
findSplit f = search 0.0 1.0 hALF
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

-- | Split a bezier at the point on the curve that is closest to the control point using taxicab distance
splitClosestControl :: (Space s) => Bezier s -> (Bezier s, Bezier s)
splitClosestControl bezier =
   let c = bezier ^. bzControl
       compareDistance (Bez mid0 _ mid1) = taxiDistance mid0 c - taxiDistance mid1 c
       splitT = findSplit (\t -> compareDistance . insideBezier t $ bezier)
   in  splitBezier splitT bezier

-- | Given two onCurve points and a controlPoint. Find two control points and an on-curve point between them
-- by bifercating according to the parameter t.
curvePoint :: Num s => s -> Bezier s -> Bezier s
curvePoint t (Bez v0 control v1) =
  let mid0     = lerp (1-t) v0      control
      mid1     = lerp (1-t) control v1
      onCurve  = lerp (1-t) mid0    mid1
  in  (Bez mid0 onCurve mid1)

isKnob :: (Space s) => Lens' (Point2 s ) s -> Bezier s -> Bool
isKnob axis bz@(Bez v0 control v1) =
  let a = abs (v0 ^. axis - control ^. axis)
      b = abs (control ^. axis  - v1 ^. axis)
      c = abs (v0 ^. axis - v1 ^. axis)
  in
    abs ((a + b) - c) > iota

splitDirection :: Space s => Lens' (Point2 s) s -> Bezier s -> s -> s
splitDirection axis bz@(Bez v0 control v1) t =
    let (Bez mid0 onCurve mid1) = curvePoint t bz
    in  (abs (onCurve ^. axis - v0 ^. axis) - (abs (onCurve ^. axis - mid0 ^. axis) + abs (mid0 ^. axis - v0 ^. axis))) -
        (abs (onCurve ^. axis - v1 ^. axis) - (abs (onCurve ^. axis - mid1 ^. axis) + abs (mid1 ^. axis - v1 ^. axis)))

findKnobSplit :: Space s => Lens' (Point2 s) s -> Bezier s -> s
findKnobSplit axis bz = findSplit (splitDirection axis bz)

maybeKnobSplitPoint :: (Space s) => Lens' (Point2 s) s -> Bezier s -> Maybe s
maybeKnobSplitPoint axis bz =
   if isKnob axis bz
   then Just $ findKnobSplit axis bz
   else Nothing

splitChain :: (Space s, Alternative f) => Lens' (Point2 s) s -> Bezier s -> s -> f (Bezier s)
splitChain axis bz t =
    let (left, right) = splitBezier t bz
    -- And return the two resulting curves.
    in  pure left <|> pure right

instance Space s => CanDeknob (Bezier s) where
    -- | If a curve is a knob, split it.
    deKnob axis bz@(Bez v0 control v1) =
      fmap (splitChain axis bz) ({-tr ("maybeDeKnob " ++ show bz) $-} maybeKnobSplitPoint axis bz)
