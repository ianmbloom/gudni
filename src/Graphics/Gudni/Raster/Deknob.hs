module Graphics.Gudni.Raster.Deknob
  ( Triple (..)
  , pairsToTriples
  , checkKnob
  )
where

import Linear
import Graphics.Gudni.Figure

import Control.Lens

-- | A triple is just a convenient way to represent a complete quadratic curve section.
type Triple s = V3 (Point2 s)

pairsToTriples :: [CurvePair s] -> [Triple s]
pairsToTriples vs = go (view onCurve . head $ vs) vs
  where go first vs =
         case vs of
           (v0:v1:vs) -> V3 (v0 ^. onCurve) (v0 ^. offCurve) (v1 ^. onCurve):go first (v1:vs)
           (v0:[]) -> [V3 (v0 ^. onCurve) (v0 ^. offCurve) first]
           [] -> error "pairsToTriples encountered end of list"

-- * Remove Knobs
-- Knobs are segments of a curve where the curve point is outside of the horizontal range of the anchor points.
-- In other words the curve bulges out in the x direction.
-- It must be split into two curves that do not bulge out by finding the on curve point that is as close as possible
-- to the verticle tangent point.

-- | Constant for bifercating exactly in half.
sPLIT :: (Fractional s, Num s) => s
sPLIT = 1 / 2

-- | Find the point along the curve parameterized by t.
midPoint :: Num s => s -> Point2 s -> Point2 s -> Point2 s
midPoint t v0 v1 = (v0 ^* (1-t)) ^+^ (v1 ^* t)

-- | Given two onCurve points and a controlPoint. Find two control points and a midway on-curve point.
curvePoint :: Num s => s -> Point2 s -> Point2 s -> Point2 s -> Triple s
curvePoint t left control right =
  let leftMid  = midPoint t left    control
      rightMid = midPoint t control right
      onCurve  = midPoint t leftMid rightMid
  in  (V3 leftMid onCurve rightMid)

-- | Return true if a curve and its control point would be convex in the positive horizontal direction.
leftConvex  :: (Show s, Num s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Bool
leftConvex control onCurve  = control ^. pX < onCurve ^. pX

-- | Return true if a curve and its control point would be convex in the negative horizontal direction.
rightConvex :: (Show s, Num s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Bool
rightConvex control onCurve = onCurve ^. pX < control ^. pX

-- | Find an onCurve point and two new control points to can horizonally divide a curve that
-- is convex to the right (like a right bracket ")")
splitRightKnob ::(Show s, Fractional s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Point2 s -> Triple s
splitRightKnob = splitKnob rightConvex sPLIT -- start halfway

-- | Find an onCurve point and two new control points that horizonally divide a curve that
-- is convex to the left (like a left bracket "(")
splitLeftKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Point2 s -> Triple s
splitLeftKnob = splitKnob leftConvex sPLIT -- start halfway

-- | Find a new onCurve point and two new control points that divide the curve based on the convex function.
splitKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => (Point2 s -> Point2 s -> Bool) -> s -> Point2 s -> Point2 s -> Point2 s -> Triple s
splitKnob = splitKnob' 0.0 1.0

-- | Given a range of parameters along the curve determine if the are close enough to split the curve.
splitKnob' :: (Show s, Fractional s, Ord s, Num s, Iota s) => s -> s -> (Point2 s -> Point2 s -> Bool) -> s -> Point2 s -> Point2 s -> Point2 s -> Triple s
splitKnob' bottom top convex t v0 control v1
  -- So if the top and bottom parameters are close enough, return the points to divide the curve.
  | top - bottom <= iota = V3 leftMid onCurve rightMid
  -- Otherwise if the leftMid control point is still convex move the paramters to toward the top parameter and split again.
  | convex rightMid onCurve =
        splitKnob' t top convex (t + ((top - t) / 2)) v0 control v1 -- search closer to v0
  -- Otherwise if the rightMid control point is still convex move the paramters to toward the bottom parameter and split again.
  | convex leftMid  onCurve = -- if the rightMid control point is still convex.
        splitKnob' bottom t convex (bottom + ((t - bottom) / 2)) v0 control v1 -- search closer to v1
  -- Otherwise it's not convex anymore so split it
  | otherwise = V3 leftMid onCurve rightMid
  where (V3 leftMid onCurve rightMid) = curvePoint t v0 control v1

-- | If a curve is a knob, split it.
checkKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => Triple s -> [Triple s]
checkKnob (V3 a b c) =
    -- If both sides are convex in the left direction.
    if leftConvex b a && leftConvex b c
    then-- Split the knob based on it being left convex.
        let (V3 leftMid onCurve rightMid) = splitLeftKnob a b c
        -- And return the two resulting curves.
        in  [V3 a leftMid onCurve, V3 onCurve rightMid c]
    else-- Else if both sides are convex in the right direction
        if rightConvex b a && rightConvex b c
        then-- Split the know based on it being right convex.
            let (V3 leftMid  onCurve  rightMid) = splitRightKnob a b c
            -- And return the two resulting curves.
            in [V3 a leftMid onCurve, V3 onCurve rightMid c]
        else-- Otherwise return the curve unharmed.
            [V3 a b c]