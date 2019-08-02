-- | Functions on quadratic Bézier curves

module Graphics.Gudni.Figure.Bezier where

import Graphics.Gudni.Figure.Point

import Numeric.Interval
import Linear
import Linear.Affine

type Bezier s = V3 (Point2 s)

eval :: Num s => Bezier s -> s -> Point2 s
eval (V3 p0 p1 p2) t = let mt = 1 - t in
    p0 ^* (mt * mt) + (p1 ^* (mt * 2) + p2 ^* t) ^* t

-- | Arc length of a single quadratic Bézier segment.
-- From https://github.com/linebender/kurbo
-- This computation is based on an analytical formula. Since that formula suffers
-- from numerical instability when the curve is very close to a straight line, we
-- detect that case and fall back to Legendre-Gauss quadrature.
{-# SPECIALIZE arcLength :: Bezier Float -> Float #-}
{-# SPECIALIZE arcLength :: Bezier Double -> Double #-}
-- arcLength runtime increases ~50x without the SPECIALIZE
arcLength :: (Floating s, Ord s) => Bezier s -> s
arcLength (V3 p0 p1 p2) = let
    d2 = p0 - 2.0 * p1 + p2
    a = quadrance d2
    d1 = p1 - p0
    c = quadrance d1
    in if a < 5e-4 * c
    then
        -- This case happens for nearly straight Béziers.
        --
        -- Calculate arclength using Legendre-Gauss quadrature using formula from Behdad
        -- in https:--github.com/Pomax/BezierInfo-2/issues/77
        let
            v0 = norm
                (-0.492943519233745 *^ p0
                + 0.430331482911935 *^ p1
                + 0.0626120363218102 *^ p2)
            v1 = norm ((p2 - p0) ^* 0.4444444444444444)
            v2 = norm
                (-0.0626120363218102 *^ p0
                - 0.430331482911935 *^ p1
                + 0.492943519233745 *^ p2)
        in v0 + v1 + v2
    else
        let
            b = 2.0 * dot d2 d1

            sabc = sqrt (a + b + c)
            a2 = a ** (-0.5)
            a32 = a2 ^ 3
            c2 = 2.0 * sqrt c
            ba_c2 = b * a2 + c2

            v0 = 0.25 * a2 * a2 * b * (2.0 * sabc - c2) + sabc
        in if ba_c2 < 1e-13 -- TODO: justify and fine-tune this exact constant.
            then v0 -- This case happens for Béziers with a sharp kink.
            else
            v0 + 0.25
                * a32
                * (4.0 * c * a - b * b)
                * log (((2.0 * a + b) * a2 + 2.0 * sabc) / ba_c2)


{-# INLINE split #-}
split :: (Floating s, Ord s) => Bezier s -> s -> (Bezier s, Bezier s)
split (V3 p0 p1 p2) t = (V3 p0 c1 pm, V3 pm c2 p2) where
  c1 = lerp t p1 p0
  c2 = lerp t p2 p1
  pm = lerp t c2 c1

-- | @inverseArcLength ε bz l@ returns a parameter @t@ such that the
-- curve bz has length @l@ between its start point and @t@.  More
-- precisely, the length is @ l ± ε@, for the specified accuracy.
{-# SPECIALIZE inverseArcLength :: Float -> Bezier Float -> Float -> Float #-}
{-# SPECIALIZE inverseArcLength :: Double -> Bezier Double -> Double -> Double #-}
inverseArcLength :: (Floating s, RealFrac s, Ord s) => s -> Bezier s -> s -> s
inverseArcLength accuracy bz goal_length = inverseBezierArcLength' max_steps (0...1) 0 where
    inverseBezierArcLength' n range last_length =
        let
            mid_t = midpoint range
            mid_length = arcLength (fst (split bz mid_t))
        in if n == 0 || abs (mid_length - goal_length) < accuracy
        then -- finish up by linear interpolation
            if last_length < mid_length
            then inf range + (mid_t - inf range) * (goal_length - last_length) / (mid_length - last_length)
            else mid_t + (sup range - mid_t) * (goal_length - mid_length) / (last_length - mid_length)
        else
            let new_range = if mid_length > goal_length
                    then inf range ... mid_t
                    else mid_t ... sup range
            in inverseBezierArcLength' (n-1) new_range mid_length
    max_steps = ceiling (-1 * log accuracy / log 2)
