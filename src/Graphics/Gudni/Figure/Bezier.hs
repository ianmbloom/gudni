{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}

-- | Functions on quadratic Bézier curves
module Graphics.Gudni.Figure.Bezier
  ( Bezier(..)
  , BezierContainer(..)
  , line
  , curved
  , bzStart
  , bzControl
  , bzEnd
  , bzPoints
  , unfoldBezier
  , pattern Bez
  , overBezier
  , reverseBezier
  , maxStepsFromAccuracy
  , dropBezier
  , takeBezier
  , sliceBezier
  , splitBezier
  , insideBezier
  , inverseArcLength
  , eval
  , fixBezierNeighbor
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Util.Chain

import Data.Kind
import Numeric.Interval
import Linear
import Linear.Affine
import Linear
import qualified Data.Vector as V
import Data.Hashable
import Control.DeepSeq
import Control.Lens hiding ((...))
import Control.Applicative
import Text.PrettyPrint.GenericPretty

data Bezier s = Bezier {unBezier :: V3 (Point2 s)} deriving (Eq, Ord, Generic)
instance (Out s) => Out (V3 s)
instance (Out s) => Out (Bezier s)

pattern Bez x y z = Bezier (V3 x y z)

instance Show s => Show (Bezier s) where
  show (Bez v0 c v1) = "Bez (" ++ show v0 ++ ") (" ++ show c ++ ") (" ++ show v1 ++ ")"

-- | Lens for the start point of a bezier.
bzStart :: Lens' (Bezier s) (Point2 s)
bzStart elt_fn (Bez v0 c v1) = (\v0' -> Bez v0' c v1) <$> (elt_fn v0)

-- | Lens for the control point of a bezier.
bzControl :: Lens' (Bezier s) (Point2 s)
bzControl elt_fn (Bez v0 c v1) = (\c' -> Bez v0 c' v1) <$> (elt_fn c)

-- | Lens for the control point of a bezier.
bzEnd :: Lens' (Bezier s) (Point2 s)
bzEnd elt_fn (Bez v0 c v1) = (\v1' -> Bez v0 c v1') <$> (elt_fn v1)

bzPoints :: Lens' (Bezier s) (V3 (Point2 s))
bzPoints elt_fn (Bezier v3) = (\v3' -> Bezier v3') <$> (elt_fn v3)

unfoldV3 :: Alternative f => V3 a -> f a
unfoldV3 (V3 a b c) = pure a <|> pure b <|> pure c

unfoldBezier :: Alternative f => Bezier s -> f (Point2 s)
unfoldBezier = unfoldV3 . view bzPoints

overBezier :: (Point2 s -> Point2 z) -> Bezier s -> Bezier z
overBezier f (Bezier v3) =  Bezier (fmap f v3)

-- | Reverse the direction of a curve section.
reverseBezier :: Bezier s -> Bezier s
reverseBezier (Bez a b c) = Bez c b a

line :: Space s => Point2 s -> Point2 s -> Bezier s
line v0 v1 = Bez v0 (mid v0 v1) v1

curved :: Space s => Point2 s -> Point2 s -> Point2 s -> Bezier s
curved v0 c v1 = Bez v0 c v1

instance Space s => HasSpace (Bezier s) where
    type SpaceOf (Bezier s) = s

eval :: Num s => s -> Bezier s -> Point2 s
eval t (Bez p0 p1 p2) = let mt = 1 - t in
    p0 ^* (mt * mt) + (p1 ^* (mt * 2) + p2 ^* t) ^* t


class BezierContainer t where
  type BezFunctor t :: Type -> Type
  joinOverBeziers :: ((Bezier (SpaceOf t)) -> (BezFunctor t) (Bezier (SpaceOf t)))
                  -> t -> t

-- | Arc length of a single quadratic Bézier segment.
-- From https://github.com/linebender/kurbo
-- This computation is based on an analytical formula. Since that formula suffers
-- from numerical instability when the curve is very close to a line line, we
-- detect that case and fall back to Legendre-Gauss quadrature.
-- arcLength runtime increases ~50x without the SPECIALIZE
--{-# SPECIALIZE arcLength :: Bezier SubSpace -> SubSpace #-}
instance Space s => HasArcLength (Bezier s) where
    arcLength (Bez p0 p1 p2) = let
        d2 = p0 - 2.0 * p1 + p2
        a  = quadrance d2
        d1 = p1 - p0
        c  = quadrance d1
        in
        if a < 5e-4 * c
        then
            -- This case happens for nearly line Béziers.
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

{-# INLINE dropBezier #-}
-- | Analogous to dropping the beginning of a list but drop the part of a bezier up
-- to parameter t.
dropBezier :: Space s => s -> Bezier s -> Bezier s
dropBezier t (Bez v0 c v1) =
  let (Bez _ onCurve mid1) = insideBezier t (Bez v0 c v1)
  in  (Bez onCurve mid1 v1)

{-# INLINE takeBezier #-}
-- | Analogous to taking the beginning of a list but take the part of a bezier up
-- to parameter t.
takeBezier :: Space s => s -> Bezier s -> Bezier s
takeBezier t (Bez v0 c v1) =
 let (Bez mid0 onCurve _) = insideBezier t (Bez v0 c v1)
  in (Bez v0 mid0 onCurve)

{-# INLINE sliceBezier #-}
-- | Get a slice of the bezier curve between the two parameters t0 and t1
sliceBezier :: Space s => s -> s -> Bezier s -> Bezier s
sliceBezier t0 t1 = takeBezier ((t1 - t0) / (1 - t0)) . dropBezier t0

{-# INLINE splitBezier #-}
-- | Split a bezier at parameter t
splitBezier :: (Floating s, Ord s) => s -> Bezier s -> (Bezier s, Bezier s)
splitBezier t (Bez p0 c p1) = (Bez p0 mid0 onCurve, Bez onCurve mid1 p1) where
    (Bez mid0 onCurve mid1 ) = insideBezier t (Bez p0 c p1)

-- | Given two onCurve points and a controlPoint. Find two control points and an on-curve point between them
-- by bifercating according to the parameter t.
{-# INLINE insideBezier #-}
insideBezier :: Num s => s -> Bezier s -> Bezier s
insideBezier t (Bez v0 control v1) =
  let mid0     = lerp t control v0
      mid1     = lerp t v1      control
      onCurve  = lerp t mid1    mid0
  in  (Bez mid0 onCurve mid1)


-- | @inverseArcLength ε bz l@ returns a parameter @t@ such that the
-- curve bz has length @l@ between its start point and @t@.  More
-- precisely, the length is @ l ± ε@, for the specified accuracy.
-- {-# SPECIALIZE inverseArcLength :: Int -> Maybe Float -> Bezier Float -> Float -> Float #-}
-- {-# SPECIALIZE inverseArcLength :: Int -> Maybe Double -> Bezier Double -> Double -> Double #-}
inverseArcLength :: (Space s) => Int -> Maybe s -> Bezier s -> s -> s
inverseArcLength max_steps m_accuracy bz goal_length =
    go max_steps (0...1) 0
    where
    go n range last_length =
        let
            mid_t = midpoint range
            mid_length = arcLength (fst (splitBezier mid_t bz))
        in if n == 0 || closeEnough mid_length
        then -- finish up by linear interpolation
            if last_length < mid_length
            then inf range + (mid_t - inf range) * (goal_length - last_length) / (mid_length - last_length)
            else mid_t + (sup range - mid_t) * (goal_length - mid_length) / (last_length - mid_length)
        else
            let new_range = if mid_length > goal_length
                    then inf range ... mid_t
                    else mid_t ... sup range
            in go (n-1) new_range mid_length
    closeEnough l = case m_accuracy of
        Nothing -> False
        Just accuracy -> abs (l - goal_length) < accuracy

maxStepsFromAccuracy :: (Floating s, RealFrac s) => s -> Int
maxStepsFromAccuracy accuracy = ceiling (-1 * log accuracy / log 2)

instance (Space s) => CanBox (Bezier s) where
  boxOf (Bez v0 c v1) = minMaxBox (minMaxBox (boxOf v0) (boxOf c)) (boxOf v1)

instance Hashable s => Hashable (Bezier s) where
    hashWithSalt s (Bezier v3) = s `hashWithSalt` v3

instance NFData s => NFData (Bezier s) where
    rnf (Bezier v3) = v3 `deepseq` ()

fixBezierNeighbor :: Bezier s -> Bezier s -> Bezier s
fixBezierNeighbor bz0 bz1 = set bzEnd (view bzStart bz1) bz0
