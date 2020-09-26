{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Graphics.Gudni.Figure.Fit.Bezier
  ( projPoint
  , bezierPointAndNormal
  , projectBezierWithStepsAccuracy
  )
where

import Graphics.Gudni.Base

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Transform.Projection

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Lens
import Linear
import Linear.Affine
import Linear.V2
import Linear.V3
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad
import Data.Functor.Classes

projectBezierWithStepsAccuracy :: (Space s, Chain f) => Bool -> Int -> Maybe s -> BezierSpace s -> Bezier s -> f (Bezier s)
projectBezierWithStepsAccuracy debug max_steps m_accuracy bSpace bezier =
    let fixed = replaceKnob Vertical bezier
    in  join . fmap (traverseBezierSpace debug max_steps m_accuracy bSpace) $ fixed


instance (Space s, Reversible (Bezier s))=> CanFit (Bezier s) where
    isForward (Bez v0 _ v1) = v0 ^. pX <= v1 ^. pX
    projectTangent = projectTangentBezier
    fillGap leftResult rightResult =
        let leftEnd  = (lastLink leftResult  ) ^. bzEnd
            rightStart = (firstLink rightResult) ^. bzStart
            filler = if leftEnd /= rightStart
                     then pure (line leftEnd rightStart)
                     else empty
        in leftResult <|> filler <|> rightResult
    -- Given a target curve and a source curve, return a new target curve that approximates projecting every point in the target curve
    -- onto the source curve, such that the original x-axis corresponds to arclength along the source curve and the y-axis corresponds
    -- to distance from source curve along a normal.
    -- We can assume that the target curve has already been split and ordered so that the start x is less than or equal to the conrol x
    -- and the control x is less than or equal to the end x. In other words the curve is roughly horizontal.
    -- We can also assume that all the x coordinates will be within the range of 0 to the source curves arcLength after correction.
    projectDefaultCurve debugFlag max_steps m_accuracy start sourceCurve targetCurve =
        let -- Transform an x value into a t-parameter for the source curve
            -- that corresponds to a point x arc-distance along the curve.
            correctX x  = inverseArcLength max_steps m_accuracy sourceCurve (x - start)
            -- Transform the target curve so that each x value is a t-parameter.
            targetCurveCorrected = over bzPoints (fmap (over pX correctX)) $ targetCurve
            -- Define variables for all of the components of the transformed target curve.
            (V3 t0 tC t1) = fmap (view pX) . view bzPoints $ targetCurveCorrected
            (V3 y0 yC y1) = fmap (view pY) . view bzPoints $ targetCurveCorrected
            p0 = projPoint sourceCurve (targetCurveCorrected ^. bzStart)
            p1 = projPoint sourceCurve (targetCurveCorrected ^. bzEnd)
        in  if t0 == t1 -- the curve is vertical.
            then let -- Just project the start and end points
                     c = mid p0 p1
                 in  Bez p0 c p1
            else let split = 0.5 --(tC - t0) / (t1 - t0)
                     pM = projPoint sourceCurve (eval split targetCurveCorrected)
                     (oC, normalC) = bezierPointAndNormal sourceCurve tC
                     testControlPoint t y = let  (oC, normalC) = bezierPointAndNormal sourceCurve t
                                            in   oC .+^ (y *^ normalC)
                     projectedMid control = eval split (Bez p0 control p1)
                     projectAndTest t y = let control = Point2 t y -- testControlPoint t y
                                              projected = projectedMid control
                                          in  (control, projected, quadrance (projected .-. pM))
                     findControl left top right bottom count =
                        let (ltControl, ltProjected, ltDistance) = projectAndTest left  top
                            (rtControl, rtProjected, rtDistance) = projectAndTest right top
                            (lbControl, lbProjected, lbDistance) = projectAndTest left  bottom
                            (rbControl, rbProjected, rbDistance) = projectAndTest right bottom
                            midX = (left + right) / 2
                            midY = (top + bottom) / 2
                        in  if (abs (right - left) > 0.00001) && (count > 0)
                            then let (left', right') = if (ltDistance <= rtDistance)
                                                        then (left, midX)
                                                        else (midX, right)
                                     (top', bottom') = if (ltDistance <= lbDistance)
                                                       then (top, midY)
                                                       else (midY, bottom)
                                   in findControl left' top' right' bottom' (count-1)
                            else ltControl
                     finalControl = findControl (-10000) 20000 10000 (-20000) 16
                 in  Bez p0 finalControl p1

projectTangentPoint :: Space s => s -> Point2 s -> Diff Point2 s -> Point2 s -> Point2 s
projectTangentPoint offset v0 normal (Point2 x y) =
  let t = x - offset
      tangent = negate $ perp normal
  in  v0 .+^ (t *^ tangent) .+^ (y *^ normal)

projectTangentBezier :: Space s => s -> Point2 s -> Diff Point2 s -> Bezier s -> Bezier s
projectTangentBezier offset v0 normal bz = overBezier (projectTangentPoint offset v0 normal) bz

bezierPointAndNormal :: Space s => Bezier s -> s -> (Point2 s, Diff V2 s)
bezierPointAndNormal sourceCurve t =
  if t < 0.5
  then let (Bez s0 sC s1) = dropBezier t sourceCurve
           tangent = bezierStartTangent (Bez s0 sC s1)
           n0 = perp tangent
       in  (s0, n0)
  else let (Bez s0 sC s1) = takeBezier t sourceCurve
           tangent = bezierEndTangent (Bez s0 sC s1)
           n0 = perp tangent
       in  (s1, n0)

bezierStartTangent :: Space s => Bezier s -> Diff V2 s
bezierStartTangent (Bez s0 sC s1) = normalize (sC .-. s0)

bezierStartNormal :: Space s => Bezier s -> Diff V2 s
bezierStartNormal bz = perp (bezierStartTangent bz)

bezierEndTangent :: Space s => Bezier s -> Diff V2 s
bezierEndTangent (Bez s0 sC s1) = normalize (s1 .-. sC)

bezierEndNormal :: Space s => Bezier s -> Diff V2 s
bezierEndNormal bz = perp (bezierEndTangent bz)

relativeToNormalVector :: Space s => Diff V2 s -> Diff V2 s -> Diff V2 s
relativeToNormalVector source@(V2 sX sY) dest@(V2 dX dY) = (negate dX *^ perp source) ^+^ (dY *^ source)

slopeOf :: Space s => Diff V2 s -> s
slopeOf (V2 x y) = y / x

yInterceptSlope :: Space s => Point2 s -> s -> s -> s
yInterceptSlope v slope x = slope * (x - v^.pX) + v^.pY

xInterceptSlope :: Space s => Point2 s -> s -> s -> s
xInterceptSlope v slope y = ((y - v^.pY) / slope) + v^.pX

arbitraryIntersection :: Space s => Point2 s -> s -> Point2 s -> s -> Point2 s
arbitraryIntersection p0 slope0 p1 slope1 =
  let x = ( slope1 * (p1^.pX) - slope0 * (p0^.pX) - (p1^.pY) + (p0^.pY) ) / ( slope1 - slope0 )
      y = yInterceptSlope p0 slope0 x
  in  Point2 x y

projPoint :: forall s . Space s => Bezier s -> Point2 s -> Point2 s
projPoint curve toProject =
    let (point, normal) = bezierPointAndNormal curve (toProject ^. pX)
    in  point .+^ (((toProject ^. pY) *^ normal) :: Diff V2 s)
