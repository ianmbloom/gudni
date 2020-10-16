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

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier.Math
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

instance (Space s) => GoesForward (Bezier s) where
    isForward (Bez v0 _ v1) = v0 ^. pX <= v1 ^. pX

instance (Space s) => CanFillGap (Bezier s) where
    fillGap leftResult rightResult =
        let leftEnd  = (lastLink leftResult  ) ^. bzEnd
            rightStart = (firstLink rightResult) ^. bzStart
            filler = if leftEnd /= rightStart
                     then pure (line leftEnd rightStart)
                     else empty
        in leftResult <|> filler <|> rightResult

instance (Space s, Reversible (Bezier s))=> CanFit (Bezier s) (Bezier s) where
    projectTangent = projectTangentBezier

    -- Given a target curve and a source curve, return a new target curve that approximates projecting every point in the target curve
    -- onto the source curve, such that the original x-axis corresponds to arclength along the source curve and the y-axis corresponds
    -- to distance from source curve along a normal.
    -- We can assume that the target curve has already been split and ordered so that the start x is less than or equal to the conrol x
    -- and the control x is less than or equal to the end x. In other words the curve is roughly horizontal.
    -- We can also assume that all the x coordinates will be within the range of 0 to the source curves arcLength after correction.
    projectDefaultCurve debugFlag max_steps m_accuracy start sourceCurve targetCurve =
        let -- Transform an x value into a t-parameter for the source curve
            -- that corresponds to a point x arc-distance along the curve.
            correctX x  = toAlong Horizontal $ inverseArcLength max_steps m_accuracy sourceCurve (fromAlong Horizontal $ x - start)
            -- Transform the target curve so that each x value is a t-parameter.
            targetCurveCorrected = over bzPoints (fmap (over pX correctX)) $ targetCurve
            -- Define variables for all of the components of the transformed target curve.
            (V3 t0 tC t1) = fmap (fromAlong Horizontal . view pX) . view bzPoints $ targetCurveCorrected
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
                        in  if (abs (right - left) > 0.0000001) && (count > 0)
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
