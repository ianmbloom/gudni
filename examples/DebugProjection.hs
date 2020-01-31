{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ProjectDemo
-- Copyright   :  (c) Daniel Bergey 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Demonstration of equal-distance projectOnto, and equal spacing in t-paramater.

module DebugProjection
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Representation
import Graphics.Gudni.Util.Segment
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import qualified Graphics.Gudni.Figure.Bezier as B

import Control.Lens
import Control.Monad.State
import Linear
import Linear.Affine


import Data.Maybe
import Control.Lens

data ProjectionState = ProjectionState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   }
   deriving (Show)
makeLenses ''ProjectionState

consSecond :: (a, [b],[c]) -> [b] -> [c] -> (a, [b], [c])
consSecond (a, bs, cs) b c = (a, b ++ bs, c ++ cs)

projectCurveDemo :: Space s => Bool -> s -> Bezier s -> Bezier s -> ShapeTree Int s
projectCurveDemo debug = projectCurveDemoWithAccuracy debug 1e-3

projectCurveDemoWithAccuracy :: Space s => Bool -> s -> s -> Bezier s -> Bezier s -> ShapeTree Int s
projectCurveDemoWithAccuracy debug accuracy =
    projectCurveDemoWithStepsAccuracy debug (maxStepsFromAccuracy accuracy) (Just accuracy)

projectCurveDemoWithStepsAccuracy :: forall s
                                  .  Space s
                                  => Bool
                                  -> Int
                                  -> Maybe s
                                  -> s
                                  -> Bezier s
                                  -> Bezier s
                                  -> ShapeTree Int s
-- Given a target curve and a source curve, return a new target curve that approximates projecting every point in the target curve
-- onto the source curve, such that the original x-axis corresponds to arclength along the source curve and the y-axis corresponds
-- to distance from source curve along a normal.
-- We can assume that the target curve has already been split and ordered so that the start x is less than or equal to the conrol x
-- and the control x is less than or equal to the end x. In other words the curve is roughly horizontal.
-- We can also assume that all the x coordinates will be within the range of 0 to the source curves arcLength after correction.
projectCurveDemoWithStepsAccuracy debugFlag max_steps m_accuracy start sourceCurve targetCurve =
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
               in  colorWith green . maskOutline . stroke 0.004 $ Bez p0 c p1
          else let split = 0.5 --(tC - t0) / (t1 - t0)
                   pM = projPoint sourceCurve (eval split targetCurveCorrected)
                   (oC, normalC) = bezierPointAndNormal sourceCurve tC
                   testControlPoint t y = let  (oC, normalC) = bezierPointAndNormal sourceCurve t
                                          in   oC .+^ (y *^ normalC)
                   projectedMid control = eval split (Bez p0 control p1)
                   projectAndTest t y = let control = Point2 t y -- testControlPoint t y
                                            projected = projectedMid control
                                        in  (control, projected, quadrance (projected .-. pM))
                   findControl left top right bottom =
                      let (ltControl, ltProjected, ltDistance) = projectAndTest left  top
                          (rtControl, rtProjected, rtDistance) = projectAndTest right top
                          (lbControl, lbProjected, lbDistance) = projectAndTest left  bottom
                          (rbControl, rbProjected, rbDistance) = projectAndTest right bottom
                          midX = (left + right) / 2
                          midY = (top + bottom) / 2
                      in  if (abs (right - left) > 0.00001)
                          then let (left', right') = if (ltDistance <= rtDistance)
                                                      then (left, midX)
                                                      else (midX, right)
                                   (top', bottom') = if (ltDistance <= lbDistance)
                                                     then (top, midY)
                                                     else (midY, bottom)
                                 in consSecond (findControl left' top' right' bottom') [colorWith (transparent 0.2 red) . mask . stroke 0.01 . segmentsToOutline $
                                              [ Seg ltControl Nothing
                                              , Seg lbControl Nothing
                                              , Seg rbControl Nothing
                                              , Seg rtControl Nothing
                                              ]] []
                          else (ltControl, [colorWith (transparent 0.3 blue) . translateBy ltControl $ openCircle 0.015], [])
                   (finalControl, controlPoints, _) = findControl (-1000) 2000 1000 (-2000)
                   final = Bez p0 finalControl p1
                   --finalOnCurve = last curvePoints
               in  overlap [ colorWith purple  . translateBy pM $ overlap [openCircle 0.01, closedCircle 0.003]
                           , colorWith (dark green)  . translateBy finalControl $ overlap [openCircle 0.01, closedCircle 0.003]
                           --, colorWith (yellow  )  . translateBy finalOnCurve $ overlap [openCircle 0.01, closedCircle 0.003]
                           , overlap controlPoints
                           --, overlap . fmap (\p -> colorWith (red ) . translateBy p $ closedCircle 1.015) $ notLast curvePoints
                           , colorWith orange       . maskOutline . stroke 0.004 $ line oC (oC .+^ normalC)
                           , colorWith (light blue) . maskOutline . stroke 0.004 $ final
                           , colorWith black        . maskOutline . strokeOffset 0 0.01 $ sourceCurve
                           ]

instance HasToken ProjectionState where
  type TokenOf ProjectionState = Int

instance Model ProjectionState where
    screenSize state = Window (Point2 500 250)
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    constructScene state _status =
        do text <- (^?! unGlyph) <$> blurb 0.1 AlignMin "e" -- "Georg Guðni Hauksson"
           let angle   = state ^. stateBase . stateAngle
               repMode = state ^. stateBase . stateRepMode
               repDk   = state ^. stateBase . stateRepDk
               offset  = state ^. stateOffset
           return . Scene gray $
               --(if repMode then represent repDk else id) $
               (transformFromState (state ^. stateBase) $
               projectCurveDemo False offset bz myline :: ShapeTree Int SubSpace)
      where
        bz  = Bez (Point2 0 0) (Point2 0.5 1) (Point2 1 0) :: Bezier SubSpace
        myline = line (0 `by` 0) (0.5 `by` 0) :: Bezier SubSpace

instance HandlesInput token ProjectionState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input ^. inputType of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key ArrowRight -> stateOffset += 0.01
                          Key ArrowLeft  -> stateOffset -= 0.01
                          _ -> return ()
              _ -> return ()
          )

marker :: Color -> Point2 SubSpace -> ShapeTree Int SubSpace
marker color center = colorWith (transparent 0.5 color) $ translateBy center marker0

marker0 :: CompoundTree SubSpace
marker0 = {-rotateBy (1/8 @@ turn) $ translateBy (Point2 (s/2) (s/2)) $-} square
    where
        s = 8
        square :: CompoundTree SubSpace
        square = rectangle (Point2 s s)

main :: IO ()
main = runApplication $ ProjectionState
       (BasicSceneState
           { _stateScale       = 1500
           , _stateDelta       = Point2 100 100
           , _stateAngle       = 0 @@ deg
           , _statePaused      = True
           , _stateSpeed       = 1
           , _statePace        = 10
           , _stateLastTime    = 0
           , _stateDirection   = True
           , _statePlayhead    = 0
           , _stateFrameNumber = 0
           , _stateStep        = 69
           , _stateRepMode     = False
           , _stateRepDk       = False
           }
       ) 0
