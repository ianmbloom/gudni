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

debugScene :: ShapeTree Int SubSpace
debugScene =
  let bz = Bez (P (V2 0.86950 0.11950)) (P (V2 0.89814 0.14814)) (P (V2 0.92678 0.17678))
      sourceCurve = Bez (P (V2 0.62500 0.46875)) (P (V2 0.81250 0.37500)) (P (V2 1.00000 0.00000))
      bzCorrected =  Bez (P (V2 0.00000 0.11950)) (P (V2 0.06689 0.14814)) (P (V2 0.13097 0.17678))
      (start, normal0) = (P (V2 0.62500 0.46875),V2 0.44722 0.89443)
      s0 = P (V2 0.67844 0.57563)
      (end,   normal1) = (P (V2 0.67411 0.43937),V2 0.57151 0.82060)
      s1 = P (V2 0.77514 0.58443)
      tangent0 = V2 0.91929 0.39358
      tangent0Rotated = V2 0.99825 (-0.05910)
      tangent1 = V2 0.91296 0.40806
      tangent1Rotated = V2 0.98238 (-0.18691)
      slope0 = (-0.05920)
      slope1 = (-0.19026)
      result = Bez (P (V2 0.67844 0.57563)) (P (V2 0.88596 0.56335)) (P (V2 0.77514 0.58443))
  in
  overlap [ withColor green  . mask $ oldLine 0.001 start s0
          , withColor green  . mask $ oldLine 0.001 end   s1
          , withColor orange . mask $ oldLine 0.004 start (start .+^ normal0)
          , withColor orange . mask $ oldLine 0.004 end   (end   .+^ normal1)
          --, withColor red    . mask $ oldLine 0.003 (start) (curveSlice ^. bzControl)
          --, withColor red    . mask $ oldLine 0.003 (control) (curveSlice ^. bzEnd)
          , withColor (light purple)   . mask $ oldLine 0.005 (sourceCurve ^. bzStart)   (sourceCurve ^. bzControl)
          , withColor (light purple)   . mask $ oldLine 0.005 (sourceCurve ^. bzControl) (sourceCurve ^. bzEnd)
          , withColor (light red)     . mask $ oldLine 0.005  (result ^. bzStart)   (result ^. bzControl)
          , withColor (light red)     . mask $ oldLine 0.005  (result ^. bzControl) (result ^. bzEnd)

          , withColor (light green)    . mask $ oldLine 0.01  (bz ^. bzStart)   (bz ^. bzControl)
          , withColor (light green)    . mask $ oldLine 0.01  (bz ^. bzControl) (bz ^. bzEnd)

          , withColor (light blue)     . mask $ oldLine 0.01  (bzCorrected ^. bzStart)   (bzCorrected ^. bzControl)
          , withColor (light blue)     . mask $ oldLine 0.01  (bzCorrected ^. bzControl) (bzCorrected ^. bzEnd)

          , withColor (light yellow)   . mask $ oldLine 0.005 (bzCorrected ^. bzStart) (bzCorrected ^. bzStart .+^ tangent0)
          , withColor (light yellow)   . mask $ oldLine 0.005 (bzCorrected ^. bzEnd  ) (bzCorrected ^. bzEnd   .+^ tangent1)
          , withColor (light yellow)   . mask $ oldLine 0.005 s0 (s0 .+^ tangent0Rotated)
          , withColor (light yellow)   . mask $ oldLine 0.005 s1 (s1 .+^ tangent1Rotated)
          ]

instance HasStyle ProjectionState where
  type StyleOf ProjectionState = DefaultStyle

instance Model ProjectionState where
    screenSize state = Window (Point2 500 250)
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    constructScene state _status =
        do let angle   = state ^. stateBase . stateAngle
               repMode = state ^. stateBase . stateRepMode
               repDk   = state ^. stateBase . stateRepDk
               offset  = state ^. stateOffset
           sceneFromLayout gray . place $
               --(if repMode then represent repDk else id) $
               (transformFromState (state ^. stateBase) $
               debugScene :: ShapeTree Int SubSpace)
      where
        bz  = Bez (Point2 0 0) (Point2 0.5 1) (Point2 1 0)
        --bz2 = Bez (Point2 20 40) (Point2 0 80) (Point2 40 80)
        --bz3 = Bez (Point2 40 80) (Point2 80 80) (Point2 80 160)
        smallBz = Bez (Point2 0 0) (Point2 100 100) (Point2 10 100)
        path = makeOpenCurve [bz{-,bz2,bz3-}]
        doubleDotted :: Space s => OpenCurve s -> ShapeTree Int s
        doubleDotted path =
           let thickness = 2
               betweenGap = 1
               dotLength = 8
               dotGap = 2
               numDots = floor (arcLength path / (dotLength + dotGap))
           in  withColor (light . greenish $ blue) .
               projectOnto path .
               translateByXY 0 (negate ((thickness * 2 + betweenGap) / 2)) .
               overlap .
               horizontallySpacedBy translateBy zeroPoint (dotLength + dotGap) .
               replicate numDots .
               overlap .
               verticallySpacedBy translateBy zeroPoint (thickness + betweenGap) .
               replicate 2 .
               mask .
               rectangle $
               dotLength `by` thickness

    providePictureMap _ = noPictures
    handleOutput state target = do
        presentTarget target
        return state

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
