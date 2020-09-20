{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

module StrokeDemo
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw
import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad.State
import Control.Applicative
import Linear

import Data.Maybe
import Control.Lens

data StrokeState = StrokeState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   }
   deriving (Show)
makeLenses ''StrokeState

instance HasStyle StrokeState where
    type StyleOf StrokeState = DefaultStyle

instance Model StrokeState where
    screenSize state = Window (Point2 500 250)
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    constructScene state _status =
        sceneFromLayout gray .
        place .
        transformFromState (state ^. stateBase) .
        overlap $
        [projected, stroked]
      where
        bz  = Bez (Point2 0 0) (Point2 40 0) (Point2 20 40)
        bz2 = Bez (Point2 20 40) (Point2 0 80) (Point2 40 80)
        bz3 = Bez (Point2 40 80) (Point2 40 90) (Point2 40 100)
        path = makeOpenCurve (pure bz <|> pure bz2 <|> pure bz3)
        stroked :: ShapeTree Int SubSpace
        stroked =
            withColor blue .
            mask .
            stroke 2 $ path
        projected :: ShapeTree Int SubSpace
        projected =
            projectOnto path .
            translateByXY 0 (-2.5) .
            translateByXY (state ^. stateOffset) 0 .
            withColor (transparent 0.8 red) .
            overlap .
            horizontallySpacedBy translateBy zeroPoint 12 .
            replicate 100 .
            mask .
            rectangle $
            10 `by` 5

instance HandlesInput token StrokeState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input ^. inputType of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key ArrowRight -> stateOffset += 2
                          Key ArrowLeft  -> stateOffset -= 2
                          _ -> return ()
              _ -> return ()
          )

main :: IO ()
main = runApplication $ StrokeState
       (BasicSceneState
           { _stateScale       = 5
           , _stateDelta       = Point2 100 100
           , _stateAngle       = 0 @@ deg
           , _statePaused      = True
           , _stateSpeed       = 0.1
           , _statePace        = 0.5
           , _stateLastTime    = 0
           , _stateDirection   = True
           , _statePlayhead    = 0
           , _stateFrameNumber = 0
           , _stateStep        = 69
           , _stateRepMode     = False
           , _stateRepDk       = False
           , _stateCursor      = Point2 0 0
           }
       ) 0
