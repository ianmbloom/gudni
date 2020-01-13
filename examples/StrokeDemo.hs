{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Debug

import qualified Graphics.Gudni.Figure.Bezier as B

import Control.Lens
import Control.Monad.State
import Linear

import Data.Maybe
import Control.Lens

data StrokeState = StrokeState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   }
   deriving (Show)
makeLenses ''StrokeState

instance Model StrokeState where
    screenSize state = Window (Point2 500 250)
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    ioTask = return
    constructScene state _status = return . Scene gray $
        transformFromState (state ^. stateBase) $
        overlap ([projected, stroked])
      where
        bz  = Bez (Point2 0 0) (Point2 40 0) (Point2 20 40)
        bz2 = Bez (Point2 20 40) (Point2 0 80) (Point2 40 80)
        bz3 = Bez (Point2 40 80) (Point2 40 90) (Point2 40 100)
        path = makeOpenCurve $ [bz,bz2,bz3]
        stroked :: ShapeTree Int SubSpace
        stroked =
            colorWith blue .
            mask .
            stroke 2 $ path
        projected :: ShapeTree Int SubSpace
        projected =
            projectOnto path .
            translateByXY 0 (-2.5) .
            translateByXY (state ^. stateOffset) 0 .
            colorWith (transparent 0.8 red) .
            overlap .
            horizontallySpacedBy 12 .
            replicate 100 .
            rectangle $
            10 `by` 5


    providePictureMap _ = noPictures
    handleOutput state target = do
        presentTarget target
        return state

instance HandlesInput StrokeState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key ArrowRight -> stateOffset += 2
                          Key ArrowLeft  -> stateOffset -= 2
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
main = runApplication $ StrokeState
       (BasicSceneState
           { _stateScale       = 5
           , _stateDelta       = Point2 100 100
           , _stateAngle       = 0 @@ deg -- 0.02094 @@ rad -- 0 @@ turn-- quarterTurn
           , _statePaused      = True
           , _stateSpeed       = 0.1
           , _statePace        = 0.5
           , _stateLastTime    = 0
           , _stateDirection   = True
           , _statePlayhead    = 0
           , _stateFrameNumber = 0
           , _stateStep        = 69
           }
       ) 0
