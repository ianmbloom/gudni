{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Square
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- The most basic possible example application using the Gudni library.

module Plot
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw

import Control.Lens
import Control.Monad.State
import Data.Maybe

data PlotState = PlotState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   }
   deriving (Show)
makeLenses ''PlotState

instance HasStyle PlotState where
    type StyleOf PlotState = DefaultStyle

instance Model PlotState where
    screenSize state = Window (Point2 1024 768)
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    constructScene state status =
        sceneFromLayout (light . greenish $ blue) (plots state)
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

instance HandlesInput token PlotState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case (input ^. inputType) of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key ArrowRight -> stateOffset += 10.01
                          Key ArrowLeft  -> stateOffset -= 10.01
                          _ -> return ()

              _ -> return ()
          )

main :: IO ()
main = runApplication $ PlotState
       (BasicSceneState
           { _stateScale       = 1
           , _stateDelta       = Point2 0 0
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
       ) 0.75

-- | All the turtle plots from the plot module.
plots :: PlotState -> Layout (StyleOf PlotState)
plots state = translateByXY 100 300 .
              scaleBy 30 .
              overlap .
              gridOf 10 16 1 .
              catMaybes .
              map (fmap (withColor yellow . mask . stroke 0.5) . (curveLibrary :: String -> Maybe (OpenCurve SubSpace))) $
              turtleNames
