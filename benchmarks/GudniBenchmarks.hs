{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GudniBenchmarks
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Fuzzy


import Data.Word
import Data.List(isInfixOf)
import Data.Maybe(listToMaybe, fromMaybe)

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse

import Codec.Picture

import GudniTests

import System.IO.Silently

import System.Info
import Data.Maybe

getTest :: BenchmarkState -> (String, BenchmarkState -> FontMonad IO (ShapeTree Int SubSpace))
getTest state = (state ^. stateTests) !! (state ^. stateCurrentTest)

instance Model BenchmarkState where
    screenSize state = --FullScreen
                       Window $ Point2 128 64
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState frame elapsedTime inputs state =
        over stateBase (updateSceneState frame elapsedTime) $ foldl (flip processInput) state inputs
    ioTask = return
    constructScene state status =
        do  testScene <- (snd $ getTest state) state
            let testName = (fst $ getTest state)
            statusTree <- (^?! unGlyph) <$> statusDisplay (state ^. stateBase) testName (lines status)
            let tree = transformFromState (state ^. stateBase) testScene
                withStatus = if False then overlap [statusTree, tree] else tree
            return . Scene (light gray) $ withStatus
    providePictureMap state = return $ state ^. statePictureMap
    handleOutput state target = do  presentTarget target
                                    return state

instance HandlesInput BenchmarkState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input of
              (InputKey Pressed _ inputKeyboard) ->
                  do  tests <- use stateTests
                      case inputKeyboard of
                          Key ArrowRight -> whenM (uses stateCurrentTest (< (length tests - 1))) $ stateCurrentTest += 1
                          Key ArrowLeft  -> whenM (uses stateCurrentTest (> 0)) $ stateCurrentTest -= 1
                          _ -> return ()
              _ -> return ()
          )

main :: IO ()
main = --silence $
       do putStrLn "Started"
          jpeg <- readJpeg "image/hero-yellow-flowers.jpg"
          case jpeg of
            Left message -> putStrLn message
            Right pict   -> do let picts = makePictureMap [("flowers",pict)]
                               runApplication (initialModel picts :: BenchmarkState)
