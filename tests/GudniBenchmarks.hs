{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GudniBenchmarks
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Application

import Graphics.Gudni.Util.Debug

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

data BenchmarkState = BenchmarkState
  { _stateBase        :: BasicSceneState
  , _statePictureMap  :: PictureMap
  , _stateTests       :: [(String, SubSpace -> Int -> Layout DefaultStyle )]
  , _stateCurrentTest :: Int
  }
makeLenses ''BenchmarkState

instance Show BenchmarkState where
  show state =
     "BenchmarkState { " ++
     show (state ^. stateBase       ) ++ ", " ++
     show (state ^. stateCurrentTest) ++  " }"

initialModel pictureMap =
    BenchmarkState
    { _stateBase = BasicSceneState
        { _stateScale       = 10
        , _stateDelta       = Point2 20 20
        , _stateAngle       = 0 @@ rad
        , _statePaused      = True
        , _stateSpeed       = 0.1
        , _statePace        = 1
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 0
        , _stateFrameNumber = 0
        , _stateStep        = 100
        , _stateRepMode     = False
        , _stateRepDk       = False
        , _stateCursor      = Point2 0 0
        }
    , _statePictureMap  = pictureMap
    , _stateTests       = testList
    , _stateCurrentTest = 0
    }

getTest :: BenchmarkState -> (String, SubSpace -> Int -> Layout DefaultStyle)
getTest state = (state ^. stateTests) !! (state ^. stateCurrentTest)

instance HasStyle BenchmarkState where
  type StyleOf BenchmarkState = DefaultStyle

instance Model BenchmarkState where
    screenSize state = --FullScreen
                       Window $ Point2 512 512
    -- shouldLoop _ = False
    updateModelState frame elapsedTime inputs state =
        over stateBase (updateSceneState frame elapsedTime) $ foldl (flip processInput) state inputs
    constructScene state status =
        do let testScene = (snd $ getTest state) (state ^. stateBase . statePlayhead) (state ^. stateBase . stateStep)
               testName = (fst $ getTest state)
               repMode = state ^. stateBase . stateRepMode
               repDk   = state ^. stateBase . stateRepDk
               statusTree = statusDisplay (state ^. stateBase) testName (lines status)
           flatScene <- sceneFromLayout gray $ transformFromState (state ^. stateBase) testScene
           let tree = if repMode
                      then place . represent repDk . fromJust $ flatScene ^. sceneShapeTree
                      else transformFromState (state ^. stateBase) $ testScene
               withStatus = if False then overlap [statusTree, tree] else tree
           sceneFromLayout (light gray) $ withStatus
    providePictureMap state = return $ state ^. statePictureMap

instance HandlesInput Int BenchmarkState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input ^. inputType of
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
