{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfineTreeTest
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Application
import Graphics.Gudni.Draw
import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Util.Debug

import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Serialize
import Graphics.Gudni.Raster.ConfineTree.Build

import qualified Data.Map as M
import Control.Lens
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.State
import Text.PrettyPrint.GenericPretty
import System.Info
import Data.List.Lens

import Control.Monad.Random
import System.Random

import GudniTests
import BasicShapes
import DrawSweepTrace

data ConfineTreeState = ConfineTreeState
  { _stateBase        :: BasicSceneState
  , _stateShapeAngle  :: Angle SubSpace
  , _stateTraceStep   :: Int
  , _stateCurrentTest :: Int
  } deriving (Show)
makeLenses ''ConfineTreeState

initialModel =
    ConfineTreeState
    { _stateBase = BasicSceneState
        { _stateScale       = 1
        , _stateDelta       = Point2 0 0
        , _stateAngle       = 0 @@ deg
        , _statePaused      = False
        , _stateSpeed       = 0.1
        , _statePace        = 50
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 0
        , _stateFrameNumber = 0
        , _stateStep        = 21
        , _stateRepMode     = False
        , _stateRepDk       = False
        , _stateCursor      = Point2 0 0
        }
    --, _stateTree        = tree
    , _stateShapeAngle = 0 @@ rad -- 0 @@ rad
    , _stateTraceStep = 0
    , _stateCurrentTest = {-4 -} findTest {-"fuzzyGlyphs" "millionFuzzyCircles"-}{- "randomCurves" -} "diamondBox" allTests
    }

allTests = testList ++ basicShapes
numTests :: Int
numTests = length allTests

getTest :: ConfineTreeState -> (String, SubSpace -> Int -> Layout DefaultStyle)
getTest state = allTests !! (state ^. stateCurrentTest)

instance HasStyle ConfineTreeState where
  type StyleOf ConfineTreeState = DefaultStyle

instance Model ConfineTreeState where
    screenSize state = --FullScreen
                       Window $ Point2 1024 512
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState frame elapsedTime inputs state =
        over stateBase (updateSceneState frame elapsedTime) $ foldl (flip processInput) state inputs
    ioTask = return
    constructScene state status =
        do  let testShape = {-scaleBy 100 $-} (snd $ getTest state) (view deg $ state ^. stateShapeAngle) (round (state ^. stateBase . statePlayhead)) --(state ^. stateBase . stateStep)
                name = (fst $ getTest state)
                decorationLimit = 0 --12
            scene <- sceneFromLayout (light gray) testShape
            confineState <- liftIO $ withConfinedScene Nothing M.empty scene $ \ pictDataPile serialState -> return serialState
            liftIO $ putStrLn "confineState"
            (tree, decoTree, sweepTrace) <- liftIO $ buildConfineTree (state ^. stateTraceStep) decorationLimit (confineState ^. conBezierPile)
            let colorMap        = confineState ^. conColorMap
                constructedTree :: Layout DefaultStyle
                constructedTree = constructConfineTree colorMap tree
                constructedDecoTree= constructDecorateTree colorMap decoTree
                makePixel point = Box point (point + Point2 500 500)
                setPoints :: [Layout DefaultStyle]
                setPoints = map (checkPoint colorMap tree decoTree) [{-Point2 120.40964 730,Point2 130 849.66302-} Point2 300 500]
                randomPoints :: [Layout DefaultStyle]
                randomPoints    = map (checkPoint colorMap tree decoTree) $
                                  evalRand (take 400 <$> getRandomRs (Point2 0 0, Point2 4000 4000)) .
                                  mkStdGen $ round $
                                  state ^. stateBase . statePlayhead
                --traceConstructed :: Layout DefaultStyle
                --traceConstructed = constructSweepTrace sweepTrace
                testScene = overlap [  overlap randomPoints
                                       ,
                                       -- overlap setPoints
                                       -- ,
                                       constructedDecoTree
                                       ,
                                       constructedTree
                                       ,
                                      --traceConstructed
                                      --,
                                      testShape
                                    ]
                statusTree = statusDisplay (state ^. stateBase) "Test ConfineTree" (lines status)
                treeScene  = transformFromState (state ^. stateBase) testScene
                withStatus = if False then overlap [statusTree, treeScene] else treeScene
            sceneFromLayout gray withStatus
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

instance HandlesInput token ConfineTreeState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input ^. inputType of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key LetterN -> stateShapeAngle %= normalizeAngle . (^+^ (1 @@ deg))
                          Key LetterM -> stateShapeAngle %= normalizeAngle . (^-^ (1 @@ deg))
                          Key LetterZ -> whenM (uses stateTraceStep (> 0     )) $ stateTraceStep -= 1
                          Key LetterX -> whenM (uses stateTraceStep (< 100000)) $ stateTraceStep += 1
                          Key ArrowRight -> whenM (uses stateCurrentTest (< (numTests - 1))) $ stateCurrentTest += 1
                          Key ArrowLeft  -> whenM (uses stateCurrentTest (> 0)) $ stateCurrentTest -= 1
                          _ -> return ()
              _ -> return ()
              )

main :: IO ()
main = do putStrLn "Started"
          runApplication initialModel
