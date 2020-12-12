{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}

module ConfineTreeTest
  ( main
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Application
import Graphics.Gudni.Draw
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Image

import Graphics.Gudni.Util.Debug

import Graphics.Gudni.Raster.Dag.Fabric.Out
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Build
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Storage
import Graphics.Gudni.Raster.Dag.FromLayout
import Graphics.Gudni.Raster.Dag.Serialize
import Graphics.Gudni.Raster.Dag.Fabric.Traverse
import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.TextureReference

import qualified Data.Map as M
import Control.Lens
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.State
import Text.PrettyPrint.GenericPretty
import System.Info
import Data.List.Lens
import Foreign.Storable

import Control.Monad.Random
import System.Random

import GudniTests
import BasicShapes


data ConfineTreeState = ConfineTreeState
  { _stateBase        :: BasicSceneState
  , _stateShapeAngle  :: Angle SubSpace
  , _stateTraceStep   :: Int
  , _stateCurrentTest :: Int
  , _stateDecorationType :: Bool
  , _stateDecorationLimit :: Int
  } deriving (Generic, Show)
makeLenses ''ConfineTreeState

instance Out ConfineTreeState

initialModel :: ConfineTreeState
initialModel =
    ConfineTreeState
    { _stateBase =
          BasicSceneState
          { _stateScale       = 1
          , _stateDelta       = Point2 0 0
          , _stateAngle       = 0 @@ deg
          , _statePaused      = True
          , _stateSpeed       = 0.1
          , _statePace        = 50
          , _stateLastTime    = 0
          , _stateDirection   = True
          , _statePlayhead    = 10
          , _stateFrameNumber = 0
          , _stateStep        = 53
          , _stateRepMode     = False
          , _stateRepDk       = False
          , _stateCursor      = Point2 512 58
          }
    , _stateShapeAngle = 0 @@ deg -- @@ rad -- 0 @@ rad
    , _stateTraceStep = 0
    , _stateCurrentTest = flip findTest allTests  "randomCurves" -- "onTriangles"  -- "onTriangles" -- "benchmark1" -- "twoTriangles" -- "bigTriangle" -- "randomCurves" -- "diamondBox" "fuzzyGlyphs" "millionFuzzyCircles"
    , _stateDecorationType = True
    , _stateDecorationLimit = 0
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
                       Window $ Point2 256 256
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState frame elapsedTime inputs state =
        over stateBase (updateSceneState frame elapsedTime) $ foldl (flip processInput) state inputs
    ioTask = return
    constructScene state status =
        do  let testShape = (snd $ getTest state) (view deg $ state ^. stateShapeAngle) (round (state ^. stateBase . statePlayhead))
                name = (fst $ getTest state)
                frameNumber = state ^. stateBase . stateFrameNumber
                canvasSize :: Point2 PixelSpace
                canvasSize = Point2 64 64
                canvas :: Box SubSpace
                canvas = sizeToBox . fmap fromIntegral $ canvasSize
                point  = state ^. stateBase . stateCursor
                limit = realToFrac cROSSsPLITlIMIT
            liftIO $ putStrLn name
            pictureMap <- liftIO $ providePictureMap state
            (pictureMemoryMap, pixelPile) <- liftIO $ collectPictureMemory pictureMap
            fabric <- sceneToFabric pictureMemoryMap (Scene gray testShape)
            -- liftIO . putStrLn . render . doc $ fabric
            liftIO $ putStrLn "Before withSerialized Fabric"
            testScene <- withSerializedFabric limit 0 (Just canvas) pixelPile fabric $ \storage ->
                evalStateT (evalRandT (
                     do  out <- lift outFabric
                         liftIO $ putStrLn "**** outFabric *******************************************"
                         liftIO $ putStrLn $ render out
                         -- (tree, decoTree, sweepTrace) <- liftIO $ buildConfineTree (state ^. stateDecorationType )
                         --                                                           (state ^. stateTraceStep      )
                         --                                                           (state ^. stateDecorationLimit)
                         --                                                           (confineState ^. conPrimStorage . primBezierPile)
                         -- constructedTree     <- constructConfineTree tree
                         -- constructedDecoTree <- constructDecorateTree decoTree
                         -- boxQuery     <- checkBox tree decoTree (state ^. stateBase . stateCursor)
                         -- (setPoints :: [Layout DefaultStyle]) <-
                         --      mapM constructRayColor [point] -- , Point2 200 200]
                         -- (randomPoints :: [Layout DefaultStyle]) <-
                         --      mapM constructRayColor .
                         --      evalRand (take 100 <$> getRandomRs (Point2 0 0, fromIntegral <$> canvasSize)) .
                         --      mkStdGen $ round $
                         --      state ^. stateBase . statePlayhead
                         -- traceConstructed :: Layout DefaultStyle
                         -- traceConstructed <- constructSweepTrace sweepTrace

                         -- let traversePixel loc = traverseFabric limit (locationToSubSpace loc) (Point2 10 10)
                         -- img <- createImageM traversePixel canvasSize
                         -- let imageLayout :: Layout DefaultStyle
                         --     imageLayout = withTexture (NewTexture "pictureFrozen" (PictureFrozen img)) $ mask $ rectangle $ fmap fromIntegral canvasSize

                         -- liftIO $ putStrLn "about to constructDag"
                         -- (dag :: Layout DefaultStyle) <- lift $ constructDag root
                         -- let curs :: Layout DefaultStyle
                         --     curs = translateBy point . scaleBy 5 . withColor black $ mask circle
                         return $
                             overlap [  --curs
                                        -- ,
                                        -- imageLayout
                                        -- ,
                                        -- withColor black . mask . rectangle $ fmap (+3) $ fmap fromIntegral canvasSize
                                        -- ,
                                        -- overlap randomPoints
                                        -- ,
                                        -- overlap setPoints
                                        -- ,
                                        -- boxQuery
                                        -- ,
                                        -- constructedDecoTree
                                        -- ,
                                        -- constructedTree
                                        -- ,
                                        -- traceConstructed
                                        -- ,
                                        -- dag
                                        -- ,
                                        testShape
                                     ]
                ) (mkStdGen frameNumber)) storage
            liftIO $ putStrLn $ "sizeOf Haskell DecoTag    " ++ show (sizeOf (undefined :: DecoTag    SubSpace))
            liftIO $ putStrLn $ "sizeOf Haskell ConfineTag " ++ show (sizeOf (undefined :: ConfineTag SubSpace))
            liftIO $ putStrLn "After withSerialized Fabric"
            let statusTree = statusDisplay (state ^. stateBase) "Test ConfineTree" (lines status)
                treeScene  = transformFromState (state ^. stateBase) testShape
                withStatus = if False then overlap [statusTree, treeScene] else treeScene
            return $ withBackgroundColor gray withStatus
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state
    dumpState state inputs =
        do putStrLn $ pretty state
           when (not . null $ inputs) $ putStrLn $  pretty inputs

instance HandlesInput token ConfineTreeState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input ^. inputType of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key LetterU -> stateDecorationType %= not
                          Key LetterI -> whenM (uses stateDecorationLimit (> 0  )) $ stateDecorationLimit -= 1
                          Key LetterO -> whenM (uses stateDecorationLimit (< 100)) $ stateDecorationLimit += 1
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
