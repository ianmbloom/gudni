{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConfineTreeDemo
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Segment
import Graphics.Gudni.Util.Fuzzy
import Graphics.Gudni.Util.Representation
import Graphics.Gudni.Experimental.TreeOrderTable
import Graphics.Gudni.Experimental.ConfineTree
import Graphics.Gudni.Experimental.ConfineTreeTest
import Graphics.Gudni.Experimental.ConstructConfineTree
import Graphics.Gudni.Experimental.ConstructConfineQuery
import Graphics.Gudni.Experimental.SerializeConfineTree

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

data ConfineTreeState = ConfineTreeState
  { _stateBase        :: BasicSceneState
  , _stateShapeAngle  :: Angle SubSpace
  , _stateTraceStep   :: Int
  } deriving (Show)
makeLenses ''ConfineTreeState

initialModel =
    ConfineTreeState
    { _stateBase = BasicSceneState
        { _stateScale       = 1
        , _stateDelta       = Point2 600 600
        , _stateAngle       = 0 @@ deg -- 0.02094 @@ rad -- 0 @@ turn-- quarterTurn
        , _statePaused      = True
        , _stateSpeed       = 0.1
        , _statePace        = 50
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 164
        , _stateFrameNumber = 0
        , _stateStep        = 45
        , _stateRepMode     = False
        , _stateRepDk       = False
        }
    --, _stateTree        = tree
    , _stateShapeAngle = 1.07334 @@ rad -- 1.15187 @@ rad --0.91625 @@ rad -- 5.44538 @@ rad -- 4.20620 @@ rad -- 3.98806 @@ rad -- 30 @@ deg
    , _stateTraceStep = 0
    }

instance HasStyle ConfineTreeState where
  type StyleOf ConfineTreeState = DefaultStyle

instance Model ConfineTreeState where
    screenSize state = --FullScreen
                       Window $ Point2 512 512
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    ioTask = return
    constructScene state status =
        do  let range = makePoint 2 2
                randomCurves = evalRand (sequence . replicate 4 $ fuzzyCurve range 20) (mkStdGen $ (state ^. stateBase . stateStep)) :: [ShapeTree Int SubSpace]
                maxSize   = 64
                table     = buildTreeOrderTable maxSize
                shapes    = scaleBy 400 $
                            overlap [ -- place .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- translateByXY (-1) (-1)
                                      -- $
                                      -- overlap randomCurves
                                      -- ,
                                      -- withColor (transparent 0.25 blue) .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- mask $
                                      -- circle
                                      -- ,
                                      -- withColor (transparent 0.25 red) .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- translateByXY 0.25 0.25 $
                                      -- mask
                                      -- $
                                      -- circle .
                                      -- ,
                                      -- scaleBy 2 .
                                      -- withColor (transparent 1.0 (dark $ dark gray)) .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- fromSegments $
                                      -- [ straightXY 0 0
                                      -- , straightXY 1 1
                                      -- , straightXY 1 0
                                      -- , straightXY 0 1
                                      -- ]
                                      -- ,
                                      -- withColor green .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- translateByXY 0.5 0.5 .
                                      -- mask
                                      -- $
                                      -- circle
                                      -- ,
                                      -- withColor (transparent 0.1 blue) .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- translateByXY (-0.5) (-0.5) .
                                      -- mask
                                      -- $
                                      -- --triangle
                                      -- --rectangle (Point2 1 1)
                                      -- sixPointRectangle
                                      --,
                                      withColor (transparent 0.25 red) .
                                      translateByXY (0) (-1) .
                                      rotateBy (state ^. stateShapeAngle) .
                                      scaleBy 1.5 .
                                      mask
                                      $
                                      triangle
                                      -- ,
                                      -- withColor (transparent 0.25 $ light green) .
                                      -- translateByXY (0.5) (-0.5) .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- scaleBy 1.5 .
                                      -- mask
                                      -- $
                                      -- triangle
                                    ]
            scene <- sceneFromLayout (light gray) shapes
            confineState <- liftIO $ withConfinedScene Nothing M.empty scene $ \ pictDataPile serialState -> return serialState
            let tree            = trWith pretty "confineTree" $ crossConfineTree $ confineState ^. conConfineTree
                colorMap        = confineState ^. conColorMap
                constructed     = constructConfineTree colorMap tree
                makePixel point = Box point (point + Point2 500 500)
                randomPoints :: [Layout DefaultStyle]
                randomPoints    = map (checkPoint colorMap tree . applyScale 500) $
                                  evalRand (take 50 <$> getRandomRs (Point2 (-1) (-1), Point2 1 1)) .
                                  mkStdGen $ round $
                                  state ^. stateBase . statePlayhead
                testScene = overlap [--overlap randomPoints
                                    --, overlap setPoints
                                    --trace
                                    --, withColor purple . translateBy (Point2 490.40347 (-446.67499)) . scaleBy 15 $ circle
                                    --,
                                    constructed
                                    , shapes
                                    ]
                statusTree = statusDisplay (state ^. stateBase) "Test ConfineTree" (lines status)
                treeScene  = transformFromState (state ^. stateBase) runCrossTests --runLessCurveTests) --testScene
                withStatus = if False then overlap [statusTree, treeScene] else treeScene
            sceneFromLayout gray withStatus
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

sixPointRectangle :: Space s => Shape s
sixPointRectangle =
  Shape . pure . fromSegments $
  [ straightXY 0 0, straightXY 1 0, straightXY 2 0
  , straightXY 2 1, straightXY 1 1, straightXY 0 1
  ]

triangle :: Space s => Shape s
triangle =
    Shape . pure . fromSegments $
    [ straightXY 0 0
    , straightXY 1 0
    , straightXY 0 1
    ]

instance HandlesInput token ConfineTreeState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input ^. inputType of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key LetterN -> stateShapeAngle %= normalizeAngle . (^+^ (0.5 @@ deg))
                          Key LetterM -> stateShapeAngle %= normalizeAngle . (^-^ (0.5 @@ deg))
                          Key LetterZ -> whenM (uses stateTraceStep (> 0     )) $ stateTraceStep -= 1
                          Key LetterX -> whenM (uses stateTraceStep (< 100000)) $ stateTraceStep += 1
                          _ -> return ()
              _ -> return ()
          )

main :: IO ()
main = do putStrLn "Started"
          runApplication initialModel
