{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Graphics.Gudni.Experimental.TreeOrderTable
import Graphics.Gudni.Experimental.ConfineTree
import Graphics.Gudni.Experimental.ConstructConfineTree

import Data.Maybe(listToMaybe, fromMaybe, fromJust)
import Control.Lens
import Control.Monad
import Control.Monad.State


import System.Info

data ConfineTreeState = ConfineTreeState
  { _stateBase        :: BasicSceneState
  , _stateShapeAngle  :: Angle SubSpace
  } deriving (Show)
makeLenses ''ConfineTreeState

initialModel =
    ConfineTreeState
    { _stateBase = BasicSceneState
        { _stateScale       = 1
        , _stateDelta       = Point2 400 400
        , _stateAngle       = 0 @@ deg -- 0.02094 @@ rad -- 0 @@ turn-- quarterTurn
        , _statePaused      = True
        , _stateSpeed       = 0.1
        , _statePace        = 10
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 0
        , _stateFrameNumber = 0
        , _stateStep        = 1
        , _stateRepMode     = False
        , _stateRepDk       = False
        }
    --, _stateTree        = tree
    , _stateShapeAngle = 0 @@ rad -- 45 @@ deg
    }

instance HasToken ConfineTreeState where
  type TokenOf ConfineTreeState = Int

instance Model ConfineTreeState where
    screenSize state = --FullScreen
                       Window $ Point2 512 512
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    ioTask = return
    constructScene state status =
        do  let maxSize   = 64
                table     = buildTreeOrderTable maxSize
                testScene = constructConfineTree (state ^. stateBase . stateStep) $
                            (addToConfineTree table maxSize 1 Nothing :: ShapeTree Int SubSpace -> Maybe (ConfineTree SubSpace)) $
                            overlap [
                                      withColor blue .
                                      mask .
                                      shapeFrom .
                                      rotateBy (state ^. stateShapeAngle) .
                                      scaleBy 200 $
                                      --rectangle (Point2 1 1)
                                      --sixPointRectangle
                                      circle
                                    , withColor red .
                                      mask .
                                      shapeFrom .
                                      rotateBy (state ^. stateShapeAngle) .
                                      translateByXY 50 50 .
                                      scaleBy 200 $
                                      --rectangle (Point2 1 1)
                                      --sixPointRectangle
                                      circle
                                    ]

            statusTree <- (^?! unGlyph) <$> statusDisplay (state ^. stateBase) "Test ConfineTree" (lines status)
            let tree = transformFromState (state ^. stateBase) testScene
                withStatus = if False then overlap [statusTree, tree] else tree
            return $ (Scene (light gray) $ withStatus)
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

sixPointRectangle :: Space s => Outline s
sixPointRectangle =
        fromSegments [straightXY 0 0, straightXY 1 0, straightXY 2 0
                     ,straightXY 2 1, straightXY 1 1, straightXY 0 1
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
                          _ -> return ()
              _ -> return ()
          )

main :: IO ()
main = --silence $
       do putStrLn "Started"
          runApplication initialModel
