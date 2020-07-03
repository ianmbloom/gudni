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
import Graphics.Gudni.Experimental.ConstructConfineTree

import Data.Maybe(listToMaybe, fromMaybe, fromJust)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Text.PrettyPrint.GenericPretty
import System.Info

import Control.Monad.Random
import System.Random

data ConfineTreeState = ConfineTreeState
  { _stateBase        :: BasicSceneState
  , _stateShapeAngle  :: Angle SubSpace
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
        , _statePace        = 10
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 8
        , _stateFrameNumber = 0
        , _stateStep        = 45
        , _stateRepMode     = False
        , _stateRepDk       = False
        }
    --, _stateTree        = tree
    , _stateShapeAngle = 90 @@ deg -- 2.32131 @@ rad -- 0 @@ rad -- 45 @@ deg
    }

instance HasToken ConfineTreeState where
  type TokenOf ConfineTreeState = Int

labelPoint :: forall m token
           .  (Monad m)
           => ConfineTree SubSpace
           -> Point2 SubSpace
           -> FontMonad m (ShapeTree token SubSpace)
labelPoint tree point =
  let (anchorPoint, winding) = pointWinding tree point
      color = if even winding
              then white
              else black
      anchorLine :: CompoundTree SubSpace
      anchorLine = if point /= anchorPoint
                   then mask . shapeFrom . stroke 0.3 $ line point anchorPoint
                   else emptyItem
  in
  do  text <- (^?! unGlyph) <$> blurb 0.1 AlignMin (show winding) :: FontMonad m (CompoundTree SubSpace)
      return $ withColor color .
               overlap $
                   [ translateBy point . closedCircle $ 3
                   --, translateBy point . translateByXY 4 (-12.5) . scaleBy 25 $ text
                   --, anchorLine
                   ]


instance Model ConfineTreeState where
    screenSize state = --FullScreen
                       Window $ Point2 512 512
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    ioTask = return
    constructScene state status =
        do  let range = makePoint 2 2
                randomCurve = evalRand (fuzzyCurve range 20) (mkStdGen $ (state ^. stateBase . stateStep)) :: ShapeTree Int SubSpace
                maxSize   = 64
                table     = buildTreeOrderTable maxSize
                scene     = scaleBy 400 $
                            overlap [ rotateBy (state ^. stateShapeAngle) .
                                      translateByXY (-1) (-1)
                                      $
                                      randomCurve
                                      -- ,
                                      -- withColor blue .
                                      -- mask .
                                      -- shapeFrom .
                                      -- rotateBy (state ^. stateShapeAngle) $
                                      -- circle
                                      -- ,
                                      -- withColor red .
                                      -- mask .
                                      -- shapeFrom .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- translateByXY 0.25 0.25 $
                                      -- circle
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
                                      -- mask .
                                      -- shapeFrom .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- translateByXY 0.5 0.5 $
                                      -- circle
                                      -- ,
                                      -- withColor blue .
                                      -- mask .
                                      -- shapeFrom .
                                      -- rotateBy (state ^. stateShapeAngle) .
                                      -- translateByXY (-0.5) (-0.5)
                                      -- $
                                      -- triangle
                                      -- rectangle (Point2 1 1)
                                      -- sixPointRectangle
                                    ]
                tree      = --trWith pretty "confineTree" .
                            (addToConfineTree table maxSize 1 Nothing :: ShapeTree Int SubSpace -> ConfineTree SubSpace) $
                            scene
            constructed <- constructConfineTree tree
            randomPoints <- mapM (labelPoint tree . scaleBy 800) $
                            evalRand (take 1000 <$> getRandomRs (Point2 (-1) (-1), Point2 1 1)) .
                            mkStdGen $ round $
                            state ^. stateBase . statePlayhead
            let  testScene = overlap [ constructed
                                     , overlap randomPoints
                                   --, constructConfineTreeFromBoxes tree
                                     , scene
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

triangle :: Space s => Outline s
triangle =
        fromSegments [straightXY 0 0, straightXY 1 0, straightXY 0 1
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
main = do putStrLn "Started"
          runApplication initialModel
