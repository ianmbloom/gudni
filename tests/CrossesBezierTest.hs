{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}

module CrossesBezierTest
  ( main
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Interface
import Graphics.Gudni.Application
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw

import Control.Lens
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.State

import System.Random
import Control.Monad.Random

data CrossTestState = CrossTestState
  { _stateBase        :: BasicSceneState
  } deriving (Show, Generic)
makeLenses ''CrossTestState

instance Out CrossTestState

initialModel =
    CrossTestState
    { _stateBase = BasicSceneState
        { _stateScale       = 5
        , _stateDelta       = Point2 150 150
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
        , _stateCursor      = Point2 0 0
        }
    }


instance HasStyle CrossTestState where
  type StyleOf CrossTestState = DefaultStyle

instance Model CrossTestState where
    screenSize state = --FullScreen
                       Window $ Point2 1024 512
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    ioTask = return
    constructScene state status =
        do  let statusTree = statusDisplay (state ^. stateBase) "Test of orthogonal line segments crossing curves" (lines status)
                treeScene  = transformFromState (state ^. stateBase) runCrossTests --runLessCurveTests) --testScene
                withStatus = if False then overlap [statusTree, treeScene] else treeScene
            return $ withBackgroundColor gray withStatus
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state
    dumpState state inputs =
         do putStrLn $ pretty state
            when (not . null $ inputs) $ putStrLn $  pretty inputs

showCross :: (Axis axis, IsStyle style)
          => axis
          -> Point2 (SpaceOf style)
          -> Point2 (SpaceOf style)
          -> Bezier (SpaceOf style)
          -> Layout style
showCross axis start end bez =
    let doesCross = crossesBezierAlong False axis (start ^. along axis) (start ^. athwart axis) (end ^. along axis) bez
        color = if doesCross then red else green
    in  withColor color . mask . stroke 0.3 . makeOpenCurve $ [line start end]

overCs :: (Point2 s -> Point2 s) -> (EitherAxis, Point2 s, Point2 s) -> (EitherAxis, Point2 s, Point2 s)
overCs f (x, a, b) = (x, f a, f b)

crossTests :: forall style
           .  (IsStyle style)
           => Bezier (SpaceOf style)
           -> Layout style
crossTests bez =
   let s :: SpaceOf style
       s = 50
       cs :: [(EitherAxis, Point2 (SpaceOf style), Point2 (SpaceOf style))]
       cs = [
               (Right Vertical,  Point2 0   0,   Point2 0   1  )
             , (Left Horizontal, Point2 0   1,   Point2 1   1  )
             , (Right Vertical,  Point2 1   1,   Point2 1   0  )
             , (Left Horizontal, Point2 1   0,   Point2 0   0  )
             , (Right Vertical,  Point2 0.5 0,   Point2 0.5 1  )
             , (Left Horizontal, Point2 0 0.5,   Point2 1   0.5)

             , (Right Vertical,  Point2 0.1 0.1, Point2 0.1 0.9)
             , (Left Horizontal, Point2 0.1 0.9, Point2 0.9 0.9)
             , (Right Vertical,  Point2 0.9 0.9, Point2 0.9 0.1)
             , (Left Horizontal, Point2 0.9 0.1, Point2 0.1 0.1)

             , (Right Vertical,  Point2 0.25 0.25, Point2 0.25 0.75)
             , (Left Horizontal, Point2 0.25 0.75, Point2 0.75 0.75)

             , (Right Vertical,  Point2 0.75 0.75, Point2 0.75 0.25)
             , (Left Horizontal, Point2 0.75 0.25, Point2 0.25 0.25)

             , (Right Vertical,  Point2 0 (-0.5),Point2 0   0  )
             , (Left  Horizontal,Point2 (-0.5) 0,Point2 0   0  )

             , (Right Vertical,   Point2 0 1, Point2 0      1.5)
             , (Left  Horizontal, Point2 0 1, Point2 (-0.5) 1  )

             , (Right Vertical,   Point2 1 1, Point2 1 1.5)
             , (Left  Horizontal, Point2 1 1, Point2 1.5 1)

             , (Right Vertical,   Point2 1 0, Point2 1 (-0.5))
             , (Left  Horizontal, Point2 1 0, Point2 (1.5) 0)
            ]
       reBez :: Bezier (SpaceOf style)
       reBez = overBezier (^* s) bez

       zs :: [(EitherAxis, Point2 (SpaceOf style), Point2 (SpaceOf style))]
       zs = map (overCs (^* s)) cs
   in  overlap $ (map (\(e, start, end) ->
                       withEitherAxis showCross showCross e start end reBez) zs)
                 ++ [ (withColor white . mask . stroke 0.5 $ makeOpenCurve [reBez])]

revTests bez = crossTests bez --rack $ map crossTests $ [bez, reverseBezier bez]

mkBez x0 y0 x1 y1 x2 y2 = Bez (Point2 x0 y0) (Point2 x1 y1) (Point2 x2 y2)

runCrossTests :: Layout DefaultStyle
runCrossTests =
  let diagonalLines = [ mkBez 0   0   0.5 0.5 1   1
                      , mkBez 0   1   0.5 0.5 1   0
                      ]
      centerLines = [ mkBez 0   0.5 0.5 0.5 1   0.5
                    , mkBez 0.5 0   0.5 0.5 0.5 1
                    ]

      diagonalCurves = [ mkBez 0 0 0 1 1 1
                       , mkBez 0 0 1 0 1 1
                       , mkBez 1 0 1 1 0 1
                       , mkBez 1 0 0 0 0 1
                       ]

      alignedCurves = [ mkBez 0 0 0.5    1.5  1 0
                      , mkBez 1 0 (-0.5) 0.5  1 1
                      , mkBez 0 0 1.5    0.5  0 1
                      , mkBez 0 1 0.5  (-0.5) 1 1
                      ]

      sideLines = [ mkBez 0 0 0 0.5   0 1
                  , mkBez 0 1 0.5 1   1 1
                  , mkBez 1 1 1   0.5 1 0
                  , mkBez 1 0 0.5 0   0 0
                  ]
      randoms = [  overBezier (applyTranslation (Point2 (-0.43118674) 0)) $ mkBez 0.43118674 0.63469476 0.15476434 0.78878772 0.88651923 0.29197949
                 , overBezier (applyTranslation (Point2 (-0.43118674) 0)) $ mkBez 0.88651923 0.29197949 0.93376459 0.37227292 0.13838916 0.37558884
                 , overBezier (applyScale 1 . applyTranslation (negate $ Point2 0.12040964 0.84966302)) $ mkBez 0.17114418 0.54112964 (-0.09138076) 0.85783649 0.12288326 0.84959149
                 , overBezier (applyScale 1 . applyTranslation (negate $ Point2 0.12040964 0.84966302)) $ mkBez 0.12288326 0.84959149 0.16324765    0.13756374 0.17952071 0.61213672
                ]
  in  rack . map (stack . map revTests) $
           [
              diagonalLines
            ++ centerLines
            , diagonalCurves
            , alignedCurves
            , sideLines
            ,
            randoms
           ]
{-
randomCross ::
randomCross =
  let range = (-2, 2)
  do
      baseline <- getRandomR range
      start    <- getRandomR range
      end      <- getRandomR range
      vert     <- getRandom
      let eitherAxis = if vert then Right Vertical else Left Horizontal
      return (eitherAxis, baseline, start, end)


randomCrosses =
  crosses = evalRand (randomCross .
                      mkStdGen $ round $
                      state ^. stateBase . statePlayhead
-}

instance HandlesInput token CrossTestState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input ^. inputType of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          _ -> return ()
              _ -> return ()
          )

main :: IO ()
main = do putStrLn "Started"
          runApplication initialModel
