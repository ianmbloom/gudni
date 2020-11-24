{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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

module ProjectDemo
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw

import Graphics.Gudni.Util.Subdividable
import Graphics.Gudni.Util.Segment
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad.State
import Linear
import Linear.Affine
import qualified Data.Vector as V
import Control.Applicative
import Data.Maybe

data ProjectionState = ProjectionState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   ,_stateInsideAngle :: Angle SubSpace
   }
   deriving (Show)
makeLenses ''ProjectionState

instance HasToken ProjectionState where
  type TokenOf ProjectionState = Int

slantedLine :: SubSpace -> SubSpace -> Shape SubSpace
slantedLine dotLength thickness =
  let slantOffset = Point2 thickness thickness
      horiOffset = Point2 dotLength 0
  in  segmentsToShape [[Straight (Point2 0 0), Straight horiOffset, Straight (horiOffset + slantOffset), Straight (slantOffset)]]

instance HasStyle ProjectionState where
    type StyleOf ProjectionState = DefaultStyle

instance Model ProjectionState where
    screenSize state = Window (Point2 512 512)
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    constructScene state _status =
        do let text = blurb "Georg Guðni Hauksson    Georg Guðni Hauksson    Georg Guðni Hauksson"
               angle   = state ^. stateBase . stateAngle
               repMode = state ^. stateBase . stateRepMode
               repDk   = state ^. stateBase . stateRepDk
               offset  = state ^. stateOffset
               stacked :: Layout (StyleOf ProjectionState)
               stacked = stack $
                            [ unprojected
                            , place . represent False $ path
                            , projectOnto path $ unprojected
                            , projectOnto path $ subdivide 6 unprojected
                            , scaleBy 30 . withColor blue $ text
                            ]
           return $ withBackgroundColor gray $
               --(if repMode then represent repDk else id) $
               transformFromState (state ^. stateBase) stacked



        where
        path :: OpenCurve SubSpace
        path = tr "path" $
               applyScale 500 $
               segmentsToOpenCurve [ Curved (Point2 0 0  ) (Point2 2 0)
                                   , Curved (Point2 2 0.5) (Point2 2 1)
                                   ] (Point2 4 1)

               -- plotTurtle initialTurtleState $ [ TTurn Smooth R
               --                                 , TTurn Smooth L
               --                                 , TTurn Smooth R
               --                                 , TTurn Smooth L
               --                                 , TTurn Smooth L
               --                                 , TTurn Smooth R
               --                                 , TTurn Smooth L
               --                                 , TTurn Smooth R
               --                              ]
        pathLength = arcLength path
        layerThickness = 50
        unprojected :: Layout (StyleOf ProjectionState)
        unprojected = overlap [
                       -- follow text path
                       translateByXY 0 (-1) . withColor white . mask . rectangle $ Point2 pathLength 2
                       -- translateByXY (state ^. stateOffset) 0 .
                       ,  translateByXY 0 (toAlong Vertical (-(layerThickness * 3 / 2))) .
                          overlap $
                          [ slashDotted (transparent 0.5 red) layerThickness
                          , translateByXY 0 (toAlong Vertical layerThickness) $ doubleDotted (transparent 0.5 $ light blue) layerThickness
                          , translateByXY 0 (toAlong Vertical layerThickness) $ withColor (light green) . mask . rectangle $ Point2 pathLength layerThickness
                          , translateByXY 0 (toAlong Vertical (2*layerThickness)) $ slashDotted (transparent 0.5 purple) layerThickness
                          ]
                       ]

        follow :: CompoundLayout (StyleOf ProjectionState) -> OpenCurve SubSpace -> Layout (StyleOf ProjectionState)
        follow text path =
           let thickness = 2
               betweenGap = 1
               dotLength = 8
               dotGap = 2
               numDots = floor (pathLength / realToFrac (dotLength + dotGap))
           in  withColor (dark . greenish $ red) .
               projectOnto path .
               translateByXY (toAlong Horizontal $ state ^. stateOffset) 0 .
               translateByXY 10 3 .
               scaleBy 5 $
               text

        doubleDotted :: Color SubSpace -> SubSpace -> Layout (StyleOf ProjectionState)
        doubleDotted color thickness =
           let dotThickness = thickness / 3
               dotLength = 80
               dotGap = 20
               numDots = floor (pathLength / realToFrac (dotLength + dotGap))
           in  withColor color .
               overlap .
               horizontallySpacedBy translateBy zeroPoint (toAlong Horizontal . realToFrac $ dotLength + dotGap) .
               replicate numDots .
               overlap .
               verticallySpacedBy translateBy zeroPoint (toAlong Vertical . realToFrac $ dotThickness * 2) .
               replicate 2 .
               mask .
               rectangle $
               (toAlong Horizontal dotLength) `by` (toAlong Vertical dotThickness)

        slashDotted :: Color SubSpace -> SubSpace -> Layout (StyleOf ProjectionState)
        slashDotted color thickness =
           let dotLength = 20
               dotGap = 20
               numDots = floor (arcLength path / realToFrac (dotLength + dotGap))
           in  withColor color .
               overlap .
               horizontallySpacedBy translateBy zeroPoint (toAlong Horizontal . realToFrac $ dotLength + dotGap) .
               replicate numDots .
               mask $
               slantedLine dotLength thickness

    providePictureMap _ = noPictures
    handleOutput state target = do
        presentTarget target
        return state

instance HandlesInput token ProjectionState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case (input ^. inputType) of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key ArrowRight -> stateOffset += 10.01
                          Key ArrowLeft  -> stateOffset -= 10.01
                          Key ArrowUp    -> stateInsideAngle %= normalizeAngle . (^+^ (3 @@ deg))
                          Key ArrowDown  -> stateInsideAngle %= normalizeAngle . (^-^ (3 @@ deg))
                          _ -> return ()

              _ -> return ()
          )

marker :: Color SubSpace -> Point2 SubSpace -> Layout (StyleOf ProjectionState)
marker color center = withColor (transparent 0.5 color) $ translateBy center marker0

marker0 :: CompoundLayout (StyleOf ProjectionState)
marker0 = {-rotateBy (1/8 @@ turn) $ translateBy (Point2 (s/2) (s/2)) $-} square
    where
        s = 8
        square :: CompoundLayout (StyleOf ProjectionState)
        square = mask $ rectangle (Point2 s s)

main :: IO ()
main = runApplication $ ProjectionState
       (BasicSceneState
           { _stateScale       = 1
           , _stateDelta       = Point2 100 50
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
           , _stateCursor      = Point2 0 0
           }
       ) 0 (0 @@ deg)
