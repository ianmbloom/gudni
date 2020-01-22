{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Representation
import Graphics.Gudni.Util.Segment

import qualified Graphics.Gudni.Figure.Bezier as B

import Control.Lens
import Control.Monad.State
import Linear
import Linear.Affine


import Data.Maybe
import Control.Lens

data ProjectionState = ProjectionState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   }
   deriving (Show)
makeLenses ''ProjectionState


instance Model ProjectionState where
    screenSize state = Window (Point2 500 250)
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    ioTask = return
    constructScene state _status =
        do text <- (^?! unGlyph) <$> blurb 0.1 AlignMin "e" -- "Georg Guðni Hauksson"
           let angle   = state ^. stateBase . stateAngle
               repMode = state ^. stateBase . stateRepMode
               repDk   = state ^. stateBase . stateRepDk
               offset  = state ^. stateOffset
           return . Scene gray $
               --(if repMode then represent repDk else id) $
               ((transformFromState {-(set stateAngle (0 @@ deg)-} (state ^. stateBase){-)-} $
               overlap [
                       --overlap . fmap (represent False) . transformFromState (set stateAngle (0 @@ deg) (state ^. stateBase)) . projectOnto True (makeOpenCurve [bzX]). (pure :: Bezier s -> [Bezier s]) . translateBy (offset `by` 0) . rotateBy angle $ (myline :: Bezier SubSpace)
                       --colorWith (dark red) . projectOnto path . translateBy (offset `by` 0) . rotateBy angle . rectangle $ 0.125 `by` 0.125 -- {-scaleBy 100 . translateByXY 1 (1) .-} mask . stroke 200 $ smallBz
                       --, colorWith (dark green) . scaleBy 100 . translateByXY 2 (1) . mask . stroke 0.1 $ smallBz
                        doubleDotted path
                       ]) :: ShapeTree Int SubSpace)
      where
        bzX  = Bez (Point2 0 0) (Point2 0.5 1) (Point2 1 0)

        bz1 = Bez (Point2 20 0) (Point2 0 0) (Point2 0 40)
        bz2 = Bez (Point2 0 40) (Point2 0 80) (Point2 40 80)
        bz3 = Bez (Point2 40 80) (Point2 80 80) (Point2 80 160)
        myline = line (0 `by` 0) (0.25 `by` 0) :: Bezier SubSpace
        smallBz = Bez (Point2 0 0) (Point2 100 100) (Point2 10 100)
        path = makeOpenCurve [bz1,bz2,bz3]
        doubleDotted :: Space s => OpenCurve s -> ShapeTree Int s
        doubleDotted path =
           let thickness = 2
               betweenGap = 1
               dotLength = 8
               dotGap = 2
               numDots = floor (arcLength path / (dotLength + dotGap))
           in  colorWith (light . greenish $ blue) .
               projectOnto False path .
               translateByXY 0 (negate ((thickness * 2 + betweenGap) / 2)) .
               overlap .
               horizontallySpacedBy (dotLength + dotGap) .
               replicate numDots .
               overlap .
               verticallySpacedBy (thickness + betweenGap) .
               replicate 2 .
               rectangle $
               dotLength `by` thickness

    providePictureMap _ = noPictures
    handleOutput state target = do
        presentTarget target
        return state

instance HandlesInput ProjectionState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case input of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key ArrowRight -> stateOffset += 0.01
                          Key ArrowLeft  -> stateOffset -= 0.01
                          _ -> return ()
              _ -> return ()
          )

marker :: Color -> Point2 SubSpace -> ShapeTree Int SubSpace
marker color center = colorWith (transparent 0.5 color) $ translateBy center marker0

marker0 :: CompoundTree SubSpace
marker0 = {-rotateBy (1/8 @@ turn) $ translateBy (Point2 (s/2) (s/2)) $-} square
    where
        s = 8
        square :: CompoundTree SubSpace
        square = rectangle (Point2 s s)

main :: IO ()
main = runApplication $ ProjectionState
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
