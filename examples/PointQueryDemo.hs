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

module PointQueryDemo
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
import Control.Monad.Random

import Control.Lens
import Control.Monad.State
import Linear
import Linear.Affine
import qualified Data.Vector as V


import Data.Maybe
import Control.Lens

data PQState = PQState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   ,_stateGlyphs      :: [(Int, Color, CompoundTree SubSpace)]
   }
   deriving (Show)
makeLenses ''PQState

randomGlyph :: (Space s, Random s, RandomGen g)
            => V.Vector (CompoundTree s) -> Point2 s -> s -> s -> Rand g (Int, Color, CompoundTree s)
randomGlyph glyphs range minRad maxRad =
  do  i <- getRandomR(0, V.length glyphs - 1)
      let g = (V.!) glyphs i
      hue       <- getRandomR(0,360)
      sat       <- getRandomR(0.3,1)
      lightness <- getRandomR(0.4,0.9)
      alpha     <- return 1 -- getRandomR(0.2,0.5)
      let color  = transparent alpha $ hslColor hue sat lightness
      token  <- getRandomR(0,32768)
      angle  <- getRandomR(0,360)
      radius <- getRandomR(minRad,maxRad)
      point  <- getRandomR(makePoint 0 0, range)
      return (token,  color , translateBy point . scaleBy radius . rotateBy (angle @@ deg) $ g)

applyGlyph (token, color, compound) = assignToken token . colorWith color $ compound

instance Model PQState where
    screenSize state = Window (Point2 500 250)
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime inputs state = foldl (flip processInput) state inputs
    ioTask = return
    constructScene state _status =
      do defaultGlyphs <- V.fromList <$> glyphString ['!'..'z']
         let randomGlyphs = evalRand (sequence . replicate 2000 $
                            randomGlyph defaultGlyphs (makePoint 800 800) 10 200) (mkStdGen 3000)

         return . Scene gray $
             ((transformFromState (state ^. stateBase) $
             overlap (map applyGlyph randomGlyphs)

                     ) :: ShapeTree Int SubSpace)
    constructQueries =
    providePictureMap _ = noPictures
    handleOutput state target = do
        presentTarget target
        return state

instance HandlesInput PQState where
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
main = runApplication $ PQState
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
       ) 0.75 []
