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
-- Demonstration of equal-distance projection, and equal spacing in t-paramater.

module ProjectDemo
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Application
import Graphics.Gudni.Layout

import qualified Graphics.Gudni.Figure.Bezier as B

import Control.Lens
import Control.Monad.State
import Linear

import Data.Maybe

data ProjectionState = ProjectionState
    deriving Show

instance Model ProjectionState where
    screenSize state = Window (Point2 500 250)
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState _frame _elapsedTime _inputs state = state
    ioTask = return
    constructScene _state _status = return . Scene gray $
        overlap (byT ++ map (translateBy (Point2 0 0)) byLength)
      where
        bz = Bez (Point2 20 20) (Point2 220 20) (Point2 220 220)
        l = arcLength bz
        byT = map (marker blue . eval bz) [0, 0.1..1.0]
        byLength = map (marker red . eval bz . inverseArcLength 8 (Just 1e-6) bz) [0, 0.1*l .. l]
        long :: ShapeTree Int SubSpace
        long = solid (transparent 0.5 green) $ projection bz $ rectangle (Point2 100 3)
    providePictureMap _ = noPictures
    handleOutput state target = do
        presentTarget target
        return state

marker :: Color -> Point2 SubSpace -> ShapeTree Int SubSpace
marker color center = solid (transparent 0.5 color) $ translateBy center marker0

marker0 :: CompoundTree SubSpace
marker0 = rotateBy (1/8 @@ turn) $ translateBy (Point2 (s/2) (s/2)) $ square
    where
        s = 8
        square :: CompoundTree SubSpace
        square = rectangle (Point2 s s)

main :: IO ()
main = runApplication ProjectionState
