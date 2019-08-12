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
    updateModelState _frame _elapsedTime _inputs state = return state
    constructScene _state _status = return . Scene clearBlack . Just $
        overlap (byT ++ map (tTranslate (Point2 240 0)) byLength)
      where
        bz = V3 (Point2 20 20) (Point2 170 70) (Point2 220 220)
        l = B.arcLength bz
        byT = map (marker blue . B.eval bz) [0, 0.1..1.0]
        byLength = map (marker red . B.eval bz . B.inverseArcLength 1e-6 bz) [0, 0.1*l .. l]
    providePictureMap _ = noPictures
    handleOutput state target = do
        presentTarget target
        return state

-- overlap :: [ShapeTree ()] -> ShapeTree ()
-- overlap = foldr1 (SMeld ())

marker :: Color -> Point2 SubSpace -> ShapeTree Int SubSpace
marker color center = solid color $ tTranslate center marker0

marker0 :: CompoundTree SubSpace
marker0 = tRotate (1/8 @@ turn) $ tTranslate (Point2 (s/2) (s/2)) $ square
    where
        s = 8
        square :: CompoundTree SubSpace
        square = rectangle (Point2 s s)

main :: IO ()
main = runApplication ProjectionState
