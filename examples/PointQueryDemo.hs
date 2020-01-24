{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.List.Lens

data PQGlyph s = PQGlyph
   { _glyphSelected :: Bool
   , _glyphColor :: Color
   , _glyphOffset:: Point2 s
   , _glyphScale :: s
   , _glyphAngle :: Angle s
   , _glyphOrd :: Int
   }
   deriving (Show)
makeLenses ''PQGlyph

data PQState = PQState
   {_stateBase        :: BasicSceneState
   ,_stateOffset      :: SubSpace
   ,_stateGlyphs      :: [PQGlyph SubSpace]
   }
   deriving (Show)
makeLenses ''PQState

randomGlyph :: (Space s, Random s, RandomGen g)
            => Int -> Point2 s -> s -> s -> Rand g (PQGlyph s)
randomGlyph numGlyphs range minScale maxScale =
  do  i <- getRandomR(0, numGlyphs - 1)
      hue       <- getRandomR(0,360)
      sat       <- getRandomR(0.3,1)
      lightness <- getRandomR(0.4,0.9)
      alpha     <- return 1 -- getRandomR(0.2,0.5)
      let color  = transparent alpha $ hslColor hue sat lightness
      delta <- getRandomR(makePoint 0 0, range)
      angle  <- (@@ deg) <$> getRandomR(0,360)
      scale <- getRandomR(minScale,maxScale)
      return $ PQGlyph False color delta scale angle i

applyGlyph glyphs token (PQGlyph selected color delta scale angle i) =
   let finalColor = if selected then black else color
   in  assignToken token .
       colorWith finalColor .
       translateBy delta .
       scaleBy scale .
       rotateBy angle $
       (V.!) glyphs i

instance HasToken PQState where
  type TokenOf PQState = Int

charList = ['!'..'z']

instance Model PQState where
    screenSize state = Window (Point2 64 64)
    updateModelState _frame _elapsedTime inputs state =
      do let state' = foldl (flip processInput) state inputs
         if null (state' ^. stateGlyphs)
         then let randomGlyphs = evalRand (sequence . replicate 10 $
                           randomGlyph (length charList) (makePoint 100 100) 10 200) (mkStdGen 3000)
              in
              (set stateGlyphs randomGlyphs state')
         else state'
    constructScene state _status =
      do defaultGlyphs <- V.fromList <$> glyphString charList
         return . Scene gray .
               transformFromState (state ^. stateBase) .
               overlap .
               imap (applyGlyph defaultGlyphs) $
               state ^. stateGlyphs


instance HandlesInput Int PQState where
   processInput input =
          over stateBase (processInput input) . (
          execState $
          case (input ^. inputType) of
              (InputKey Pressed _ inputKeyboard) ->
                  do  case inputKeyboard of
                          Key ArrowRight -> stateOffset += 0.01
                          Key ArrowLeft  -> stateOffset -= 0.01
                          _ -> return ()
              (InputMouse Pressed _ _ _) ->
                 case input ^. inputToken of
                    Just token -> stateGlyphs . ix token . glyphSelected .=  True
                    Nothing -> return ()
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
