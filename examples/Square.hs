{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Square
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- The most basic possible example application using the Gudni library.

module Square
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw

import Control.Lens
import Control.Monad.State

import Data.Maybe

data SquareState = SquareState
  { _squareAngle :: Angle SubSpace
  , _squareScale :: SubSpace
  } deriving (Show)
makeLenses ''SquareState

instance HasToken SquareState where
  type TokenOf SquareState = Int

instance HasStyle SquareState where
  type StyleOf SquareState = DefaultStyle

instance Model SquareState where
    screenSize state = Window (Point2 30 30)
    updateModelState frame elapsedTime inputs state =
        execState (
            do  squareAngle .= (realToFrac elapsedTime / 2) @@ turn
            ) $ foldl (flip processInput) state inputs
    ioTask = return
    --shouldLoop _ = False
    constructScene state status =
      let l = --translateBy (Point2 100 100) .      -- translate the child ShapeTree
              scaleBy  (state ^. squareScale) .    -- scale the child ShapeTree based on the current state.
              --rotateBy (state ^. stateAngle) .    -- rotate the child ShapeTree based on the current state.
              withColor yellow .                  -- create a leaf of the ShapeTree and fill the contained CompoundTree with a color.
              mask .
              rectangle $                         -- create new compoundTree with just one leaf that is the outline of a unit square.
              1 `by` 1
      in  return $ withBackgroundColor (light . greenish $ blue) l -- Wrap the ShapeTree in a scene with background color


instance HandlesInput token SquareState where
     processInput input =
         execState $
         case input ^. inputType of
             (InputKey Pressed _ inputKeyboard) ->
                  case inputKeyboard of
                     Key ArrowUp    -> squareScale *=  1.25
                     Key ArrowDown  -> squareScale //= 1.25
                     _                   -> return ()
             _ -> return ()

main :: IO ()
main = runApplication (SquareState (0 @@ turn) 50)
