{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.Lens
import Control.Monad.State

import Data.Maybe

data SquareState = SquareState
  { _stateAngle :: Angle SubSpace
  , _stateScale :: SubSpace
  } deriving (Show)
makeLenses ''SquareState

instance Model SquareState where
    screenSize state = Window (Point2 100 100)
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState frame elapsedTime inputs state =
        execStateT (
            do  stateAngle .= (realToFrac elapsedTime / 2) @@ turn
                mapM_ processInput inputs
            ) state
    constructScene state status =
        return .                          -- Push the scene into the FontMonad
        Scene (light . greenish $ blue) . -- Wrap the ShapeTree in a scene with background color
        Just .
        tTranslate (Point2 100 100) .     -- translate the child ShapeTree
        tScale  (state ^. stateScale) .   -- scale the child ShapeTree based on the current state.
        tRotate (state ^. stateAngle) .   -- rotate the child ShapeTree based on the current state.
        solid yellow $                    -- create a leaf of the ShapeTree and fill the contained CompoundTree with a solid color.
        unitSquare                        -- create new compoundTree with just one leaf that is the outline of a unit square.
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

processInput input =
    case input of
        (InputKey Pressed _ inputKeyboard) ->
             case inputKeyboard of
                KeyArrow ArrowUp    -> stateScale *=  1.25
                KeyArrow ArrowDown  -> stateScale //= 1.25
                _                   -> return ()
        _ -> return ()

main :: IO ()
main = runApplication (SquareState (0 @@ turn) 50)
