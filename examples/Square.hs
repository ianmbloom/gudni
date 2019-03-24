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
import Graphics.Gudni.Util.Draw

import Control.Lens
import Control.Monad.State

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
        return .
        Scene (light . greenish $ blue) .
        sTranslate (Point2 100 100) .
        sScale  (state ^. stateScale) .
        sRotate (state ^. stateAngle) .
        solid yellow .
        rectangle $
        Point2 1 1
    providePictureData _ = noPictures

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
