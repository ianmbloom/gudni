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

module Plot
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Application
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Plot

import Control.Lens
import Control.Monad.State

import Data.Maybe

data PlotState = PlotState
  { _stateAngle :: Angle SubSpace
  , _stateScale :: SubSpace
  } deriving (Show)
makeLenses ''PlotState

instance Model PlotState where
    screenSize state = Window (Point2 100 100)
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState frame elapsedTime inputs state =
        execStateT (
            do  stateAngle .= (realToFrac elapsedTime / 2) @@ turn
                mapM_ processInput inputs
            ) state
    constructScene state status =
        Scene (light . greenish $ blue) <$> plots state
    providePictureData _ = noPictures
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
main = runApplication (PlotState (0 @@ turn) 50)


-- | All the turtle plots from the plot module.
plots :: Monad m => PlotState -> GlyphMonad m (ShapeTree Int SubSpace)
plots state = return .
              tTranslateXY 100 100 .
              tScale 30 .
              overlap .
              makeGrid 10 16 1 .
              catMaybes .
              map (fmap (solid yellow . SLeaf . segmentsToOutline . pure . closeOpenCurve) . curveLibrary) $
              turtleNames
