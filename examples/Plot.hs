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

instance HasToken PlotState where
    type TokenOf PlotState = Int

instance Model PlotState where
    screenSize state = Window (Point2 1024 768)
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState frame elapsedTime inputs state =
        execState (
            do  stateAngle .= (realToFrac elapsedTime / 2) @@ turn
            ) $ foldl (flip undefined {-processInput-}) state inputs
    ioTask = return
    constructScene state status =
        Scene (light . greenish $ blue) <$> plots state
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

processSimpleInput input =
    case input of
        (InputKey Pressed _ inputKeyboard) ->
             case inputKeyboard of
                Key ArrowUp    -> stateScale *=  1.25
                Key ArrowDown  -> stateScale //= 1.25
                _              -> return ()
        _ -> return ()

main :: IO ()
main = runApplication (PlotState (0 @@ turn) 50)


-- | All the turtle plots from the plot module.
plots :: Monad m => PlotState -> FontMonad m (ShapeTree Int SubSpace)
plots state = return .
              translateByXY 100 300 .
              scaleBy 30 .
              overlap .
              gridOf 10 16 1 .
              catMaybes .
              map (fmap (withColor yellow . mask . stroke 0.5) . (curveLibrary :: String -> Maybe (OpenCurve SubSpace))) $
              turtleNames
