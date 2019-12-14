{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GudniBenchmarks
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Fuzzy


import Data.Word
import Data.List(isInfixOf)
import Data.Maybe(listToMaybe, fromMaybe)

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse

import Codec.Picture

import GudniTests

import System.IO.Silently

import System.Info
import Data.Maybe

getTest :: BenchmarkState -> (String, BenchmarkState -> FontMonad IO (ShapeTree Int SubSpace))
getTest state = (state ^. stateTests) !! (state ^. stateCurrentTest)

instance Model BenchmarkState where
    screenSize state = --FullScreen
                       Window $ Point2 1440 900
    shouldLoop _ = True
    fontFile _ = findDefaultFont
    updateModelState frame elapsedTime inputs state =
        flip execStateT state $
            do  mapM_ processInput inputs
                lastTime <- use stateLastTime
                stateFrameNumber .= frame
                stateLastTime .= elapsedTime
                speed <- use stateSpeed
                whenM(not <$> use statePaused) $
                    do  direction <- use stateDirection
                        let f = if direction then (+) else (-)
                            timeDelta = elapsedTime - lastTime
                            dt = realToFrac timeDelta * realToFrac speed
                        statePlayhead %= (`f` dt)
    constructScene state status =
        do  testScene <- (snd $ getTest state) state
            let testName = (fst $ getTest state)
            (statusTree :: Maybe (ShapeTree Int SubSpace)) <- (^?! unGlyph) <$> statusDisplay state testName (lines status)
            let tree = transformFromState state testScene
                withStatus = if False then overlap [fromJust statusTree, tree] else tree
            return . Scene (light gray) $ Just $ withStatus
    providePictureMap state = return $ state ^. statePictureMap
    handleOutput state target = do  presentTarget target
                                    return state

statusDisplay :: Monad m => BenchmarkState -> String -> [String] -> FontMonad m (Glyph (ShapeTree Int SubSpace))
statusDisplay state testName status =
    tTranslateXY 1800 800 . --3200 2100 .
    tTranslate (state ^. stateDelta) .
    tScale 30 .
    fmap (solid (dark red)) .
    paragraph 0.1 0.1 AlignMin AlignMin $
    unlines (testName :
    status)

transformFromState :: BenchmarkState -> ShapeTree Int SubSpace -> ShapeTree Int SubSpace
transformFromState state constructed =
    let sc    = view stateScale state
        delta = view stateDelta state
        angle = view stateAngle state
    in  tTranslate delta .
        tRotate angle .
        tScale sc $
        constructed

processInput :: Monad m => Input (Point2 PixelSpace) -> StateT BenchmarkState m ()
processInput input =
    case input of
        (InputKey Pressed _ inputKeyboard) ->
            do  speed <- use stateSpeed
                pace  <- use statePace
                tests <- use stateTests
                case inputKeyboard of
                    KeySymbol SymbolSpace  -> statePaused %= not
                    KeyArrow  ArrowUp      -> stateSpeed *=  1.25
                    KeyArrow  ArrowDown    -> stateSpeed //= 1.25
                    KeyLetter LetterW      -> stateDelta %= (^+^ Point2   0    (-pace))
                    KeyLetter LetterS      -> stateDelta %= (^+^ Point2   0      pace )
                    KeyLetter LetterA      -> stateDelta %= (^+^ Point2 (-pace)  0    )
                    KeyLetter LetterD      -> stateDelta %= (^+^ Point2   pace   0    )
                    KeyLetter LetterY      -> stateDirection %= not
                    KeyLetter LetterR      -> stateAngle %= normalizeAngle . (^+^ ((speed/30) @@ turn))
                    KeyLetter LetterT      -> stateAngle %= normalizeAngle . (^-^ ((speed/30) @@ turn))
                    KeyArrow  ArrowRight   -> whenM (uses stateCurrentTest (< (length tests - 1))) $ stateCurrentTest += 1
                    KeyArrow  ArrowLeft    -> whenM (uses stateCurrentTest (> 0)) $ stateCurrentTest -= 1
                    KeySymbol SymbolComma  -> whenM (uses stateStep (> 0 {-arbitrary-})) $ stateStep -= 1
                    KeySymbol SymbolPeriod -> whenM (uses stateStep (< 1000)) $ stateStep += 1
                    KeySymbol SymbolRightBracket -> stateScale *=  1.1
                    KeySymbol SymbolLeftBracket  -> stateScale //= 1.1
                    _                   -> return ()
        (InputMouse detection modifier clicks positionInfo) ->
            case detection of
              Pressed -> stateCursor .= positionInfo
              _ -> return ()
        _ -> return ()

main :: IO ()
main = --silence $
       do putStrLn "Started"
          jpeg <- readJpeg "image/hero-yellow-flowers.jpg"
          case jpeg of
            Left message -> putStrLn message
            Right pict   -> do let picts = makePictureMap [("flowers",pict)]
                               runApplication (initialModel picts :: BenchmarkState)
