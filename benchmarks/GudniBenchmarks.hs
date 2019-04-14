{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GudniBenchmarks
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure

import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Fuzzy
import Graphics.Gudni.Util.Draw
import Graphics.Gudni.Util.Scaffolding

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

getTest :: BenchmarkState -> (String, BenchmarkState -> GlyphMonad IO (Glyph (ShapeTree Int SubSpace)))
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
            testName <- glyphString (fst $ getTest state)
            statusGlyphs <- mapM glyphString $ lines status
            let tree = transformFromState testScene state
                statusTree = statusDisplay state testName statusGlyphs
                withStatus = if False then overlap [statusTree, tree] else tree
            return (Scene (light gray) $ view glyphRep withStatus)
    providePictureData state = return $ state ^. statePictures
    handleOutput state target = do  presentTarget target
                                    return state

statusDisplay :: BenchmarkState -> [Glyph (CompoundTree SubSpace)] -> [[Glyph (CompoundTree SubSpace)]]  -> Glyph (ShapeTree Int SubSpace)
statusDisplay state testName status =
    tTranslateXY 1800 800 . --3200 2100 .
    mapGlyph (tRotate (45 @@ deg)) .
    tTranslate (state ^. stateDelta) .
    tScale 30 .
    solid (dark red) .
    paraGrid 1 $
    testName :
    status

transformFromState :: Glyph (ShapeTree Int SubSpace) -> BenchmarkState -> Glyph (ShapeTree Int SubSpace)
transformFromState constructed state =
    let sc    = view stateScale state
        delta = view stateDelta state
        angle = view stateAngle state
    in  tTranslate delta .
        mapGlyph (tRotate angle) .
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
            Right pict   -> do picts <- makePictures [pict]
                               runApplication (initialModel picts :: BenchmarkState)
