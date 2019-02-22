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
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Draw

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

getTest :: BenchmarkState -> (String, BenchmarkState -> ShapeTree)
getTest state = (state ^. stateTests) !! (state ^. stateCurrentTest)

instance Model BenchmarkState where
    screenSize state = --FullScreen
                       Window $ Point2 600
                                       400
    shouldLoop _ = True
    fontFile state = fromMaybe "Times New Roman.ttf" <$> listToMaybe . filter (isInfixOf "Times New Roman.ttf") <$> fontLibrary
    modelCursor state = state ^. stateCursor
    updateModel frame elapsedTime inputs state =
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
    constructFigure state status =
        let test = snd $ getTest state
            tree = transformFromState test state
            cursor size thickness = tTranslate (convert $ _stateCursor state) .
                                    solid (transparent 0.5 red) $
                                    cAdd (tTranslateXY (-size/2) 0 $ rectangle $ Point2 size thickness)
                                         (tTranslateXY 0 (-size/2) $ rectangle $ Point2 thickness size)
            withCursor = if False then overlap [cursor 100 1, tree] else tree
            statusTree = statusDisplay state status
            withStatus = if False then overlap [statusTree, withCursor] else withCursor
        in  return (ShapeRoot gray withStatus, "textForm")
    pictureData state = return $ state ^. statePictures

statusDisplay state status =
    --tTranslateXY 1500 800 . --3200 2100 .
    tTranslate (state ^. stateDelta) .
    tScale 20 .
    solid black .
    overlap .
    paraGrid 1 $
    fst (getTest state) ++
    "\n" ++
    status

transformFromState :: (BenchmarkState -> ShapeTree) -> BenchmarkState -> ShapeTree
transformFromState construction state =
  let sc    = view stateScale state
      delta = view stateDelta state
      angle = view stateAngle state
      constructed = construction state
  in  tTranslate delta .
      tRotate angle .
      tScale sc $
      constructed

processInput :: Monad m => Input (Point2 IntSpace) -> StateT BenchmarkState m ()
processInput input =
    case input of
        (InputKey Pressed _ inputKeyboard) ->
            do  speed <- use stateSpeed
                pace  <- use statePace
                tests <- use stateTests
                case inputKeyboard of
                    KeySymbol SymbolSpace  -> statePaused %= not
                    KeyArrow ArrowUp       -> stateSpeed *=  1.25
                    KeyArrow ArrowDown     -> stateSpeed //= 1.25
                    KeyLetter LetterW      -> stateDelta %= (^+^ Point2   0    (-pace))
                    KeyLetter LetterS      -> stateDelta %= (^+^ Point2   0      pace )
                    KeyLetter LetterA      -> stateDelta %= (^+^ Point2 (-pace)  0    )
                    KeyLetter LetterD      -> stateDelta %= (^+^ Point2   pace   0    )
                    KeyLetter LetterY      -> stateDirection %= not
                    KeyLetter LetterR      -> stateAngle %= normalizeAngle . (^+^ ((speed/30) @@ turn))
                    KeyLetter LetterT      -> stateAngle %= normalizeAngle . (^-^ ((speed/30) @@ turn))
                    KeyArrow ArrowRight    -> whenM (uses stateCurrentTest (< (length tests - 1))) $ stateCurrentTest += 1
                    KeyArrow ArrowLeft     -> whenM (uses stateCurrentTest (> 0)) $ stateCurrentTest -= 1
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
