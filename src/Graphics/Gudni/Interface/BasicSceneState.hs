{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Gudni.Interface.BasicSceneState
  ( BasicSceneState(..)
  , stateScale
  , stateDelta
  , stateAngle
  , statePaused
  , stateSpeed
  , statePace
  , stateLastTime
  , stateDirection
  , statePlayhead
  , stateFrameNumber
  , stateStep

  , transformFromState
  , statusDisplay
  , processInput
  , updateSceneState
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Interface.Time

--import Data.Maybe(listToMaybe, fromMaybe, fromJust)
import Control.Lens
--import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse

import System.Info

data BasicSceneState = BasicSceneState
  { _stateScale       :: SubSpace
  , _stateDelta       :: Point2 SubSpace
  , _stateAngle       :: Angle  SubSpace
  , _statePaused      :: Bool
  , _stateSpeed       :: SubSpace
  , _statePace        :: SubSpace
  , _stateLastTime    :: SimpleTime
  , _stateDirection   :: Bool
  , _statePlayhead    :: SubSpace
  , _stateFrameNumber :: Int
  , _stateStep        :: Int
  } deriving (Show)
makeLenses ''BasicSceneState

transformFromState :: (Transformable t, SpaceOf t ~ SubSpace) => BasicSceneState -> t -> t
transformFromState state constructed =
    let sc    = state ^. stateScale
        delta = state ^. stateDelta
        angle = state ^. stateAngle
    in  tTranslate delta .
        tRotate angle .
        tScale sc $
        constructed

statusDisplay :: Monad m => BasicSceneState -> String -> [String] -> FontMonad m (Glyph (ShapeTree Int SubSpace))
statusDisplay state testName status =
    tTranslateXY 1800 800 .
    fmap (mapGlyph (tRotate (45 @@ deg))) .
    tTranslate (state ^. stateDelta) .
    tScale 30 .
    fmap (solid (dark red)) .
    paragraph 0 0 AlignMin AlignMin $
    unlines $ testName : status

instance HandlesInput BasicSceneState where
    processInput input =
        execState $
        case input of
            (InputKey Pressed _ inputKeyboard) ->
                do  speed <- use stateSpeed
                    pace  <- use statePace
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
                        KeySymbol SymbolComma  -> whenM (uses stateStep (> 0 {-arbitrary-})) $ stateStep -= 1
                        KeySymbol SymbolPeriod -> whenM (uses stateStep (< 1000)) $ stateStep += 1
                        KeySymbol SymbolRightBracket -> stateScale *=  1.1
                        KeySymbol SymbolLeftBracket  -> stateScale //= 1.1
                        _                   -> return ()
            (InputMouse detection modifier clicks positionInfo) ->
                case detection of
                  _ -> return ()
            _ -> return ()

updateSceneState :: Int
                 -> SimpleTime
                 -> BasicSceneState
                 -> BasicSceneState
updateSceneState frame elapsedTime =
    execState $
    do  lastTime <- use stateLastTime
        stateFrameNumber .= frame
        stateLastTime .= elapsedTime
        speed <- use stateSpeed
        whenM(not <$> use statePaused) $
            do  direction <- use stateDirection
                let f = if direction then (+) else (-)
                    timeDelta = elapsedTime - lastTime
                    dt = realToFrac timeDelta * realToFrac speed
                statePlayhead %= (`f` dt)
