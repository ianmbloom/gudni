{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  , stateRepMode
  , stateRepDk
  , stateCursor
  , transformFromState
  , simpleTransformFromState
  , statusDisplay
  , updateSceneState
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Interface.Time
import Graphics.Gudni.Draw

import Control.Lens
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
  , _stateRepMode     :: Bool
  , _stateRepDk       :: Bool
  , _stateCursor      :: Point2 SubSpace
  } deriving (Show)
makeLenses ''BasicSceneState

simpleTransformFromState :: (SimpleTransformable t, SpaceOf t ~ SubSpace) => BasicSceneState -> t -> t
simpleTransformFromState state constructed =
    let sc    = state ^. stateScale
        delta = state ^. stateDelta
    in  translateBy delta .
        scaleBy sc $
        constructed


transformFromState :: (Transformable t, SpaceOf t ~ SubSpace) => BasicSceneState -> t -> t
transformFromState state constructed =
    let sc    = state ^. stateScale
        delta = state ^. stateDelta
        angle = state ^. stateAngle
    in  translateBy delta .
        rotateBy angle .
        scaleBy sc $
        constructed

statusDisplay :: ( IsStyle style
                 , SpaceOf style~SubSpace
                 )
              => BasicSceneState
              -> String
              -> [String]
              -> Layout style
statusDisplay state testName status =
    translateByXY 1800 800 .
    rotateBy (45 @@ deg) .
    translateBy (state ^. stateDelta) .
    scaleBy 30 .
    withColor (dark red) .
    paragraph $
    unlines $ testName : status

instance HandlesInput token BasicSceneState where
    processInput input state =
        flip execState state $
        case (input ^. inputType) of
             (InputKey Pressed _ inputKeyboard) ->
                 do  speed <- use stateSpeed
                     pace  <- use statePace
                     case inputKeyboard of
                         Key SymbolSpace  -> statePaused %= not
                         Key ArrowUp      -> stateSpeed *=  1.25
                         Key ArrowDown    -> stateSpeed //= 1.25
                         Key LetterW      -> stateDelta %= (^+^ Point2   0    (-pace))
                         Key LetterS      -> stateDelta %= (^+^ Point2   0      pace )
                         Key LetterA      -> stateDelta %= (^+^ Point2 (-pace)  0    )
                         Key LetterD      -> stateDelta %= (^+^ Point2   pace   0    )
                         Key LetterY      -> stateDirection %= not
                         Key LetterR      -> stateAngle %= normalizeAngle . (^+^ ((speed/30) @@ turn))
                         Key LetterT      -> stateAngle %= normalizeAngle . (^-^ ((speed/30) @@ turn))
                         Key SymbolComma  -> whenM (uses stateStep (> 0     )) $ stateStep -= 1
                         Key SymbolPeriod -> whenM (uses stateStep (< 100000)) $ stateStep += 1
                         Key LetterV      -> whenM (uses statePlayhead (> 0)) $ statePlayhead -= 1
                         Key LetterB      -> statePlayhead += 1
                         Key SymbolRightBracket -> stateScale *=  1.1
                         Key SymbolLeftBracket  -> stateScale //= 1.1
                         Key Number1 -> stateScale .= 1
                         Key Number2 -> stateScale .= 2
                         Key Number3 -> stateScale .= 4
                         Key Number4 -> stateScale .= 8
                         Key Number5 -> stateScale .= 16
                         Key Number6 -> stateScale .= 32
                         Key Number0 -> stateScale .= 0.25
                         Key Number9 -> stateScale .= 0.125
                         KeyCommand CommandTab -> stateRepMode %= not
                         Key SymbolBackquote -> stateRepDk %= not
                         _                 -> return ()
             (InputMouse detection modifier clicks positionInfo) ->
                 case detection of
                   Released -> stateCursor .= fmap fromIntegral positionInfo
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
