{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module ScaffoldTree
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug

import Data.Maybe(listToMaybe, fromMaybe, fromJust)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse

import System.Info

data Tree = Branch Tree Tree | Leaf Int deriving (Show)

data TreeState = TreeState
  { _stateScale       :: SubSpace
  , _stateDelta       :: Point2 SubSpace
  , _stateAngle       :: Angle  SubSpace
  , _statePaused      :: Bool
  , _stateSpeed       :: SubSpace
  , _statePace        :: SubSpace
  , _stateLastTime    :: SimpleTime
  , _stateDirection   :: Bool
  , _statePlayhead    :: SubSpace
  , _stateCursor      :: Point2 PixelSpace
  , _stateStep        :: Int
  , _stateFrameNumber :: Int
  , _stateTree        :: Tree
  } deriving (Show)
makeLenses ''TreeState

initialModel tree =
    TreeState
    { _stateScale       = 30
    , _stateDelta       = Point2 0 0
    , _stateAngle       = 0 @@ deg -- 0.02094 @@ rad -- 0 @@ turn-- quarterTurn
    , _statePaused      = True
    , _stateSpeed       = 0.1
    , _statePace        = 10
    , _stateLastTime    = 0
    , _stateDirection   = True
    , _statePlayhead    = 0
    , _stateCursor      = Point2 63 1376
    , _stateStep        = 69
    , _stateFrameNumber = 0
    , _stateTree        = tree
    }

instance Model TreeState where
    screenSize state = --FullScreen
                       Window $ Point2 1024 900
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
        do  let treeScaffold = scafTree (state ^. stateTree)
            testScene <- scaffoldToSTree treeScaffold
            statusTree <- statusDisplay state "Test Paragraph" (lines status)
            let tree = transformFromState state testScene
                withStatus = if False then overlap [statusTree, tree] else tree
            return $ (Scene (light gray) $ withStatus ^?! unGlyph)
    providePictureMap _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

statusDisplay :: Monad m => TreeState -> String -> [String] -> FontMonad m (Glyph (ShapeTree Int SubSpace))
statusDisplay state testName status =
    tTranslateXY 1800 800 .
    fmap (mapGlyph (tRotate (45 @@ deg))) .
    tTranslate (state ^. stateDelta) .
    tScale 30 .
    fmap (solid (dark red)) .
    paragraph 0 0 AlignMin AlignMin $
    unlines $ testName : status

transformFromState :: TreeState -> Glyph (ShapeTree Int SubSpace) -> Glyph (ShapeTree Int SubSpace)
transformFromState state constructed =
    let sc    = state ^. stateScale
        delta = state ^. stateDelta
        angle = state ^. stateAngle
    in  tTranslate delta .
        mapGlyph (tRotate angle) .
        tScale sc $
        constructed

processInput :: Monad m => Input (Point2 PixelSpace) -> StateT TreeState m ()
processInput input =
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
              Pressed -> stateCursor .= positionInfo
              _ -> return ()
        _ -> return ()

main :: IO ()
main = --silence $
       do putStrLn "Started"
          runApplication (initialModel (Branch (Branch (Leaf 0) (Leaf 1)) (Leaf 2)) :: TreeState)

-- | Translate tree data structure to scaffold.
--scafTree :: Monad m => Tree -> Scaffold m Compound (Glyph (CompoundTree SubSpace))
scafTree (Branch l r) =
  let left  = SLeaf $ Named "left"  $ scafTree l
      right = SLeaf $ Named "right" $ scafTree r
  in  SLeaf $ Named "origin" $
      overlap [ stack AlignMin [left, right]
              , SLeaf $ Build2 (line 0.1) (From ["left" , "origin"]) (Offset zeroPoint)
              , SLeaf $ Build2 (line 0.1) (From ["right", "origin"]) (Offset zeroPoint)
              ]
scafTree (Leaf i) = SLeaf $ Named "origin" $ SLeaf $ Build circle
