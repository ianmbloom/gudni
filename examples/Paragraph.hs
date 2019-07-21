{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Paragraph
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Application

import Data.Maybe(listToMaybe, fromMaybe, fromJust)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse

import System.Info

data ParagraphState = ParagraphState
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
  } deriving (Show)
makeLenses ''ParagraphState

initialModel =
    ParagraphState
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
    }

instance Model ParagraphState where
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
        do  para <- paragraph 0.2 0.2 AlignCenter AlignCenter mobyDick
            let testScene = solid black . rack AlignMin $ distributeRack 0 $ [para, tScale 10 circle]
            statusTree <- statusDisplay state "Test Paragraph" (lines status)
            let tree = transformFromState testScene state
                withStatus = if False then overlap [statusTree, tree] else tree
            return (Scene (light gray) $ fromJust $ withStatus ^. unBoxed)
    providePictureData _ = noPictures
    handleOutput state target = do  presentTarget target
                                    return state

statusDisplay :: Monad m => ParagraphState -> String -> [String] -> GlyphMonad m (Boxed (ShapeTree Int SubSpace))
statusDisplay state testName status =
    tTranslateXY 1800 800 .
    fmap (mapBoxed (tRotate (45 @@ deg))) .
    tTranslate (state ^. stateDelta) .
    tScale 30 .
    fmap (solid (dark red)) .
    paragraph 0 0 AlignMin AlignMin $
    unlines $ testName : status

transformFromState :: Boxed (ShapeTree Int SubSpace) -> ParagraphState -> Boxed (ShapeTree Int SubSpace)
transformFromState constructed state =
    let sc    = state ^. stateScale
        delta = state ^. stateDelta
        angle = state ^. stateAngle
    in  tTranslate delta .
        mapBoxed (tRotate angle) .
        tScale sc $
        constructed

processInput :: Monad m => Input (Point2 PixelSpace) -> StateT ParagraphState m ()
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
          runApplication (initialModel :: ParagraphState)

-- | Sample text paragraph.
mobyDick :: String
mobyDick = --"01234\n12345\n23456\n34567\n45678\n"
           "0\n01\n012\n0123\n01234\n"
{-
  =  "Call me Ishmael. Some years ago--never mind how long precisely--having little\n"
  ++ "or no money in my purse, and nothing particular to interest me on shore, I thought\n"
  ++ "I would sail about a little and see the watery part of the world. It is a way I have of\n"
  ++ "driving off the spleen and regulating the circulation. Whenever I find myself growing\n"
  ++ "grim about the mouth; whenever it is a damp, drizzly November in my soul; whenever I\n"
  ++ "find myself involuntarily pausing before coffin warehouses, and bringing up the rear of\n"
  ++ "every funeral I meet; and especially whenever my hypos get such an upper hand of me,\n"
  ++ "that it requires a strong moral principle to prevent me from deliberately stepping into\n"
  ++ "the street, and methodically knocking people's hats off--then, I account it high time\n"
  ++ "to get to sea as soon as I can. This is my substitute for pistol and ball. With a\n"
  ++ "philosophical flourish Cato throws himself upon his sword; I quietly take to the ship.\n"
  ++ "There is nothing surprising in this. If they but knew it, almost all men in their degree,\n"
  ++ "some time or other, cherish very nearly the same feelings towards the ocean with me.\n"
  ++ "There now is your insular city of the Manhattoes, belted round by wharves as Indian isles\n"
  ++ "by coral reefs--commerce surrounds it with her surf. Right and left, the streets take you\n"
  ++ "waterward. Its extreme downtown is the battery, where that noble mole is washed by waves,\n"
  ++ "and cooled by breezes, which a few hours previous were out of sight of land. Look at the\n"
  ++ "crowds of water-gazers there. Circumambulate the city of a dreamy Sabbath afternoon. Go\n"
  ++ "from Corlears Hook to Coenties Slip, and from thence, by Whitehall, northward. What do\n"
  ++ "you see?--Posted like silent sentinels all around the town, stand thousands upon thousands\n"
  ++ "of mortal men fixed in ocean reveries. Some leaning against the spiles; some seated upon the\n"
  ++ "pier-heads; some looking over the bulwarks of ships from China; some high aloft in the rigging,\n"
  ++ "as if striving to get a still better seaward peep. But these are all landsmen; of week days pent\n"
  ++ "up in lath and plaster--tied to counters, nailed to benches, clinched to desks. How then is this?\n"
  ++ "Are the green fields gone? What do they here? But look! here come more crowds, pacing straight for twater,\n"
  ++ "and seemingly bound for a dive. Strange! Nothing will content them but the extremest limit of the land; \n"
  ++ "loitering under the shady lee of yonder warehouses will not suffice. No. They must get just as nigh the\n"
  ++ "water as they possibly can without falling in. And there they stand--miles of them--leagues. Inlanders all,\n"
  ++ "they come from lanes and alleys, streets and avenues--north, east, south, and west. Yet here they all unite.\n"
  ++ "Tell me, does the magnetic virtue of the needles of tcompasses of all those ships attract them thither?\n"
-}
