{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.OpenCL.Input
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for abstracting input from the host interface library.

module Graphics.Gudni.Interface.Input
  ( InputDetection (..)
  , InputCommand (..)
  , InputKeyboard (..)
  , InputKey (..)
  , InputWindow (..)
  , InputModifier (..)
  , Input (..)
  , inputType
  , inputToken
  , InputType (..)
  , InputKeyModifier (..)
  , keyModAlt
  , keyModCtrl
  , keyModShift
  , keyModSys
  , noModifier
  , HandlesInput (..)
  )
where

import Graphics.Gudni.Figure
import Control.Lens
import Control.Monad.State


-- | Window Events
data InputWindow
  = WindowClosed
  | WindowResized Int Int
  | WindowLostFocus
  | WindowGainedFocus
  | WindowMouseEntered
  | WindowMouseLeft
  deriving (Eq, Show)

-- | Types of mouse input.
data InputDetection = Pressed | Released | Motion deriving (Eq, Show)

-- | Keyboard Modifiers
data InputKeyModifier = KeyModifier
  { _keyModAlt      :: Bool
  , _keyModCtrl     :: Bool
  , _keyModShift    :: Bool
  , _keyModSys      :: Bool
  } deriving (Eq, Show)
makeLenses ''InputKeyModifier

-- | Make a keystroke with no modifiers.
noModifier = KeyModifier False False False False
-- | Return true if the keyboard modifier contains true .
isShift (KeyModifier _ _ True _) = True
isShift _ = False

-- | Categories for different types of keyboard input.
data InputKeyboard
  = Key InputKey
  | KeyCommand InputCommand
  | KeyMod InputModifier
  | KeyUnsupported
  deriving (Eq, Show)


data InputKey
  = LetterA
  | LetterB
  | LetterC
  | LetterD
  | LetterE
  | LetterF
  | LetterG
  | LetterH
  | LetterI
  | LetterJ
  | LetterK
  | LetterL
  | LetterM
  | LetterN
  | LetterO
  | LetterP
  | LetterQ
  | LetterR
  | LetterS
  | LetterT
  | LetterU
  | LetterV
  | LetterW
  | LetterX
  | LetterY
  | LetterZ
  | Number0
  | Number1
  | Number2
  | Number3
  | Number4
  | Number5
  | Number6
  | Number7
  | Number8
  | Number9
  | SymbolPlus
  | SymbolMinus
  | SymbolComma
  | SymbolPeriod
  | SymbolQuote
  | SymbolSlash
  | SymbolBackSlash
  | SymbolTilde
  | SymbolEqual
  | SymbolDash
  | SymbolSpace
  | SymbolLeftBracket
  | SymbolRightBracket
  | SymbolExclaim
  | SymbolQuoteDbl
  | SymbolHash
  | SymbolPercent
  | SymbolDollar
  | SymbolAmpersand
  | SymbolLeftParen
  | SymbolRightParen
  | SymbolAsterisk
  | SymbolColon
  | SymbolSemiColon
  | SymbolLess
  | SymbolGreater
  | SymbolQuestion
  | SymbolAt
  | SymbolCaret
  | SymbolUnderscore
  | SymbolBackquote
  | SymbolBar
  | ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  deriving (Eq, Show)

-- | Keyboard Modifiers
data InputModifier
  = ModifierLControl
  | ModifierLShift
  | ModifierLAlt
  | ModifierLSystem
  | ModifierRControl
  | ModifierRShift
  | ModifierRAlt
  | ModifierRSystem
  deriving (Eq, Show)

-- | Keyboard Commands
data InputCommand
  = CommandEscape
  | CommandEnter
  | CommandCut
  | CommandCopy
  | CommandPaste
  | CommandUndo
  | CommandReturn
  | CommandBack
  | CommandTab
  | CommandPageUp
  | CommandPageDown
  | CommandEnd
  | CommandHome
  | CommandInsert
  | CommandDelete
  | CommandQuit
  deriving (Eq, Show)

-- | General input types
data InputType
  = InputWindow InputWindow
  | InputText String
  | InputKey   InputDetection InputKeyModifier InputKeyboard
  | InputMouse InputDetection InputKeyModifier Int (Point2 PixelSpace)
  deriving (Eq, Show)

-- | Input
data Input token = Input
  { _inputToken :: Maybe token
  , _inputType  :: InputType
  } deriving (Show)
makeLenses ''Input

-- | Convert an input to a displayable string.
inputToString :: Input token -> String
inputToString input =
  case (input ^. inputType) of
    InputKey _ modifier keyboard ->
      case keyboard of
        Key letter ->
          if isShift modifier
          then
             case letter of
               LetterA -> "A"
               LetterB -> "B"
               LetterC -> "C"
               LetterD -> "D"
               LetterE -> "E"
               LetterF -> "F"
               LetterG -> "G"
               LetterH -> "H"
               LetterI -> "I"
               LetterJ -> "J"
               LetterK -> "K"
               LetterL -> "L"
               LetterM -> "M"
               LetterN -> "N"
               LetterO -> "O"
               LetterP -> "P"
               LetterQ -> "Q"
               LetterR -> "R"
               LetterS -> "S"
               LetterT -> "T"
               LetterU -> "U"
               LetterV -> "V"
               LetterW -> "W"
               LetterX -> "X"
               LetterY -> "Y"
               LetterZ -> "Z"
               Number0 -> ")"
               Number1 -> "!"
               Number2 -> "@"
               Number3 -> "#"
               Number4 -> "$"
               Number5 -> "%"
               Number6 -> "^"
               Number7 -> "&"
               Number8 -> "*"
               Number9 -> "("
               SymbolPlus         -> "+"
               SymbolMinus        -> "_"
               SymbolComma        -> "<"
               SymbolPeriod       -> ">"
               SymbolQuote        -> "\""
               SymbolSlash        -> "?"
               SymbolBackSlash    -> "|"
               SymbolTilde        -> "~"
               SymbolEqual        -> "+"
               SymbolDash         -> "_"
               SymbolSpace        -> " "
               SymbolLeftBracket  -> "{"
               SymbolRightBracket -> "}"
               SymbolExclaim      -> "!"
               SymbolQuoteDbl     -> "\""
               SymbolHash         -> "#"
               SymbolPercent      -> "%"
               SymbolDollar       -> "$"
               SymbolAmpersand    -> "@"
               SymbolLeftParen    -> "("
               SymbolRightParen   -> ")"
               SymbolAsterisk     -> "*"
               SymbolColon        -> ":"
               SymbolSemiColon    -> ";"
               SymbolLess         -> "<"
               SymbolGreater      -> ">"
               SymbolQuestion     -> "?"
               SymbolAt           -> "@"
               SymbolCaret        -> "^"
               SymbolUnderscore   -> "_"
               SymbolBackquote    -> "`"
               SymbolBar          -> "|"
          else
             case letter of
               LetterA -> "a"
               LetterB -> "b"
               LetterC -> "c"
               LetterD -> "d"
               LetterE -> "e"
               LetterF -> "f"
               LetterG -> "g"
               LetterH -> "h"
               LetterI -> "i"
               LetterJ -> "j"
               LetterK -> "k"
               LetterL -> "l"
               LetterM -> "m"
               LetterN -> "n"
               LetterO -> "o"
               LetterP -> "p"
               LetterQ -> "q"
               LetterR -> "r"
               LetterS -> "s"
               LetterT -> "t"
               LetterU -> "u"
               LetterV -> "v"
               LetterW -> "w"
               LetterX -> "x"
               LetterY -> "y"
               LetterZ -> "z"
               Number0 -> "0"
               Number1 -> "1"
               Number2 -> "2"
               Number3 -> "3"
               Number4 -> "4"
               Number5 -> "5"
               Number6 -> "6"
               Number7 -> "7"
               Number8 -> "8"
               Number9 -> "9"
               SymbolPlus         -> "+"
               SymbolMinus        -> "-"
               SymbolComma        -> ","
               SymbolPeriod       -> "."
               SymbolQuote        -> "'"
               SymbolSlash        -> "/"
               SymbolBackSlash    -> "\\"
               SymbolTilde        -> "~"
               SymbolEqual        -> "="
               SymbolDash         -> "-"
               SymbolSpace        -> " "
               SymbolLeftBracket  -> "["
               SymbolRightBracket -> "]"
               SymbolExclaim      -> "!"
               SymbolQuoteDbl     -> "\""
               SymbolHash         -> "#"
               SymbolPercent      -> "%"
               SymbolDollar       -> "$"
               SymbolAmpersand    -> "@"
               SymbolLeftParen    -> "("
               SymbolRightParen   -> ")"
               SymbolAsterisk     -> "*"
               SymbolColon        -> ":"
               SymbolSemiColon    -> ";"
               SymbolLess         -> "<"
               SymbolGreater      -> ">"
               SymbolQuestion     -> "?"
               SymbolAt           -> "@"
               SymbolCaret        -> "^"
               SymbolUnderscore   -> "_"
               SymbolBackquote    -> "`"
               SymbolBar          -> "|"

class HandlesInput token s where
    -- | Do something with incoming input before updating
    processInput :: Input token -> s -> s
