{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Interface.Input
  ( InputDetection (..)
  , InputArrow (..)
  , InputCommand (..)
  , InputKeyboard (..)
  , InputLetter (..)
  , InputSymbol (..)
  , InputNumber (..)
  , InputWindow (..)
  , InputModifier (..)
  , Input (..)
  , InputKeyModifier (..)
  , keyModAlt
  , keyModCtrl
  , keyModShift
  , keyModSys
  , noModifier
  )
where

import Graphics.Gudni.Figure
import Control.Lens

data InputWindow
  = WindowClosed
  | WindowResized Int Int
  | WindowLostFocus
  | WindowGainedFocus
  | WindowMouseEntered
  | WindowMouseLeft
  deriving (Eq, Show)

data InputDetection = Pressed | Released | Motion deriving (Eq, Show)

data InputArrow = ArrowUp | ArrowDown | ArrowLeft | ArrowRight deriving (Eq, Show)

data InputKeyModifier = KeyModifier
  { _keyModAlt      :: Bool
  , _keyModCtrl     :: Bool
  , _keyModShift    :: Bool
  , _keyModSys      :: Bool
  } deriving (Eq, Show)
makeLenses ''InputKeyModifier

noModifier = KeyModifier False False False False
isShift (KeyModifier _ _ True _) = True
isShift _ = False

data InputKeyboard
  = KeyLetter InputLetter
  | KeyNumber InputNumber
  | KeySymbol InputSymbol
  | KeyArrow InputArrow
  | KeyCommand InputCommand
  | KeyMod InputModifier
  | KeyUnsupported
  deriving (Eq, Show)

data InputLetter
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
  deriving (Eq, Show)

data InputNumber
  = Number0
  | Number1
  | Number2
  | Number3
  | Number4
  | Number5
  | Number6
  | Number7
  | Number8
  | Number9
  deriving (Eq, Show)

data InputSymbol
  = SymbolPlus
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
  deriving (Eq, Show)

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

data Input positionInfo
  = InputWindow InputWindow
  | InputText String
  | InputKey   InputDetection InputKeyModifier InputKeyboard
  | InputMouse InputDetection InputKeyModifier Int positionInfo
  deriving (Eq, Show)

inputToString :: Input a -> String
inputToString input =
  case input of
    InputKey _ modifier keyboard ->
      case keyboard of
        KeyLetter letter ->
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
        KeyNumber number ->
          if isShift modifier
          then
            case number of
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
          else
            case number of
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
        KeySymbol symbol ->
            if isShift modifier
            then
              case symbol of
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
                case symbol of
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
