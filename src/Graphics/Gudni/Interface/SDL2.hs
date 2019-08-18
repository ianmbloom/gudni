module Graphics.Gudni.Interface.SDL2
  ( closeInterface
  , checkInputs
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.BackendClass
import Graphics.Gudni.Interface.Input

import Control.Monad.State
import Linear.V2
import qualified Data.Text as Text

import qualified SDL
import qualified SDL.Input.Keyboard as SDLK

import Data.Maybe

-- | Close out the interface.
closeInterface :: GudniBackend s => StateT s IO ()
closeInterface =
  do window <- getWindow
     lift $ SDL.destroyWindow window
     lift SDL.quit

-- | Poll the interface for new inputs.
checkInputs :: StateT s IO [Input (Point2 PixelSpace)]
checkInputs =
    do events <- lift SDL.pollEvents
       let inputs = mapMaybe processEvent events
       return inputs

-- | Convert an SDL event to a Gudni Input.
processEvent :: SDL.Event -> Maybe (Input (Point2 PixelSpace))
processEvent (SDL.Event _ payload) =
  case payload of
    SDL.WindowShownEvent  _                                              -> Nothing
    SDL.WindowHiddenEvent _                                              -> Nothing
    SDL.WindowExposedEvent _                                             -> Nothing
    SDL.WindowMovedEvent _                                               -> Nothing
    SDL.WindowResizedEvent (SDL.WindowResizedEventData _ (V2 x y))       -> Nothing
    SDL.WindowSizeChangedEvent _                                         -> Nothing
    SDL.WindowMinimizedEvent _                                           -> Nothing
    SDL.WindowMaximizedEvent _                                           -> Nothing
    SDL.WindowRestoredEvent _                                            -> Nothing
    SDL.WindowGainedMouseFocusEvent _                                    -> Just $ InputWindow WindowGainedFocus
    SDL.WindowLostMouseFocusEvent _                                      -> Just $ InputWindow WindowLostFocus
    SDL.WindowGainedKeyboardFocusEvent _                                 -> Just $ InputWindow WindowGainedFocus
    SDL.WindowLostKeyboardFocusEvent _                                   -> Just $ InputWindow WindowLostFocus
    SDL.WindowClosedEvent _                                              -> Just $ InputWindow WindowClosed
    SDL.KeyboardEvent d                                                  -> processKeyboardEventData d
    SDL.TextInputEvent (SDL.TextInputEventData w text)                   -> Just $ InputText (Text.unpack text)
    SDL.MouseMotionEvent d                                               -> processMouseMotionEventData d
    SDL.MouseButtonEvent d                                               -> processMouseButtonEventData d
    SDL.QuitEvent                                                        -> Just $ InputKey Pressed noModifier $ KeyCommand CommandQuit
    _                                                                    -> Nothing
    {-
        SDL.MouseWheelEvent  !MouseWheelEventData                        ->
        SDL.JoyAxisEvent !JoyAxisEventData                               ->
        SDL.JoyBallEvent !JoyBallEventData                               ->
        SDL.JoyHatEvent !JoyHatEventData                                 ->
        SDL.JoyButtonEvent !JoyButtonEventData                           ->
        SDL.JoyDeviceEvent !JoyDeviceEventData                           ->
        SDL.ControllerAxisEvent !ControllerAxisEventData                 ->
        SDL.ControllerButtonEvent !ControllerButtonEventData             ->
        SDL.ControllerDeviceEvent !ControllerDeviceEventData             ->
        SDL.AudioDeviceEvent !AudioDeviceEventData                       ->
        SDL.UserEvent !UserEventData                                     ->
        SDL.SysWMEvent !SysWMEventData                                   ->
        SDL.TouchFingerEvent !TouchFingerEventData                       ->
        SDL.MultiGestureEvent !MultiGestureEventData                     ->
        SDL.DollarGestureEvent !DollarGestureEventData                   ->
        SDL.DropEvent !DropEventData                                     ->
        SDL.ClipboardUpdateEvent                                         ->
        SDL.TextEditingEvent !TextEditingEventData                       -> Nothing
        SDL.UnknownEvent !UnknownEventData                               ->
        SDL.KeymapChangedEvent                                           -> Nothing
    -}

-- | Make a PixelSpace point for two ints.
makeIntPoint :: Int -> Int -> Point2 PixelSpace
makeIntPoint x y = makePoint (Ortho . PSpace . fromIntegral $ x) (Ortho . PSpace . fromIntegral $ y)

-- | Convert an input motion to a Gudni InputDetection
motionToDetection :: SDL.InputMotion -> InputDetection
motionToDetection SDL.Released = Released
motionToDetection SDL.Pressed  = Pressed

-- | Convert an SDL MouseMotionEvent to a Gudni Input
processMouseMotionEventData (SDL.MouseMotionEventData w _ buttons (Point2 x y) _) =
  Just $ InputMouse Motion noModifier 0 (makeIntPoint (fromIntegral x * 2) (fromIntegral y * 2))

-- | Convert an SDL MouseButtonEvent to a Gudni Input
processMouseButtonEventData (SDL.MouseButtonEventData w motion _ buttons clicks (Point2 x y)) =
  Just $ InputMouse (motionToDetection motion) noModifier (fromIntegral clicks) (makeIntPoint (fromIntegral x * 2) (fromIntegral y * 2))

-- | Convert an SDL Keyboard event to a Gudni Input
processKeyboardEventData (SDL.KeyboardEventData w motion rep keysym) =
  let (modifier, keyInput) = processKeySym keysym
  in  Just $ InputKey (motionToDetection motion) modifier keyInput

-- | Convert an SDL KeySym to a Gudni keyboard input and keyboard modifier.
processKeySym :: SDL.Keysym -> (InputKeyModifier, InputKeyboard)
processKeySym (SDL.Keysym _ keycode modifier) = (processModifier modifier, processKeycode keycode)

-- | Conver an SDL KeyModifier to a Gudni keyboard modifier.
processModifier :: SDL.KeyModifier -> InputKeyModifier
processModifier m =
  KeyModifier { _keyModAlt  = SDLK.keyModifierLeftAlt   m || SDLK.keyModifierRightAlt   m
              , _keyModCtrl = SDLK.keyModifierLeftCtrl  m || SDLK.keyModifierRightCtrl  m
              , _keyModShift= SDLK.keyModifierLeftShift m || SDLK.keyModifierRightShift m
              , _keyModSys  = SDLK.keyModifierLeftGUI   m || SDLK.keyModifierRightGUI   m
              }

-- | Convert a SDL Keycode to a Gudni keyboard input
processKeycode :: SDL.Keycode -> InputKeyboard
processKeycode keycode =
  case keycode of
    SDL.KeycodeUnknown           -> KeyUnsupported
    SDL.KeycodeReturn            -> KeyCommand CommandReturn
    SDL.KeycodeEscape            -> KeyCommand CommandEscape
    SDL.KeycodeBackspace         -> KeyCommand CommandBack
    SDL.KeycodeTab               -> KeyCommand CommandTab
    SDL.KeycodeSpace             -> KeySymbol SymbolSpace
    SDL.KeycodeExclaim           -> KeySymbol SymbolExclaim
    SDL.KeycodeQuoteDbl          -> KeySymbol SymbolQuoteDbl
    SDL.KeycodeHash              -> KeySymbol SymbolHash
    SDL.KeycodePercent           -> KeySymbol SymbolPercent
    SDL.KeycodeDollar            -> KeySymbol SymbolDollar
    SDL.KeycodeAmpersand         -> KeySymbol SymbolAmpersand
    SDL.KeycodeQuote             -> KeySymbol SymbolQuote
    SDL.KeycodeLeftParen         -> KeySymbol SymbolLeftParen
    SDL.KeycodeRightParen        -> KeySymbol SymbolRightParen
    SDL.KeycodeAsterisk          -> KeySymbol SymbolAsterisk
    SDL.KeycodePlus              -> KeySymbol SymbolPlus
    SDL.KeycodeComma             -> KeySymbol SymbolComma
    SDL.KeycodeMinus             -> KeySymbol SymbolMinus
    SDL.KeycodePeriod            -> KeySymbol SymbolPeriod
    SDL.KeycodeSlash             -> KeySymbol SymbolSlash
    SDL.Keycode0                 -> KeyNumber Number0
    SDL.Keycode1                 -> KeyNumber Number1
    SDL.Keycode2                 -> KeyNumber Number2
    SDL.Keycode3                 -> KeyNumber Number3
    SDL.Keycode4                 -> KeyNumber Number4
    SDL.Keycode5                 -> KeyNumber Number5
    SDL.Keycode6                 -> KeyNumber Number6
    SDL.Keycode7                 -> KeyNumber Number7
    SDL.Keycode8                 -> KeyNumber Number8
    SDL.Keycode9                 -> KeyNumber Number9
    SDL.KeycodeColon             -> KeySymbol SymbolColon
    SDL.KeycodeSemicolon         -> KeySymbol SymbolSemiColon
    SDL.KeycodeLess              -> KeySymbol SymbolLess
    SDL.KeycodeEquals            -> KeySymbol SymbolEqual
    SDL.KeycodeGreater           -> KeySymbol SymbolGreater
    SDL.KeycodeQuestion          -> KeySymbol SymbolQuestion
    SDL.KeycodeAt                -> KeySymbol SymbolAt
    SDL.KeycodeLeftBracket       -> KeySymbol SymbolLeftBracket
    SDL.KeycodeBackslash         -> KeySymbol SymbolBackSlash
    SDL.KeycodeRightBracket      -> KeySymbol SymbolRightBracket
    SDL.KeycodeCaret             -> KeySymbol SymbolCaret
    SDL.KeycodeUnderscore        -> KeySymbol SymbolUnderscore
    SDL.KeycodeBackquote         -> KeySymbol SymbolBackquote
    SDL.KeycodeA                 -> KeyLetter LetterA                      -- The A key
    SDL.KeycodeB                 -> KeyLetter LetterB                      -- The B key
    SDL.KeycodeC                 -> KeyLetter LetterC                      -- The C key
    SDL.KeycodeD                 -> KeyLetter LetterD                      -- The D key
    SDL.KeycodeE                 -> KeyLetter LetterE                      -- The E key
    SDL.KeycodeF                 -> KeyLetter LetterF                      -- The F key
    SDL.KeycodeG                 -> KeyLetter LetterG                      -- The G key
    SDL.KeycodeH                 -> KeyLetter LetterH                      -- The H key
    SDL.KeycodeI                 -> KeyLetter LetterI                      -- The I key
    SDL.KeycodeJ                 -> KeyLetter LetterJ                      -- The J key
    SDL.KeycodeK                 -> KeyLetter LetterK                      -- The K key
    SDL.KeycodeL                 -> KeyLetter LetterL                      -- The L key
    SDL.KeycodeM                 -> KeyLetter LetterM                      -- The M key
    SDL.KeycodeN                 -> KeyLetter LetterN                      -- The N key
    SDL.KeycodeO                 -> KeyLetter LetterO                      -- The O key
    SDL.KeycodeP                 -> KeyLetter LetterP                      -- The P key
    SDL.KeycodeQ                 -> KeyLetter LetterQ                      -- The Q key
    SDL.KeycodeR                 -> KeyLetter LetterR                      -- The R key
    SDL.KeycodeS                 -> KeyLetter LetterS                      -- The S key
    SDL.KeycodeT                 -> KeyLetter LetterT                      -- The T key
    SDL.KeycodeU                 -> KeyLetter LetterU                      -- The U key
    SDL.KeycodeV                 -> KeyLetter LetterV                      -- The V key
    SDL.KeycodeW                 -> KeyLetter LetterW                      -- The W key
    SDL.KeycodeX                 -> KeyLetter LetterX                      -- The X key
    SDL.KeycodeY                 -> KeyLetter LetterY                      -- The Y key
    SDL.KeycodeZ                 -> KeyLetter LetterZ                      -- The Z key
    SDL.KeycodeCapsLock          -> KeyUnsupported
    SDL.KeycodeF1                -> KeyUnsupported
    SDL.KeycodeF2                -> KeyUnsupported
    SDL.KeycodeF3                -> KeyUnsupported
    SDL.KeycodeF4                -> KeyUnsupported
    SDL.KeycodeF5                -> KeyUnsupported
    SDL.KeycodeF6                -> KeyUnsupported
    SDL.KeycodeF7                -> KeyUnsupported
    SDL.KeycodeF8                -> KeyUnsupported
    SDL.KeycodeF9                -> KeyUnsupported
    SDL.KeycodeF10               -> KeyUnsupported
    SDL.KeycodeF11               -> KeyUnsupported
    SDL.KeycodeF12               -> KeyUnsupported
    SDL.KeycodePrintScreen       -> KeyUnsupported
    SDL.KeycodeScrollLock        -> KeyUnsupported
    SDL.KeycodePause             -> KeyUnsupported
    SDL.KeycodeInsert            -> KeyCommand CommandInsert
    SDL.KeycodeHome              -> KeyCommand CommandHome
    SDL.KeycodePageUp            -> KeyCommand CommandPageUp
    SDL.KeycodeDelete            -> KeyCommand CommandDelete
    SDL.KeycodeEnd               -> KeyCommand CommandEnd
    SDL.KeycodePageDown          -> KeyCommand CommandPageDown
    SDL.KeycodeRight             -> KeyArrow ArrowRight
    SDL.KeycodeLeft              -> KeyArrow ArrowLeft
    SDL.KeycodeDown              -> KeyArrow ArrowDown
    SDL.KeycodeUp                -> KeyArrow ArrowUp
    SDL.KeycodeNumLockClear      -> KeyUnsupported
    SDL.KeycodeKPDivide          -> KeyUnsupported
    SDL.KeycodeKPMultiply        -> KeyUnsupported
    SDL.KeycodeKPMinus           -> KeyUnsupported
    SDL.KeycodeKPPlus            -> KeyUnsupported
    SDL.KeycodeKPEnter           -> KeyUnsupported
    SDL.KeycodeKP1               -> KeyUnsupported
    SDL.KeycodeKP2               -> KeyUnsupported
    SDL.KeycodeKP3               -> KeyUnsupported
    SDL.KeycodeKP4               -> KeyUnsupported
    SDL.KeycodeKP5               -> KeyUnsupported
    SDL.KeycodeKP6               -> KeyUnsupported
    SDL.KeycodeKP7               -> KeyUnsupported
    SDL.KeycodeKP8               -> KeyUnsupported
    SDL.KeycodeKP9               -> KeyUnsupported
    SDL.KeycodeKP0               -> KeyUnsupported
    SDL.KeycodeKPPeriod          -> KeyUnsupported
    SDL.KeycodeApplication       -> KeyUnsupported
    SDL.KeycodePower             -> KeyUnsupported
    SDL.KeycodeKPEquals          -> KeyUnsupported
    SDL.KeycodeF13               -> KeyUnsupported
    SDL.KeycodeF14               -> KeyUnsupported
    SDL.KeycodeF15               -> KeyUnsupported
    SDL.KeycodeF16               -> KeyUnsupported
    SDL.KeycodeF17               -> KeyUnsupported
    SDL.KeycodeF18               -> KeyUnsupported
    SDL.KeycodeF19               -> KeyUnsupported
    SDL.KeycodeF20               -> KeyUnsupported
    SDL.KeycodeF21               -> KeyUnsupported
    SDL.KeycodeF22               -> KeyUnsupported
    SDL.KeycodeF23               -> KeyUnsupported
    SDL.KeycodeF24               -> KeyUnsupported
    SDL.KeycodeExecute           -> KeyUnsupported
    SDL.KeycodeHelp              -> KeyUnsupported
    SDL.KeycodeMenu              -> KeyUnsupported
    SDL.KeycodeSelect            -> KeyUnsupported
    SDL.KeycodeStop              -> KeyUnsupported
    SDL.KeycodeAgain             -> KeyUnsupported
    SDL.KeycodeUndo              -> KeyCommand CommandUndo
    SDL.KeycodeCut               -> KeyCommand CommandCut
    SDL.KeycodeCopy              -> KeyCommand CommandCopy
    SDL.KeycodePaste             -> KeyCommand CommandPaste
    SDL.KeycodeFind              -> KeyUnsupported
    SDL.KeycodeMute              -> KeyUnsupported
    SDL.KeycodeVolumeUp          -> KeyUnsupported
    SDL.KeycodeVolumeDown        -> KeyUnsupported
    SDL.KeycodeKPComma           -> KeyUnsupported
    SDL.KeycodeKPEqualsAS400     -> KeyUnsupported
    SDL.KeycodeAltErase          -> KeyUnsupported
    SDL.KeycodeSysReq            -> KeyUnsupported
    SDL.KeycodeCancel            -> KeyUnsupported
    SDL.KeycodeClear             -> KeyUnsupported
    SDL.KeycodePrior             -> KeyUnsupported
    SDL.KeycodeReturn2           -> KeyUnsupported
    SDL.KeycodeSeparator         -> KeySymbol SymbolBar
    SDL.KeycodeOut               -> KeyUnsupported
    SDL.KeycodeOper              -> KeyUnsupported
    SDL.KeycodeClearAgain        -> KeyUnsupported
    SDL.KeycodeCrSel             -> KeyUnsupported
    SDL.KeycodeExSel             -> KeyUnsupported
    SDL.KeycodeKP00              -> KeyUnsupported
    SDL.KeycodeKP000             -> KeyUnsupported
    SDL.KeycodeThousandsSeparator-> KeyUnsupported
    SDL.KeycodeDecimalSeparator  -> KeyUnsupported
    SDL.KeycodeCurrencyUnit      -> KeyUnsupported
    SDL.KeycodeCurrencySubunit   -> KeyUnsupported
    SDL.KeycodeKPLeftParen       -> KeyUnsupported
    SDL.KeycodeKPRightParen      -> KeyUnsupported
    SDL.KeycodeKPLeftBrace       -> KeyUnsupported
    SDL.KeycodeKPRightBrace      -> KeyUnsupported
    SDL.KeycodeKPTab             -> KeyUnsupported
    SDL.KeycodeKPBackspace       -> KeyUnsupported
    SDL.KeycodeKPA               -> KeyUnsupported
    SDL.KeycodeKPB               -> KeyUnsupported
    SDL.KeycodeKPC               -> KeyUnsupported
    SDL.KeycodeKPD               -> KeyUnsupported
    SDL.KeycodeKPE               -> KeyUnsupported
    SDL.KeycodeKPF               -> KeyUnsupported
    SDL.KeycodeKPXor             -> KeyUnsupported
    SDL.KeycodeKPPower           -> KeyUnsupported
    SDL.KeycodeKPPercent         -> KeyUnsupported
    SDL.KeycodeKPLess            -> KeyUnsupported
    SDL.KeycodeKPGreater         -> KeyUnsupported
    SDL.KeycodeKPAmpersand       -> KeyUnsupported
    SDL.KeycodeKPDblAmpersand    -> KeyUnsupported
    SDL.KeycodeKPVerticalBar     -> KeyUnsupported
    SDL.KeycodeKPDblVerticalBar  -> KeyUnsupported
    SDL.KeycodeKPColon           -> KeyUnsupported
    SDL.KeycodeKPHash            -> KeyUnsupported
    SDL.KeycodeKPSpace           -> KeyUnsupported
    SDL.KeycodeKPAt              -> KeyUnsupported
    SDL.KeycodeKPExclam          -> KeyUnsupported
    SDL.KeycodeKPMemStore        -> KeyUnsupported
    SDL.KeycodeKPMemRecall       -> KeyUnsupported
    SDL.KeycodeKPMemClear        -> KeyUnsupported
    SDL.KeycodeKPMemAdd          -> KeyUnsupported
    SDL.KeycodeKPMemSubtract     -> KeyUnsupported
    SDL.KeycodeKPMemMultiply     -> KeyUnsupported
    SDL.KeycodeKPMemDivide       -> KeyUnsupported
    SDL.KeycodeKPPlusMinus       -> KeyUnsupported
    SDL.KeycodeKPClear           -> KeyUnsupported
    SDL.KeycodeKPClearEntry      -> KeyUnsupported
    SDL.KeycodeKPBinary          -> KeyUnsupported
    SDL.KeycodeKPOctal           -> KeyUnsupported
    SDL.KeycodeKPDecimal         -> KeyUnsupported
    SDL.KeycodeKPHexadecimal     -> KeyUnsupported
    SDL.KeycodeLCtrl             -> KeyUnsupported
    SDL.KeycodeLShift            -> KeyUnsupported
    SDL.KeycodeLAlt              -> KeyUnsupported
    SDL.KeycodeLGUI              -> KeyUnsupported
    SDL.KeycodeRCtrl             -> KeyUnsupported
    SDL.KeycodeRShift            -> KeyUnsupported
    SDL.KeycodeRAlt              -> KeyUnsupported
    SDL.KeycodeRGUI              -> KeyUnsupported
    SDL.KeycodeMode              -> KeyUnsupported
    SDL.KeycodeAudioNext         -> KeyUnsupported
    SDL.KeycodeAudioPrev         -> KeyUnsupported
    SDL.KeycodeAudioStop         -> KeyUnsupported
    SDL.KeycodeAudioPlay         -> KeyUnsupported
    SDL.KeycodeAudioMute         -> KeyUnsupported
    SDL.KeycodeMediaSelect       -> KeyUnsupported
    SDL.KeycodeWWW               -> KeyUnsupported
    SDL.KeycodeMail              -> KeyUnsupported
    SDL.KeycodeCalculator        -> KeyUnsupported
    SDL.KeycodeComputer          -> KeyUnsupported
    SDL.KeycodeACSearch          -> KeyUnsupported
    SDL.KeycodeACHome            -> KeyUnsupported
    SDL.KeycodeACBack            -> KeyUnsupported
    SDL.KeycodeACForward         -> KeyUnsupported
    SDL.KeycodeACStop            -> KeyUnsupported
    SDL.KeycodeACRefresh         -> KeyUnsupported
    SDL.KeycodeACBookmarks       -> KeyUnsupported
    SDL.KeycodeBrightnessDown    -> KeyUnsupported
    SDL.KeycodeBrightnessUp      -> KeyUnsupported
    SDL.KeycodeDisplaySwitch     -> KeyUnsupported
    SDL.KeycodeKbdIllumToggle    -> KeyUnsupported
    SDL.KeycodeKbdIllumDown      -> KeyUnsupported
    SDL.KeycodeKbdIllumUp        -> KeyUnsupported
    SDL.KeycodeEject             -> KeyUnsupported
    SDL.KeycodeSleep             -> KeyUnsupported
