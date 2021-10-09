{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Thresholds.OpenCL.InterfaceSDL
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for interfacing between the rasterizer and SDL2 interface library.

module Graphics.Gudni.Interface.InterfaceSDL
  ( InterfaceState (..)
  , interfaceWindow
  , startInterface
  , closeInterface
  , prepareTargetSDL
  , presentTarget
  , checkInputs
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Interface.ScreenMode
import Graphics.Gudni.Util.Debug

import qualified SDL
import qualified SDL.Input.Keyboard as SDLK

import Data.Maybe
import qualified Data.Text as Text

import Control.Monad.State
import Control.Lens

import Foreign (Ptr, castPtr, nullPtr)
import Foreign.C.Types (CInt, CUInt)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Int
import Data.Word

import Linear.Affine
import Linear

import Graphics.GL.Core31
import Graphics.Rendering.OpenGL

-- | State information about the host interface library.
data InterfaceState = InterfaceState
  { _interfaceWindow   :: SDL.Window
  , _interfaceRenderer :: SDL.Renderer
  , _interfaceTexture  :: SDL.Texture
  , _interfaceOldSize  :: V2 CInt
  }
makeLenses ''InterfaceState

-- | Convert to an SDL point from a Gudni PixelSpace point.
makeV2 :: Point2 PixelSpace -> V2 CInt
makeV2 p = V2 (fromIntegral . view pX $ p) (fromIntegral . view pY $ p)

-- | Open a window
startInterface :: ScreenMode -> IO InterfaceState
startInterface screenMode =
  do  SDL.initializeAll
      version <- SDL.version
      --putStrLn $ "SDL Version: " ++ show version
      displaySize <- SDL.displayBoundsSize . head <$> SDL.getDisplays
      displayPosition <- SDL.displayBoundsPosition . head <$> SDL.getDisplays
      let glProfile = SDL.defaultOpenGL
                    { SDL.glProfile = SDL.Core SDL.Normal 3 3
                    }
          size = case screenMode of
                    FullScreen -> displaySize
                    Window windowSize -> makeV2 windowSize
          windowConfig = SDL.WindowConfig
                       { SDL.windowBorder       = True
                       , SDL.windowHighDPI      = True
                       , SDL.windowInputGrabbed = False
                       , SDL.windowMode         = case screenMode  of
                                                    FullScreen -> SDL.FullscreenDesktop
                                                    Window _  -> SDL.Windowed
                       , SDL.windowGraphicsContext = SDL.OpenGLContext glProfile
                       , SDL.windowPosition     = {-SDL.Absolute displayPosition -} SDL.Absolute . SDL.P $ V2 10 10
                       , SDL.windowResizable    = True
                       , SDL.windowInitialSize  = size
                       , SDL.windowVisible      = True
                       }
      window <- SDL.createWindow (Text.pack "Gudni") windowConfig
      --config <- SDL.getWindowConfig window
      --putStrLn $ "Window Config: " ++ show config
      -------------------- Create Output Bitmap ------------
      let rendererConfig = SDL.RendererConfig
                         { SDL.rendererType          = SDL.AcceleratedVSyncRenderer
                         , SDL.rendererTargetTexture = True
                         }
      --drInfo <- SDL.getRenderDriverInfo
      --putStrLn $ "Driver Info: " ++ show drInfo
      SDL.setHintWithPriority SDL.OverridePriority SDL.HintRenderDriver SDL.OpenGLES2
      renderer <- SDL.createRenderer window 0 rendererConfig
      size     <- SDL.glGetDrawableSize window
      texture <- makeTexture renderer size
      -- fixes SDL2 bug on OSX Mojave
      SDL.pumpEvents
      SDL.windowSize window $= size
      return $ InterfaceState window renderer texture size

-- | Create an SDL texture with a bits per channel RGBA
makeTexture renderer size = SDL.createTexture renderer SDL.ARGB8888 SDL.TextureAccessStreaming size

-- | Prepare a draw target based on whether or not GL-CL interop is in use.
prepareTargetSDL :: Bool -> StateT InterfaceState IO DrawTarget
prepareTargetSDL useGLInterop =
    do  window   <- use interfaceWindow
        oldSize  <- use interfaceOldSize
        size     <- liftIO $ SDL.glGetDrawableSize window
        when (size /= oldSize) $
            do  interfaceOldSize .= size
                oldTexture <- use interfaceTexture
                liftIO $ SDL.destroyTexture oldTexture
                renderer <- use interfaceRenderer
                newTexture <- makeTexture renderer size
                interfaceTexture .= newTexture
        texture <- use interfaceTexture
        buffer <- if useGLInterop
                  then prepareGLTextureTarget  texture size
                  else prepareHostBitmapTarget texture size
        return $ DrawTarget size texture buffer

-- | Prepare an OpenGL texture as a target for rendering.
prepareGLTextureTarget :: SDL.Texture -> V2 CInt -> StateT InterfaceState IO TargetBuffer
prepareGLTextureTarget texture size =
      do liftIO $ do  SDL.glBindTexture texture
                      glName <- alloca (\p -> glGetIntegerv GL_TEXTURE_BINDING_2D p >> peek p)
                      SDL.glUnbindTexture texture
                      return $ GLTextureTarget (TextureObject . fromIntegral $ glName)

-- | Prepare a CPU memory buffer as a target for rendering.
prepareHostBitmapTarget :: SDL.Texture -> V2 CInt -> StateT InterfaceState IO TargetBuffer
prepareHostBitmapTarget texture size =
      do (ptr, _) <- liftIO $ SDL.lockTexture texture Nothing
         return $ HostBitmapTarget (castPtr ptr)

-- | Present a rendered frame on the screne.
presentTarget :: DrawTarget -> StateT InterfaceState IO ()
presentTarget target =
  do  let texture = target ^. targetTexture
      when (isHostBitmapTarget target) $ SDL.unlockTexture texture
      renderer <- use interfaceRenderer
      liftIO $ do  SDL.copy renderer texture Nothing Nothing
                   SDL.present renderer

-- | Close out the interface.
closeInterface :: StateT InterfaceState IO ()
closeInterface =
  do window <- use interfaceWindow
     lift $ SDL.destroyWindow window
     lift SDL.quit

-- | Poll the interface for new inputs.
checkInputs :: StateT InterfaceState IO [Input token]
checkInputs =
    do events <- lift SDL.pollEvents
       let inputs = mapMaybe processEvent events
       return inputs

-- | Convert an SDL event to a Gudni Input.
processEvent :: SDL.Event -> Maybe (Input token)
processEvent (SDL.Event _ payload) =
  Input Nothing <$>
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
makeIntPoint x y = makePoint (toAlong Horizontal $ PSpace . fromIntegral $ x) (toAlong Vertical $ PSpace . fromIntegral $ y)

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
    SDL.KeycodeSpace             -> Key SymbolSpace
    SDL.KeycodeExclaim           -> Key SymbolExclaim
    SDL.KeycodeQuoteDbl          -> Key SymbolQuoteDbl
    SDL.KeycodeHash              -> Key SymbolHash
    SDL.KeycodePercent           -> Key SymbolPercent
    SDL.KeycodeDollar            -> Key SymbolDollar
    SDL.KeycodeAmpersand         -> Key SymbolAmpersand
    SDL.KeycodeQuote             -> Key SymbolQuote
    SDL.KeycodeLeftParen         -> Key SymbolLeftParen
    SDL.KeycodeRightParen        -> Key SymbolRightParen
    SDL.KeycodeAsterisk          -> Key SymbolAsterisk
    SDL.KeycodePlus              -> Key SymbolPlus
    SDL.KeycodeComma             -> Key SymbolComma
    SDL.KeycodeMinus             -> Key SymbolMinus
    SDL.KeycodePeriod            -> Key SymbolPeriod
    SDL.KeycodeSlash             -> Key SymbolSlash
    SDL.Keycode0                 -> Key Number0
    SDL.Keycode1                 -> Key Number1
    SDL.Keycode2                 -> Key Number2
    SDL.Keycode3                 -> Key Number3
    SDL.Keycode4                 -> Key Number4
    SDL.Keycode5                 -> Key Number5
    SDL.Keycode6                 -> Key Number6
    SDL.Keycode7                 -> Key Number7
    SDL.Keycode8                 -> Key Number8
    SDL.Keycode9                 -> Key Number9
    SDL.KeycodeColon             -> Key SymbolColon
    SDL.KeycodeSemicolon         -> Key SymbolSemiColon
    SDL.KeycodeLess              -> Key SymbolLess
    SDL.KeycodeEquals            -> Key SymbolEqual
    SDL.KeycodeGreater           -> Key SymbolGreater
    SDL.KeycodeQuestion          -> Key SymbolQuestion
    SDL.KeycodeAt                -> Key SymbolAt
    SDL.KeycodeLeftBracket       -> Key SymbolLeftBracket
    SDL.KeycodeBackslash         -> Key SymbolBackSlash
    SDL.KeycodeRightBracket      -> Key SymbolRightBracket
    SDL.KeycodeCaret             -> Key SymbolCaret
    SDL.KeycodeUnderscore        -> Key SymbolUnderscore
    SDL.KeycodeBackquote         -> Key SymbolBackquote
    SDL.KeycodeA                 -> Key LetterA                      -- The A key
    SDL.KeycodeB                 -> Key LetterB                      -- The B key
    SDL.KeycodeC                 -> Key LetterC                      -- The C key
    SDL.KeycodeD                 -> Key LetterD                      -- The D key
    SDL.KeycodeE                 -> Key LetterE                      -- The E key
    SDL.KeycodeF                 -> Key LetterF                      -- The F key
    SDL.KeycodeG                 -> Key LetterG                      -- The G key
    SDL.KeycodeH                 -> Key LetterH                      -- The H key
    SDL.KeycodeI                 -> Key LetterI                      -- The I key
    SDL.KeycodeJ                 -> Key LetterJ                      -- The J key
    SDL.KeycodeK                 -> Key LetterK                      -- The K key
    SDL.KeycodeL                 -> Key LetterL                      -- The L key
    SDL.KeycodeM                 -> Key LetterM                      -- The M key
    SDL.KeycodeN                 -> Key LetterN                      -- The N key
    SDL.KeycodeO                 -> Key LetterO                      -- The O key
    SDL.KeycodeP                 -> Key LetterP                      -- The P key
    SDL.KeycodeQ                 -> Key LetterQ                      -- The Q key
    SDL.KeycodeR                 -> Key LetterR                      -- The R key
    SDL.KeycodeS                 -> Key LetterS                      -- The S key
    SDL.KeycodeT                 -> Key LetterT                      -- The T key
    SDL.KeycodeU                 -> Key LetterU                      -- The U key
    SDL.KeycodeV                 -> Key LetterV                      -- The V key
    SDL.KeycodeW                 -> Key LetterW                      -- The W key
    SDL.KeycodeX                 -> Key LetterX                      -- The X key
    SDL.KeycodeY                 -> Key LetterY                      -- The Y key
    SDL.KeycodeZ                 -> Key LetterZ                      -- The Z key
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
    SDL.KeycodeRight             -> Key ArrowRight
    SDL.KeycodeLeft              -> Key ArrowLeft
    SDL.KeycodeDown              -> Key ArrowDown
    SDL.KeycodeUp                -> Key ArrowUp
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
    SDL.KeycodeSeparator         -> Key SymbolBar
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
