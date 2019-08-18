{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.InterfaceSDL
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for interfacing between the rasterizer and SDL2 interface library.

module Graphics.Gudni.Interface.BackendGL
  ( InterfaceState (..)
  , interfaceWindow
  , startInterface
  , closeInterface
  , prepareTarget
  , presentTarget
  , checkInputs
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Class
import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Interface.ScreenMode
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.Util.Debug

import qualified SDL
import qualified SDL.Input.Keyboard as SDLK

import Data.Maybe
import qualified Data.Text as Text

import Control.Monad.State
import Control.Lens
import Control.DeepSeq

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

import CLUtil.State

-- | State information about the host interface library.
data InterfaceStateGL = InterfaceStateGL
  { _interfaceWindow   :: SDL.Window
  , _interfaceRenderer :: SDL.Renderer
  , _interfaceTexture  :: SDL.Texture
  , _interfaceOldSize  :: V2 CInt
  }
makeLenses ''InterfaceState

-- | Convert to an SDL point from a Gudni PixelSpace point.
makeV2 :: Point2 PixelSpace -> V2 CInt
makeV2 p = V2 (fromIntegral . view pX $ p) (fromIntegral . view pY $ p)

-- | Create an SDL texture with a bits per channel RGBA
makeTexture :: a
makeTexture renderer size = SDL.createTexture renderer SDL.ARGB8888 SDL.TextureAccessStreaming size

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

instance GudniInterface InterfaceStateGL where
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
                           , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
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
    prepareTarget =
        do  useGLInterop <- use interfaceGLInterop
            window   <- use interfaceWindow
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
    presentTarget target =
      do  let texture = targetTexture target
          when (isHostBitmapTarget target) $ SDL.unlockTexture texture
          renderer <- use interfaceRenderer
          liftIO $ do  SDL.copy renderer texture Nothing Nothing
                       SDL.present renderer
