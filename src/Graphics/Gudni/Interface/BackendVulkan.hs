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

module Graphics.Gudni.Interface.BackendVulkan
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
import Graphics.Gudni.Interface.BackendClass
import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Interface.ScreenMode
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.Util.Debug

import qualified SDL
import qualified SDL.Input.Keyboard as SDLK
import qualified SDL.Video.Vulkan as SDLV

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
data BackendVk = BackendVk
  { _backendWindow   :: SDL.Window
  , _backendInstance :: SDLV.VkInstance
  , _backendSurface  :: SDLV.VkSurfaceKHR
  , _backendOldSize  :: V2 CInt
  }
makeLenses ''BackendVk

-- | Convert to an SDL point from a Gudni PixelSpace point.
makeV2 :: Point2 PixelSpace -> V2 CInt
makeV2 p = V2 (fromIntegral . view pX $ p) (fromIntegral . view pY $ p)

instance GudniBackend BackendVk where
  startInterface screenMode =
     do  SDL.initializeAll
         version <- SDL.version
         putStrLn $ "SDL Version: " ++ show version
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
                          , SDL.windowGraphicsContext = SDL.VulkanContext
                          , SDL.windowPosition     = {-SDL.Absolute displayPosition -} SDL.Absolute . SDL.P $ V2 10 10
                          , SDL.windowResizable    = True
                          , SDL.windowInitialSize  = size
                          , SDL.windowVisible      = True
                          }
         SDLV.vkLoadLibrary Nothing
         SDLV.vkCreateInstance
         window <- SDL.createWindow (Text.pack "Gudni") windowConfig
         extensions <- SDLV.vkGetInstanceExtensions window
         surface  <- SDLV.vkCreateSurface window extensions
         size     <- SDLV.vkGetDrawableSize window

         -- fixes SDL2 bug on OSX Mojave
         --SDL.pumpEvents
         SDL.windowSize window $= size
         return $ BackendVk window surface size
  prepareTarget =
      do  window   <- getWindow
          oldSize  <- use backendOldSize
          size     <- liftIO $ SDLV.vkGetDrawableSize window
          when (size /= oldSize) $
              do  backendOldSize .= size
                  extensions <- liftIO $ SDLV.vkGetInstanceExtensions window
                  surface  <- liftIO $ SDLV.vkCreateSurface window extensions
                  backendSurface .= surface
          surface <- use backendSurface
          return $ DrawTarget size surface
  presentTarget target =
    do  let texture = targetTexture target
        when (isHostBitmapTarget target) $ SDL.unlockTexture texture
        renderer <- use interfaceRenderer
        liftIO $ do  SDL.copy renderer texture Nothing Nothing
                     SDL.present renderer
