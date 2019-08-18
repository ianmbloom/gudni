module Graphics.Gudni.Interface.BackendClass
  ( GudniBackend(..)
  )
where

import Graphics.Gudni.Interface.ScreenMode
import Graphics.Gudni.Interface.DrawTarget

import Control.Monad.State

import qualified SDL

class GudniBackend s where
  startInterface :: ScreenMode -> IO s
  -- | Prepare a target to be rendered to.
  prepareTarget  :: StateT s IO DrawTarget
  -- | Present a rendered frame on the screne.
  presentTarget :: DrawTarget -> StateT s IO ()
  getWindow :: StateT s IO SDL.Window
