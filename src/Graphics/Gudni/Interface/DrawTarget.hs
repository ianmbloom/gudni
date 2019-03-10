-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Interface.DrawTarget
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructors and functions for making various drawing targets for the rasterizer polymorphic.

module Graphics.Gudni.Interface.DrawTarget
  ( TargetBuffer(..)
  , DrawTarget(..)
  , isHostBitmapTarget
  , TextureObject (..)
  , OutputPtr(..)
  )
where

import Linear
import Foreign.C.Types (CInt, CUInt)
import Foreign.Ptr
import qualified SDL

import Graphics.Rendering.OpenGL (TextureObject(..))

-- | Allocated CPU Memory buffer for the output bitmap.
data OutputPtr a = OutPtr (Ptr a) Int deriving (Show)

-- | Target buffer type where the output of the rasterizer is stored.
data TargetBuffer
    = HostBitmapTarget { bitPtr  :: Ptr CUInt}
    | GLTextureTarget  { texName :: TextureObject }

-- | Target buffer with metadata.
data DrawTarget = DrawTarget
    { targetArea    :: V2 CInt
    , targetTexture :: SDL.Texture
    , targetBuffer  :: TargetBuffer
    }

-- | Return true if the DrawTarget is a CPU memory buffer.
isHostBitmapTarget :: DrawTarget -> Bool
isHostBitmapTarget target =
    case targetBuffer target of
        HostBitmapTarget _ -> True
        _                  -> False
