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

data OutputPtr a = OutPtr (Ptr a) Int deriving (Show)

data TargetBuffer
    = HostBitmapTarget { bitPtr  :: Ptr CUInt}
    | GLTextureTarget  { texName :: TextureObject }

data DrawTarget = DrawTarget
    { targetArea    :: V2 CInt
    , targetTexture :: SDL.Texture
    , targetBuffer  :: TargetBuffer
    }

isHostBitmapTarget :: DrawTarget -> Bool
isHostBitmapTarget target =
    case targetBuffer target of
        HostBitmapTarget _ -> True
        _                  -> False
