{-# LANGUAGE TemplateHaskell #-}

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

module Graphics.Gudni.Interface.TargetFromImage
  ( copyImageToTarget
  )
where


import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Substance.Color
import Graphics.Gudni.Image
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Figure.StorableInstances

import qualified Data.Vector.Storable as V
import Control.Monad.IO.Class
import Data.Word
import qualified Data.Foldable as Foldable
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils(copyBytes)
import Control.Lens
import Linear.V4

colorToBGRA8 (Color v4) =
  let (V4 r g b a) = fmap (cFloatToWord8 . realToFrac) v4
  in  [b,g,r,a]

copyImageToTarget :: MonadIO m => FrozenImage (Color SubSpace) -> DrawTarget -> m ()
copyImageToTarget image target =
    case target ^. targetBuffer of
        HostBitmapTarget destPtr ->
            let bgra8Vector = V.concatMap (V.fromList . colorToBGRA8) $ (image ^. frImageVector)
                sourceSize = V.length bgra8Vector
                sourceSizeInBytes = sourceSize * sizeOf (undefined :: Word8)
            in  liftIO $ do V.unsafeWith bgra8Vector $ \sourcePtr ->
                                 do copyBytes (castPtr destPtr) sourcePtr sourceSizeInBytes
        GLTextureTarget _ -> error "cannot copy image to GLTextureTarget"
