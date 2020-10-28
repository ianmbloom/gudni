{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.OpenCL.Instances
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Instances for extending the types of kernel arguments that can be sent to an OpenCL kernel
-- by adding instances of CLUtil KernelArgs.

module Graphics.Gudni.Raster.OpenCL.Instances
where

import Graphics.Gudni.Figure
import CLUtil
import CLUtil.KernelArgs
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Interface.DrawTarget(TextureObject(..))
import Graphics.Gudni.Interface.GLInterop
import Graphics.Rendering.OpenGL(GLuint)
import Graphics.Gudni.Raster.Thresholds.TileTree
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Unsafe.Coerce

import Graphics.GL.Core31

import Control.Monad (void)

-- | Convert a pile to an OpenCL memory buffer.
pileToBuffer :: forall t . (Storable t) => CLContext -> Pile t -> IO (CLBuffer t)
pileToBuffer context (Pile cursor _ startPtr) =
    let vecSize = fromIntegral cursor * sizeOf (undefined :: t)
        adjustedVecSize = max 1 vecSize -- OpenCL will reject a memory buffer with size 0 so the minimum size is 1.
    in  CLBuffer vecSize <$> clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (adjustedVecSize, castPtr startPtr)


instance KernelArgs s g w o r => KernelArgs s g w o (Tile -> r) where
  prepArg = stoPrepArg

instance KernelArgs s g w o r => KernelArgs s g w o (Slice x -> r) where
  prepArg = stoPrepArg

-- | Make a color into an individual kernel argument.
instance KernelArgs s g w o r => KernelArgs s g w o (Color SubSpace -> r) where
  prepArg = stoPrepArg

-- | Make a pile of Storables in a OpenCL memory buffer.
instance {-# OVERLAPPING #-} (KernelArgs s g w o r, Storable t) => KernelArgs s g w o (Pile t -> r) where
    prepArg k arg prep v = prepArg k (arg+1) (addKernelArgument load prep)
        where load cont = cont $ \s sz ->
                            do b <- pileToBuffer (clContext s) v
                               clSetKernelArgSto k arg b
                               return (Just . FreeInput $ void (clReleaseMemObject $ bufferObject b), sz)

-- | Make an OpenGL texture into a kernel argument.
instance {-# OVERLAPS #-} (KernelArgs s g w o r) => KernelArgs s g w o (TextureObject -> r) where
  prepArg k arg prep tex = prepArg k (arg+1) (addKernelArgument load prep)
      where t = unsafeCoerce tex :: GLuint
            load cont = cont allocateOutput
            allocateOutput s szs =
                        do  (w, h) <- glGetTextureSize tex
                            image <- CLImage (fromIntegral w, fromIntegral h, 1) <$>
                                     clCreateFromGLTexture2D (clContext s) [CL_MEM_READ_WRITE] GL_TEXTURE_2D (0::CInt) t
                            clSetKernelArgSto k arg image
                            return (Just . FreeInput $ void (clReleaseMemObject . imageObject $ image), szs)

-- * Convenient KernelArg instances.
instance KernelArgs s g w o r => KernelArgs s g w o (Bool    -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CChar   -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CSChar  -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CUChar  -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CShort  -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CUShort -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CInt    -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CUInt   -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CLong   -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CULong  -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CSize   -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CLLong  -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (CULLong -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (Point2 CInt -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (Point2 PixelSpace -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (Point2 SubSpace -> r) where prepArg = stoPrepArg
