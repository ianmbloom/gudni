{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Graphics.Gudni.OpenCL.Instances
where

import Graphics.Gudni.Figure
import CLUtil
import CLUtil.KernelArgs
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Interface.DrawTarget(OutputPtr(..), TextureObject(..))
import Graphics.Gudni.OpenCL.GLInterop
import Graphics.Rendering.OpenGL(GLuint)
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Unsafe.Coerce

import Graphics.GL.Core31

import Control.Monad (void)

pileToBuffer :: forall t . (Storable t) => CLContext -> Pile t -> IO CLMem
pileToBuffer context (Pile cursor _ startPtr) =
    let vecSize = cursor * sizeOf (undefined :: t)
    in  clCreateBuffer context [CL_MEM_READ_ONLY, CL_MEM_USE_HOST_PTR] (vecSize, castPtr startPtr)

instance KernelArgs s g w o r => KernelArgs s g w o (Color -> r) where
  prepArg = stoPrepArg

instance {-# OVERLAPPING #-} (KernelArgs s g w o r, Storable t) => KernelArgs s g w o (Pile t -> r) where
    prepArg k arg prep v = prepArg k (arg+1) (addKernelArgument load prep)
        where load cont = cont $ \s sz ->
                            do b <- pileToBuffer (clContext s) v
                               clSetKernelArgSto k arg b
                               return (Just . FreeInput $ void (clReleaseMemObject b), sz)


-- Allow an output that copies directly to an area of
-- host memory that is allocated elsewhere such as a display buffer.
instance {-# OVERLAPPING #-} forall s g w o r a . (Storable a, KernelArgs s g w o r) => KernelArgs s g w o (OutputPtr a -> r) where
  prepArg k arg prep (OutPtr ptr sz) =  prepArg k (arg+1) (addKernelArgument load prep)
      where load cont = cont allocateOutput
            allocateOutput s szs =
              do let m = sizeOf (undefined :: a)
                 b <- clCreateBuffer (clContext s)
                                     [ CL_MEM_WRITE_ONLY
                                     , CL_MEM_USE_HOST_PTR ]
                                     (m*sz, castPtr ptr)
                 clSetKernelArgSto k arg b
                 let clean = FreeInput . void $
                               do ev <- clEnqueueReadBuffer (clQueue s) b False 0 (m*sz) (castPtr ptr) []
                                 --  _ <- clWaitForEvents [ev]
                                  --void $ clReleaseEvent ev
                                  return ()
                 return (Just clean, szs)

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
instance KernelArgs s g w o r => KernelArgs s g w o (Point2 IntSpace -> r) where prepArg = stoPrepArg
instance KernelArgs s g w o r => KernelArgs s g w o (Point2 DisplaySpace -> r) where prepArg = stoPrepArg
