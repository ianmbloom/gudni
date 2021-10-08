{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Interface.GLInterop
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for using GLTexture objects as an OutputBuffer for the rasterizer.

module Graphics.Gudni.Interface.GLInterop
    ( initFromGL
    , glGetTextureSize
--    , bufferFromGL
--    , imageFromGL2D
--    , withGLObjects
--    , withGLObjectsT
    )
where

import Control.Monad.Trans.Class
import Control.Parallel.OpenCL
import Control.Parallel.OpenCL.Context(CLContextProperty(..))
import Control.Monad (void, forM)

import CLUtil
import CLUtil.CL
import CLUtil.State
import CLUtil.KernelArgs

import Graphics.Gudni.Raster.OpenCL.DeviceQuery
import Control.Parallel.OpenCL

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array(withArray)
import Foreign.Ptr (nullPtr, castPtr, Ptr)
import Foreign.Storable (Storable(peek, sizeOf))
import Foreign.C.Types (CInt(..), CUInt(..), CSize(..))
import Foreign.C.String

import Graphics.Rendering.OpenGL
import Graphics.GL.Core31
import Unsafe.Coerce

import Debug.Trace

import System.Info

import Data.List

foreign import ccall "cl.h clGetExtensionFunctionAddress"
    raw_clGetExtensionFunctionAddress :: CString -> IO (Ptr ())

-- -----------------------------------------------------------------------------
-- Mac OS X (CGL API)

foreign import ccall "CGLGetCurrentContext"
    cGLGetCurrentContext :: IO (Ptr ())

foreign import ccall "CGLGetShareGroup"
    cGLGetShareGroup :: Ptr () -> IO (Ptr ())

-- -----------------------------------------------------------------------------
-- XWindows (GLX API) -------
-- https://en.wikipedia.org/wiki/GLX
foreign import ccall "glXGetCurrentContext"
    glXGetCurrentContext :: IO (Ptr ())

foreign import ccall "glXGetCurrentDisplay"
    glXGetCurrentDisplay :: IO (Ptr ())

-- -----------------------------------------------------------------------------
-- Microsoft Windows (WGL API) ----
-- obtain a handle to the current OpenGL rendering context of the calling thread.
-- https://msdn.microsoft.com/en-us/library/dd374383(v=vs.85).aspx
data HGLRC

foreign import ccall "wingdi.h wglGetCurrentContext"
    wglGetCurrentContext :: IO (Ptr HGLRC)

-- get current device context
-- see https://msdn.microsoft.com/en-us/library/dd374384(v=vs.85).aspx
data HDC

foreign import ccall "wingdi.h wglGetCurrentDC"
    wglGetCurrentDC :: IO (Ptr HDC)


data GLInteropContextProperty t
    where GLContext :: GLInteropContextProperty HGLRC
          WGLHDC    :: GLInteropContextProperty HDC

glInteropContextToClContext :: (GLInteropContextProperty t) -> CLContextProperty
glInteropContextToClContext GLContext = undefined -- 0x2008
glInteropContextToClContext WGLHDC    = undefined -- 0x200B

--instance ContextProperty GLInteropContextProperty t

type CLGLContextInfo = CLuint

--- clGetGLContextInfoKHR
--- https://www.khronos.org/registry/OpenCL/sdk/1.1/docs/man/xhtml/clGetGLContextInfoKHR.html

foreign import ccall "clGetGLContextInfoKHR"
  raw_clGetGLContextInfoKHR :: Ptr () -> CLuint -> CSize -> Ptr () -> Ptr CSize -> CLint

-- cl_int clGetGLContextInfoKHR (
--    const cl_context_properties *properties,
--  	cl_gl_context_info param_name,
--  	size_t param_value_size,
--  	void *param_value,
--  	size_t *param_value_size_ret)
clGetGLContextInfoKHR :: [CLContextProperty]
                      -> [CLDeviceID]
                      -> (String -> IO ())
                      -> IO CLContext
clGetGLContextInfoKHR props devs f = withArray devs $ \pdevs -> undefined
    -- wrapPError $ \perr -> do
    --     fptr <- wrapContextCallback $ mkContextCallback f
    --     case props of
    --        [] -> raw_clGetGLContextInfoKHR nullPtr cndevs pdevs fptr nullPtr perr
    --        _  -> withArray (packContextProperties props) $ \pprops ->
    --                raw_clGetGLContextInfoKHR pprops cndevs pdevs fptr nullPtr perr

clGetExtensionFunctionAddress :: String -> IO (Ptr ())
clGetExtensionFunctionAddress str = withCString str raw_clGetExtensionFunctionAddress

initFromGL :: CLDeviceType -> IO OpenCLState
initFromGL devType =
  do --putStrLn $ "Device Ptr: " ++ show device
     --putStrLn $ "ShareGroup Ptr: " ++ show shareGroup
     (context, device) <- clCreateSharedContext devType
     queue <- clCreateCommandQueue context device [] --CL_QUEUE_PROFILING_ENABLE | enableProfiling
     return $ OpenCLState { clDevice        = device
                          , clContext       = context
                          , clQueue         = queue
                          }

-- name of the OpenGL sharing extension for the system
cLGLSharingExtension =
  case os of
    "darwin" ->  "cl_APPLE_gl_sharing"
    _        ->  "cl_khr_gl_sharing"

clCreateSharedContext devType = do
    platform <- head <$> clGetPlatformIDs
    device <- head <$> clGetDeviceIDs platform devType
    return undefined

glGetTextureSize tex = do textureBinding Texture2D $= Just tex
                          TextureSize2D w h <- get $ textureSize2D Texture2D 0
                          textureBinding Texture2D $= Nothing
                          flush
                          return (w, h)

{-
imageFromGL2D :: TextureObject -> CL (CLImage n a)
imageFromGL2D tex =
  do  context <- clContext `fmap` ask
      img <- liftIO $
          do  textureBinding Texture2D $= Just tex
              TextureSize2D w h <- get $ textureSize2D Texture2D 0
              textureBinding Texture2D $= Nothing
              flush
              CLImage (fromIntegral w, fromIntegral h, 1) `fmap`
                clCreateFromGLTexture2D context [CL_MEM_READ_WRITE] GL_TEXTURE_2D (0::CInt) t

      _ <- registerCleanup $ clReleaseMemObject (imageObject img) >> return ()
      return img
  where t = unsafeCoerce tex :: GLuint

bufferFromGL :: forall a. Storable a => BufferObject -> CL (CLBuffer a)
bufferFromGL bufferObject =
  do context <- clContext `fmap` ask
     buf <- liftIO $
       do bindBuffer ArrayBuffer $= Just bufferObject
          n <- alloca $ \ptr ->
            do glGetBufferParameteriv GL_ARRAY_BUFFER GL_BUFFER_SIZE ptr
               peek ptr :: IO GLint
          bindBuffer ArrayBuffer $= Nothing
          flush
          CLBuffer (fromIntegral n `quot` sizeOf (undefined::a)) `fmap`
            clCreateFromGLBuffer context [CL_MEM_READ_WRITE] b
     _ <- registerCleanup $ clReleaseMemObject (bufferObject buf) >> return ()
     return buf
  where b = unsafeCoerce bufferObject :: GLuint

withGLObjects :: [CLMem] -> CL r -> CL r
withGLObjects obs m =
  do q <- clQueue `fmap` ask
     liftIO $ clEnqueueAcquireGLObjects q obs [] >>= waitOne
     r <- m
     liftIO $ clEnqueueReleaseGLObjects q obs [] >>= waitOne
     return r

withGLObjectsT :: (MonadTrans t, Monad (t CL)) => [CLMem] -> t CL r -> t CL r
withGLObjectsT obs m =
  do q <- lift $ clQueue `fmap` ask
     lift . liftIO $ clEnqueueAcquireGLObjects q obs [] >>= waitOne
     r <- m
     lift . liftIO $ clEnqueueReleaseGLObjects q obs [] >>= waitOne
     return r
-}


{-
#ifndef __APPLE__
    -- get OpenGL share group    currentContext <- cGLGetCurrentContext
    shareGroup <- cGLGetShareGroup currentContext
    clContext <- clCreateContext [CL_CGL_SHAREGROUP_KHR shareGroup]
                                 [device]
                                 putStrLn
    return (clContext, device)
#else
    platformExtensions <- clGetPlatformInfo platform CL_PLATFORM_EXTENSIONS
    if isInfixOf platformExtensions cLGLSharingExtension
    then do
        extAddress <- clGetExtensionFunctionAddress "clGetGLContextInfoKHR"
        if extAddress /= nullPtr
        then do -- create context properties listing the platform and current OpenGL display
             props <- undefined
             -- lookup current OpenCL device for current OpenGL context
             -- cl_device_id gpu_id;
             let gpu_id = device
             clContext <- clGetGLContextInfoKHR (platformProperty:displayProperties)
                                                 gpu_id
                                                 putStrLn
             if clContext /= nullPtr
             then  do  extDevice <- clGetDeviceExtensions gpu_id
                       if isInfixOf platformExtensions cLGLSharingExtension
                       then
                           return (clContext, device)
                       else
                           error "device does not support clGL interop extension"
             else error "getGLContextInfoKHR failed"
        else error "null extension address"
    else error "no cLGLSharingExtension"
#endif
-}
