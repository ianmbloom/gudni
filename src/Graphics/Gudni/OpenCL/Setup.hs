-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.Setup
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for setting up an OpenCL platform to run the rasterizer kernel.

module Graphics.Gudni.OpenCL.Setup
  ( setupOpenCL
  )
where

import Graphics.Gudni.OpenCL.KernelLibrary
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.Interface.GLInterop

import Control.Parallel.OpenCL
import CLUtil.KernelArgs
import CLUtil


import qualified Data.ByteString.Char8 as BS

instance OpenCLSource BS.ByteString where
  prepSource = BS.unpack

setupOpenCL :: Bool -> Bool -> String -> IO OpenCLKernelLibrary
setupOpenCL enableProfiling useCLGLInterop src =
  do
      -- List all platforms and all devices.
      --queryOpenCL CL_DEVICE_TYPE_ALL
      --let deviceCriteria = deviceNameContains "Iris Pro"  -- select the first Iris Pro GPU
      --let deviceCriteria = deviceNameContains "AMD"       -- select the first AMD GPU
      let deviceCriteria = return . const True -- select the first available GPU
      mState <- if useCLGLInterop
               then Just <$> initFromGL CL_DEVICE_TYPE_ALL -- initialize an OpenCL state for the device that is available for CL-GL-Interop
               else clDeviceSelect CL_DEVICE_TYPE_GPU deviceCriteria -- initialize an OpenCL state for the first GPU that meets the criteria.
      case mState of
        Nothing -> error "could not initialize openCL device."
        Just state ->
          do -- Output all of the device information to the terminal.
             dumpOpenCLDeviceInfo . clDevice $ state
             -- Compiler options for OpenCL compiler.
             let options = [ CLFastRelaxedMath
                           , CLStrictAliasing
                           --, CLWarningIntoError
                           ]
             -- Compile the source.
             putStrLn $ "Starting OpenCL kernel compile"
             program <- loadProgramWOptions options state src
             putStrLn $ "Finished OpenCL kernel compile"
             -- get the rasterizer kernel.
             rasterKernel <- program "multiTileRaster"
             -- Get metadata from the openCL device.
             let device = clDevice state
             computeUnits  <- clGetDeviceMaxComputeUnits       device
             maxGroupSize  <- clGetDeviceMaxWorkGroupSize      device
             localMemSize  <- clGetDeviceLocalMemSize          device
             maxBufferSize <- clGetDeviceMaxConstantBufferSize device
             globalMemSize <- clGetDeviceGlobalMemSize         device
             -- Return a Library constructor with relevant information about the device for the rasterizer.
             return CLLibrary { clState = state
                              , multiTileRasterCL = rasterKernel
                              , clComputeUnits = computeUnits
                              , clMaxGroupSize = maxGroupSize
                              , clLocalMemSize = localMemSize
                              , clMaxConstantBufferSize = maxBufferSize
                              , clGlobalMemSize = globalMemSize
                              , clUseGLInterop = useCLGLInterop
                              }
