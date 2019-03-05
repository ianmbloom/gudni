module Graphics.Gudni.OpenCL.Setup
  ( setupOpenCL
  )
where

import Graphics.Gudni.OpenCL.KernelLibrary
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.OpenCL.GLInterop

import Control.Parallel.OpenCL
import CLUtil.KernelArgs
import CLUtil


import qualified Data.ByteString.Char8 as BS

instance OpenCLSource BS.ByteString where
  prepSource = BS.unpack

setupOpenCL :: Bool -> Bool -> String -> IO OpenCLKernelLibrary
setupOpenCL enableProfiling useCLGLInterop src =
  do
      --queryOpenCL CL_DEVICE_TYPE_ALL -- list all available devices
      --let deviceCriteria = deviceNameContains "Iris Pro"  -- select the first Iris Pro GPU
      --let deviceCriteria = deviceNameContains "AMD"       -- select the first AMD GPU
      let deviceCriteria = return . const True -- select the first available GPU
      mState <- if useCLGLInterop
               then Just <$> initFromGL CL_DEVICE_TYPE_ALL
               else clDeviceSelect CL_DEVICE_TYPE_GPU deviceCriteria
      case mState of
        Nothing -> error "could not initialize openCL device."
        Just state ->
          do dumpOpenCLDeviceInfo . clDevice $ state
             let options = [ CLFastRelaxedMath
                           , CLStrictAliasing
                           --, CLWarningIntoError
                           ]
             putStrLn $ "Start compile"
             program <- loadProgramWOptions options state src
             putStrLn $ "Program compiled"
             rasterKernel <- program "multiTileRaster"
             putStrLn $ "OpenCL compile complete."
             let device = clDevice state
             computeUnits  <- clGetDeviceMaxComputeUnits       device
             maxGroupSize  <- clGetDeviceMaxWorkGroupSize      device
             localMemSize  <- clGetDeviceLocalMemSize          device
             maxBufferSize <- clGetDeviceMaxConstantBufferSize device
             globalMemSize <- clGetDeviceGlobalMemSize         device
             return CLLibrary { clState = state
                              , multiTileRasterCL = rasterKernel
                              , clComputeUnits = computeUnits
                              , clMaxGroupSize = maxGroupSize
                              , clLocalMemSize = localMemSize
                              , clMaxConstantBufferSize = maxBufferSize
                              , clGlobalMemSize = globalMemSize
                              , clUseGLInterop = useCLGLInterop
                              }
