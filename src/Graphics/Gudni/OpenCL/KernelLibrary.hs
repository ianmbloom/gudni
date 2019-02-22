module Graphics.Gudni.OpenCL.KernelLibrary where

--import Control.Parallel.OpenCL
import Foreign.C.Types(CUInt, CSize)
import CLUtil

data OpenCLKernelLibrary = CLLibrary
  { clState :: OpenCLState
  , multiTileRasterCL :: CLKernel
  , fillBackgroundCL :: CLKernel
  , checkContinuationCL :: CLKernel
  , clComputeUnits  :: CUInt
  , clMaxGroupSize  :: CSize
  , clLocalMemSize  :: CLulong
  , clMaxConstantBufferSize :: CLulong
  , clUseGLInterop :: Bool
  , clGlobalMemSize :: CLulong
  }

geoMemoryLimit :: OpenCLKernelLibrary -> Int
geoMemoryLimit library = fromIntegral $ clGlobalMemSize library --clMaxConstantBufferSize library

groupSizeLimit :: OpenCLKernelLibrary -> CSize
groupSizeLimit = clMaxGroupSize
