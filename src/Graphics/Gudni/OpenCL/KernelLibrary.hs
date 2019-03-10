-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.Instances
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A constructor for storing an OpenCL state, compiled kernels (just one right now) and other metadata.

module Graphics.Gudni.OpenCL.KernelLibrary where

import Foreign.C.Types(CUInt, CSize)
import CLUtil

data OpenCLKernelLibrary = CLLibrary
  { -- | The OpenCL state
    clState :: OpenCLState
    -- | The main rasterizer kernel.
  , multiTileRasterCL :: CLKernel
    -- | Number of actual SIMD processors on the chip.
  , clComputeUnits  :: CUInt
    -- | Largest compute size for each dimension of a kernel.
  , clMaxGroupSize  :: CSize
    -- | Size of the local memory buffer.
  , clLocalMemSize  :: CLulong
    -- | Largest constant buffer size
  , clMaxConstantBufferSize :: CLulong
    -- | Flag for if OpenCL-OpenGL interop should be used to render the drawing target.
  , clUseGLInterop :: Bool
    -- | Largest global memory buffer size.
  , clGlobalMemSize :: CLulong
  }
