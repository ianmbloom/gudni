{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.OpenCL.Rasterizer
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A constructor for storing an OpenCL state, compiled kernels (just one right now) and other metadata.

module Graphics.Gudni.Raster.OpenCL.Rasterizer
  ( DeviceSpec(..)
  , specMaxTileSize
  , specComputeSize
  , specComputeDepth
  , DagOpenCLState(..)
  , dagOpenCLState
  , dagOpenCLTraverseDagKernel
  , dagOpenCLUseGLInterop
  , dagOpenCLDeviceSpec
  )
where

import Graphics.Gudni.Figure

import CLUtil
import Control.Lens

data DeviceSpec = DeviceSpec
    { -- | Determined proper square tile size for this device. Should be a power of two.
      _specMaxTileSize :: PixelSpace
      -- | The number of threads to execute per tile. This must be >= specMaxTileSize and a power of two.
    , _specComputeSize :: Int
      -- | The compute depth is the adjusted log2 of the threads per tile (see Kernels.cl, function initTileState)
    , _specComputeDepth :: Int
    } deriving (Show)
makeLenses ''DeviceSpec

data DagOpenCLState = DagOpenCLState
  { -- | The OpenCL state
    _dagOpenCLState :: OpenCLState
    -- | The rasterizer dag traversal kernel.
  , _dagOpenCLTraverseDagKernel  :: CLKernel
    -- | Flag for if OpenCL-OpenGL interop should be used to render the drawing target.
  , _dagOpenCLUseGLInterop :: Bool
    -- | Specifications
  , _dagOpenCLDeviceSpec   :: DeviceSpec
  }
makeLenses ''DagOpenCLState
