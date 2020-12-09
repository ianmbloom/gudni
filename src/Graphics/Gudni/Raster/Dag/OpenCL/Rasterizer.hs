{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.OpenCL.Rasterizer
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A constructor for storing an OpenCL state, compiled kernels (just one right now) and other metadata.

module Graphics.Gudni.Raster.Dag.OpenCL.Rasterizer
  ( DeviceSpec(..)
  , specMaxTileSize
  , specColumnsPerBlock
  , specColumnDepth
  , DagOpenCLState(..)
  , dagOpenCLState
  , dagOpenCLTraverseDagKernel
  , dagOpenCLUseGLInterop
  , dagOpenCLDeviceSpec
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget

import Graphics.Gudni.Raster.Class
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Dag.FromLayout
import Graphics.Gudni.Raster.Dag.Serialize
import Graphics.Gudni.Raster.TextureReference

import CLUtil
import Control.Lens

data DeviceSpec = DeviceSpec
    { -- | Determined proper square tile size for this device. Should be a power of two.
      _specMaxTileSize          :: PixelSpace
      -- | The number of threads to execute per tile. This must be >= specMaxTileSize and a power of two.
    , _specColumnsPerBlock      :: Int
      -- | The compute depth is the adjusted log2 of the threads per tile (see Kernels.cl, function initTileState)
    , _specColumnDepth          :: Int
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
