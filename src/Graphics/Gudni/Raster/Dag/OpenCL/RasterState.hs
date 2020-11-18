{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.OpenCL.RasterState
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A constructor for storing an OpenCL state, compiled kernels (just one right now) and other metadata.

module Graphics.Gudni.Raster.Dag.OpenCL.RasterState
  ( DeviceSpec(..)
  , specMaxTileSize
  , specColumnsPerBlock
  , specColumnDepth
  , RasterState(..)
  , rasterClState
  , rasterTraverseDagKernel
  , rasterUseGLInterop
  , rasterDeviceSpec
  , rasterRandomField
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Util.RandomField

import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

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

data RasterState = RasterState
  { -- | The OpenCL state
    _rasterClState :: OpenCLState
    -- | The rasterizer dag traversal kernel.
  , _rasterTraverseDagKernel  :: CLKernel
    -- | Flag for if OpenCL-OpenGL interop should be used to render the drawing target.
  , _rasterUseGLInterop :: Bool
    -- | Specifications
  , _rasterDeviceSpec   :: DeviceSpec
    -- | Supply of random floats
  , _rasterRandomField  :: RandomField
  }
makeLenses ''RasterState
