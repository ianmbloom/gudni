{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Thresholds.OpenCL.RasterState
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A constructor for storing an OpenCL state, compiled kernels (just one right now) and other metadata.

module Graphics.Gudni.Raster.Thresholds.OpenCL.Rasterizer
  ( DeviceSpec(..)
  , specMaxTileSize
  , specMaxStrandSize
  , specColumnsPerBlock
  , specColumnDepth
  , specMaxThresholds
  , specMaxLayers
  , specBlocksPerSection
  , specBlockSectionDepth
  , RasterState(..)
  , rasterClState
  , rasterInitializeSectionKernel
  , rasterInitializeBlockKernel
  , rasterTotalThresholdKernel
  , rasterGenerateThresholdsKernel
  , rasterCollectMergedBlocksKernel
  , rasterCollectRenderBlocksKernel
  , rasterSplitTileKernel
  , rasterCombineSectionKernel
  , rasterMergeBlockKernel
  , rasterSortThresholdsKernel
  , rasterRenderThresholdsKernel
  , rasterPointQueryKernel
  , rasterUseGLInterop
  , rasterDeviceSpec
  , rasterReorderTable
  , rasterRandomField
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget
--import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.Thresholds.ReorderTable
--import Graphics.Gudni.Raster.Thresholds.OpenCL.ProcessBuffers
import Graphics.Gudni.Util.RandomField

import Graphics.Gudni.Raster.Class
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Foreign.C.Types(CInt, CUInt, CSize)
import CLUtil
import Control.Lens


data DeviceSpec = DeviceSpec
    { -- | Determined proper square tile size for this device. Should be a power of two.
      _specMaxTileSize          :: PixelSpace
      -- | Maximum Strand Length
    , _specMaxStrandSize        :: Int
      -- | The number of threads to execute per tile. This must be >= specMaxTileSize and a power of two.
    , _specColumnsPerBlock      :: Int
      -- | The compute depth is the adjusted log2 of the threads per tile (see Kernels.cl, function initTileState)
    , _specColumnDepth          :: Int
      -- | Determined best maximum number of thresholds per thread for this device.
    , _specMaxThresholds        :: Int
      -- | Determined best maximum number of shapes per tile for this device∘
    , _specMaxLayers            :: Int
      -- | Determined maximum number of blocks that can be contained in a single buffer.
    , _specBlocksPerSection     :: Int
      -- | The block buffer depth is the adjusted log2 of the blockBufferSize
    , _specBlockSectionDepth    :: Int
    } deriving (Show)
makeLenses ''DeviceSpec

data RasterState = RasterState
  { -- | The OpenCL state
    _rasterClState :: OpenCLState
    -- | The rasterizer kernels.
  , _rasterInitializeSectionKernel   :: CLKernel
  , _rasterTotalThresholdKernel      :: CLKernel
  , _rasterInitializeBlockKernel     :: CLKernel
  , _rasterGenerateThresholdsKernel  :: CLKernel
  , _rasterCollectMergedBlocksKernel :: CLKernel
  , _rasterCollectRenderBlocksKernel :: CLKernel
  , _rasterSplitTileKernel           :: CLKernel
  , _rasterCombineSectionKernel      :: CLKernel
  , _rasterMergeBlockKernel          :: CLKernel
  , _rasterSortThresholdsKernel      :: CLKernel
  , _rasterRenderThresholdsKernel    :: CLKernel
  , _rasterPointQueryKernel          :: CLKernel
    -- | Flag for if OpenCL-OpenGL interop should be used to render the drawing target.
  , _rasterUseGLInterop :: Bool
  , _rasterDeviceSpec   :: DeviceSpec
  , _rasterReorderTable :: ReorderTable
  , _rasterRandomField  :: RandomField
  }
makeLenses ''RasterState
