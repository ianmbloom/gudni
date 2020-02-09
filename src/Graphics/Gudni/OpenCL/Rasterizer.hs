{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.Rasterizer
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A constructor for storing an OpenCL state, compiled kernels (just one right now) and other metadata.

module Graphics.Gudni.OpenCL.Rasterizer
  ( DeviceSpec(..)
  , specMaxTileSize
  , specThreadsPerBlock
  , specComputeDepth
  , specMaxThresholds
  , specMaxLayers
  , specAvailableBlocks
  , specMaxGenerateJobSize
  , specMaxCheckSplitJobSize
  , specMaxSplitJobSize
  , specMaxMergeJobSize
  , specMaxSortJobSize
  , specMaxRenderJobSize
  , Rasterizer(..)
  , rasterClState
  , rasterGenerateThresholdsKernel
  , rasterCheckSplitKernel
  , rasterSplitTileKernel
  , rasterMergeTileKernel
  , rasterSortThresholdsKernel
  , rasterRenderThresholdsKernel
  , rasterPointQueryKernel
  , rasterUseGLInterop
  , rasterDeviceSpec
  , FrameSpec(..)
  , specBitmapSize
  , specDrawTarget
  , specFrameCount
  , RasterParams(..)
  , rpFrameSpec
  , rpRasterizer
  , rpGeometryState
  , rpSubstanceState
  , rpPictDataPile
  , rpPointQueries
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Raster.Serialize

import Graphics.Gudni.Util.Pile
import Foreign.C.Types(CInt, CUInt, CSize)
import CLUtil
import Control.Lens

data DeviceSpec = DeviceSpec
    { -- | Determined proper square tile size for this device. Should be a power of two.
      _specMaxTileSize          :: PixelSpace
      -- | The number of threads to execute per tile. This must be >= specMaxTileSize and a power of two.
    , _specThreadsPerBlock       :: Int
      -- | The compute depth is the adjusted log2 of the threads per tile (see Kernels.cl, function initTileState)
    , _specComputeDepth         :: Int
      -- | Determined best maximum number of thresholds per thread for this device.
    , _specMaxThresholds        :: Int
      -- | Determined best maximum number of shapes per tile for this device∘
    , _specMaxLayers            :: Int
      -- | Determined maximum number of blocks that can be contained in a single buffer.
    , _specAvailableBlocks      :: Int
      -- | Determined maximum number of tiles per threshold generation kernel call for this device.
    , _specMaxGenerateJobSize   :: Int
      -- | Determined maximum number of tiles per check split kernel call for this device.
    , _specMaxCheckSplitJobSize :: Int
      -- | Determined maximum number of tiles per split kernel call for this device.
    , _specMaxSplitJobSize      :: Int
      -- | Determined maximum number of tiles per merge kernel call for this device.
    , _specMaxMergeJobSize      :: Int
      -- | Determined maximum number of tiles per sort kernel call for this device.
    , _specMaxSortJobSize       :: Int
      -- | Determined maximum number of tiles per render kernel call for this device.
    , _specMaxRenderJobSize     :: Int
    }
makeLenses ''DeviceSpec

data Rasterizer = Rasterizer
  { -- | The OpenCL state
    _rasterClState :: OpenCLState
    -- | The rasterizer kernels.
  , _rasterGenerateThresholdsKernel :: CLKernel
  , _rasterCheckSplitKernel         :: CLKernel
  , _rasterSplitTileKernel          :: CLKernel
  , _rasterMergeTileKernel          :: CLKernel
  , _rasterSortThresholdsKernel     :: CLKernel
  , _rasterRenderThresholdsKernel   :: CLKernel
  , _rasterPointQueryKernel         :: CLKernel
    -- | Flag for if OpenCL-OpenGL interop should be used to render the drawing target.
  , _rasterUseGLInterop :: Bool
  , _rasterDeviceSpec :: DeviceSpec
  }
makeLenses ''Rasterizer

data FrameSpec = FrameSpec
  { _specBitmapSize :: Point2 Int
  , _specDrawTarget :: DrawTarget
  , _specFrameCount :: Int
  }
makeLenses ''FrameSpec

data RasterParams token = RasterParams
  { _rpFrameSpec       :: FrameSpec
  , _rpRasterizer      :: Rasterizer
  , _rpGeometryState   :: GeometryState
  , _rpSubstanceState  :: SubstanceState token SubSpace
  , _rpPictDataPile    :: Pile Word8
  , _rpPointQueries    :: [(PointQueryId, Point2 SubSpace)]
  }
makeLenses ''RasterParams
