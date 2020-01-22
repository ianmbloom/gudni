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
  ( RasterSpec(..)
  , specMaxTileSize
  , specThreadsPerTile
  , specMaxTilesPerJob
  , specMaxThresholds
  , specMaxLayers
  , Rasterizer(..)
  , rasterClState
  , rasterGenerateThresholdsKernel
  , rasterSortThresholdsKernel
  , rasterRenderThresholdsKernel
  , rasterQueryKernel
  , rasterUseGLInterop
  , rasterSpec
  )
where

import Graphics.Gudni.Figure
import Foreign.C.Types(CUInt, CSize)
import CLUtil
import Control.Lens

data RasterSpec = RasterSpec
    { -- | Determined proper square tile size for this device. Should be a power of two.
      _specMaxTileSize  :: PixelSpace
      -- | The number of threads to execute per tile. This must be >= specMaxTileSize and a power of two.
    , _specThreadsPerTile :: Int
      -- | Determined best number of tiles per kernel call for this device.
    , _specMaxTilesPerJob :: Int
      -- | Determined best maximum number of thresholds per thread for this device.
    , _specMaxThresholds :: Int
      -- | Determined best maximum number of shapes per thread for this device∘
    , _specMaxLayers :: Int
    }
makeLenses ''RasterSpec

data Rasterizer = Rasterizer
  { -- | The OpenCL state
    _rasterClState :: OpenCLState
    -- | The rasterizer kernels.
  , _rasterGenerateThresholdsKernel :: CLKernel
  , _rasterSortThresholdsKernel     :: CLKernel
  , _rasterRenderThresholdsKernel   :: CLKernel
  , _rasterQueryKernel              :: CLKernel
    -- | Flag for if OpenCL-OpenGL interop should be used to render the drawing target.
  , _rasterUseGLInterop :: Bool
  , _rasterSpec :: RasterSpec
  }
makeLenses ''Rasterizer
