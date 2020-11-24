{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Params
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Paramaters for rasterizing a scene.

module Graphics.Gudni.Raster.Thresholds.Params
  ( RasterParams(..)
  , rpRasterState
  , rpSerialState
  , rpPixelPile
  , rpPointQueries
  , rpBitmapSize
  , rpDrawTarget
  , rpFrameCount
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Raster.Thresholds.Serialize
import Graphics.Gudni.Raster.Thresholds.ReorderTable
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.Raster.Thresholds.OpenCL.Rasterizer

import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Foreign.C.Types(CInt, CUInt, CSize)
import CLUtil
import Control.Lens

data RasterParams token = RasterParams
  { _rpRasterState  :: RasterState
  , _rpSerialState  :: SerialState token SubSpace
  , _rpPixelPile    :: PixelPile
  , _rpPointQueries :: [PointQuery SubSpace]
  , _rpBitmapSize   :: Point2 PixelSpace
  , _rpDrawTarget   :: DrawTarget
  , _rpFrameCount   :: Int
  }
makeLenses ''RasterParams
