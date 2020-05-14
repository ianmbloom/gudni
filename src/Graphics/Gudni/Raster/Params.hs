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

module Graphics.Gudni.Raster.Params
  ( RasterParams(..)
  , rpRasterizer
  , rpSerialState
  , rpPictDataPile
  , rpPointQueries
  , rpBitmapSize
  , rpDrawTarget
  , rpFrameCount
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.ReorderTable
import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.OpenCL.Rasterizer

import Graphics.Gudni.Util.Pile
import Foreign.C.Types(CInt, CUInt, CSize)
import CLUtil
import Control.Lens

data RasterParams token = RasterParams
  { _rpRasterizer      :: Rasterizer
  , _rpSerialState     :: SerialState token SubSpace
  , _rpPictDataPile    :: Pile Word8
  , _rpPointQueries    :: [(PointQueryId, Point2 SubSpace)]
  , _rpBitmapSize      :: Point2 PixelSpace
  , _rpDrawTarget      :: DrawTarget
  , _rpFrameCount      :: Int
  }
makeLenses ''RasterParams
