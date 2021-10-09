{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.OpenCL.RasterState
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A constructor for storing an OpenCL state, compiled kernels (just one right now) and other metadata.

module Graphics.Gudni.Raster.Class
  ( Rasterizer(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Substance.Picture
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.InterfaceSDL
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.FromLayout
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Type

import Graphics.Gudni.Util.MonadST
import Control.Monad.IO.Class
import Control.Monad.State

class Rasterizer r where
    setupRasterizer :: IO r
    prepareTarget :: r -> StateT InterfaceState IO DrawTarget
    rasterFrame :: ( MonadIO m
                   , MonadST m
                   , IsStyle style
                   , SpaceOf style ~ SubSpace
                   , Out style
                   )
                => r
                -> Point2 PixelSpace
                -> PictureMap
                -> Layout Rgba style
                -> Int
                -> [(PointQuery (SpaceOf style))]
                -> Point2 PixelSpace
                -> DrawTarget
                -> FontMonad (SpaceOf style) m ()
