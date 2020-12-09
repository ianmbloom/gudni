{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Graphics.Gudni.Raster.Dag.OpenCL.Instance
  (
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
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Raster.Dag.OpenCL.Rasterizer
import Graphics.Gudni.Raster.Dag.OpenCL.EmbeddedOpenCLSource
import Graphics.Gudni.Raster.Dag.OpenCL.Setup
import Graphics.Gudni.Raster.Dag.OpenCL.PrepareBuffers
import Graphics.Gudni.Raster.Dag.OpenCL.CallKernels
import CLUtil
import Control.Lens

import Graphics.Gudni.Interface.InterfaceSDL

instance Rasterizer DagOpenCLState where
    setupRasterizer = setupOpenCL False False embeddedOpenCLSource
    prepareTarget rasterizer = prepareTargetSDL (rasterizer ^. dagOpenCLUseGLInterop)
    rasterFrame rasterizer canvasSize pictureMap scene frameCount queries target =
        do (pictureMemoryMap, pixelPile) <- liftIO $ collectPictureMemory pictureMap
           fabric <- sceneToFabric pictureMemoryMap scene
           let limit = realToFrac cROSSsPLITlIMIT
               canvas = sizeToBox . fmap fromIntegral $ canvasSize
           withSerializedFabric limit 0 (Just canvas) pixelPile fabric $ \storage ->
               do let start = storageCodeStart storage
                  liftIO $ runCL (rasterizer ^. dagOpenCLState) $
                      withBuffersInCommon storage $ \bic ->
                          withOutputBuffer canvasSize target $ \ outputBuffer ->
                              runTraverseDagKernelTiles rasterizer
                                                        bic
                                                        start
                                                        canvasSize
                                                        frameCount
                                                        outputBuffer
