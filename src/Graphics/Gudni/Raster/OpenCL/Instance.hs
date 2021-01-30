{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Graphics.Gudni.Raster.OpenCL.Instance
  (
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget

import Graphics.Gudni.Raster.Class
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.WithSerialized
import Graphics.Gudni.Raster.State
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Storage
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Fabric.Out
import Graphics.Gudni.Raster.Fabric.FromLayout

import Graphics.Gudni.Raster.OpenCL.Rasterizer
import Graphics.Gudni.Raster.OpenCL.EmbeddedOpenCLSource
import Graphics.Gudni.Raster.OpenCL.Setup
import Graphics.Gudni.Raster.OpenCL.PrepareBuffers
import Graphics.Gudni.Raster.OpenCL.CallKernels

import Graphics.Gudni.Layout

import Graphics.Gudni.Interface.InterfaceSDL

import CLUtil
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

instance Rasterizer DagOpenCLState where
    setupRasterizer = setupOpenCL False False embeddedOpenCLSource
    prepareTarget rasterizer = prepareTargetSDL (rasterizer ^. dagOpenCLUseGLInterop)
    rasterFrame rasterizer canvasSize pictureMap layout frameCount queries cursor target =
        do (pictureMemoryMap, pixelPile) <- liftIO $ collectPictureMemory pictureMap
           let fabric = prepFabric pictureMemoryMap $ unLayout layout
               limit = realToFrac cROSSsPLITlIMIT
               canvas = sizeToBox . fmap fromIntegral $ canvasSize
           liftIO . putStrLn . render . doc $ fabric
           withSerializedFabric limit 0 (Just canvas) pixelPile fabric $ \storage ->
               do let start = storageCodeStart storage
                  out <- evalStateT outFabric storage
                  liftIO $ putStrLn "**** outFabric *******************************************"
                  liftIO $ putStrLn $ render out
                  out <- evalStateT simpleOutFabric storage
                  liftIO $ putStrLn "**** simpleOutFabric *******************************************"
                  liftIO $ putStrLn $ render out
                  liftIO $ runCL (rasterizer ^. dagOpenCLState) $
                      withBuffersInCommon storage $ \bic ->
                          withOutputBuffer canvasSize target $ \ outputBuffer ->
                              runTraverseDagKernelTiles rasterizer
                                                        bic
                                                        start
                                                        canvasSize
                                                        frameCount
                                                        cursor
                                                        outputBuffer
