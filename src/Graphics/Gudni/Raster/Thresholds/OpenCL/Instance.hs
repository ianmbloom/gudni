module Graphics.Gudni.Raster.Thresholds.OpenCL.Instance
  (
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Layout.FromLayout

import Graphics.Gudni.Raster.Class
import Graphics.Gudni.Raster.Thresholds.Serialize
import Graphics.Gudni.Raster.Thresholds.OpenCL.ProcessBuffers
import Graphics.Gudni.Raster.Thresholds.OpenCL.Rasterizer
import Graphics.Gudni.Raster.Thresholds.Params

import Graphics.Gudni.Raster.Thresholds.OpenCL.Setup
import Graphics.Gudni.Raster.Thresholds.OpenCL.EmbeddedOpenCLSource
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Interface.InterfaceSDL

import Control.Monad.IO.Class
import Control.Lens

instance Rasterizer RasterState where
    setupRasterizer = setupOpenCL False False embeddedOpenCLSource
    prepareTarget rasterizer = prepareTargetSDL (rasterizer ^. rasterUseGLInterop)
    rasterFrame rasterizer canvasSize pictureMap (Scene color layout) frameCount queries target =
        do  tree <- fromLayout layout
            withSerializedScene rasterizer canvasSize pictureMap (Scene color tree) $
                 \ pictDataPile serialState ->
                       do  -- | Create a specification for the current frame.
                           let rasterParams = RasterParams rasterizer
                                                           serialState
                                                           pictDataPile
                                                           queries
                                                           canvasSize
                                                           target
                                                           frameCount
                           liftIO $ putStrLn "===================== rasterStart ====================="
                           queryResults <- liftIO $ runRaster rasterParams
                           liftIO $ putStrLn "===================== rasterDone ====================="
                           --liftIO $ threadDelay 3000000
                           return ()
