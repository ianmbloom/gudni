{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Haskell.Rasterizer
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Haskell Only Rasterizer maintained for reference and debugging purposes.

module Graphics.Gudni.Raster.Haskell.Rasterizer
  ( DagHaskellState(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Image
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Interface.TargetFromImage
import Graphics.Gudni.Interface.InterfaceSDL
import Graphics.Gudni.Raster.Class

import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.FromLayout
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Traverse
import Graphics.Gudni.Raster.Fabric.Ray.Class
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.TextureReference

import CLUtil
import Control.Lens
import Control.Monad.State
import Control.Monad.Random

data DagHaskellState = DagHaskellState
makeLenses ''DagHaskellState

instance Rasterizer DagHaskellState where
    setupRasterizer = return DagHaskellState
    prepareTarget _ = prepareTargetSDL False
    rasterFrame rasterizer canvasSize pictureMap scene frameCount queries _ target =
      do (pictureMemoryMap, pixelPile) <- liftIO $ collectPictureMemory pictureMap
         fabric :: x <- sceneToFabric pictureMemoryMap scene
         let limit = realToFrac cROSSsPLITlIMIT
             canvas = sizeToBox . fmap fromIntegral $ canvasSize
         withSerializedFabric limit 0 (Just canvas) pixelPile fabric $ \storage ->
             evalStateT (evalRandT (
                 do let traversePixel loc = traverseFabric limit (Point2 10 10) (fmap fromIntegral loc)
                    img <- createImageM traversePixel canvasSize
                    copyImageToTarget img target
             ) (mkStdGen frameCount)) storage