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

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Image
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.Interface.TargetFromImage
import Graphics.Gudni.Interface.InterfaceSDL
import Graphics.Gudni.Raster.Class
import Graphics.Gudni.Raster.Serialize

import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Traverse
import Graphics.Gudni.Raster.Fabric.Ray.Class
import Graphics.Gudni.Raster.Fabric.FromLayout
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Fabric.Out

import Graphics.Gudni.Layout

import Control.Lens
import Control.Monad.State
import Control.Monad.Random

data DagHaskellState = DagHaskellState
makeLenses ''DagHaskellState

instance Rasterizer DagHaskellState where
    setupRasterizer = return DagHaskellState
    prepareTarget _ = prepareTargetSDL False
    rasterFrame rasterizer canvasSize pictureMap layout frameCount queries cursor target =
      do (pictureMemoryMap, pixelPile) <- liftIO $ collectPictureMemory pictureMap
         let fabric = prepFabric pictureMemoryMap $ unLayout layout
             limit = realToFrac cROSSsPLITlIMIT
             canvas = sizeToBox . fmap fromIntegral $ canvasSize
         liftIO . putStrLn . render . doc $ fabric
         withSerializedFabric limit 0 (Just canvas) pixelPile fabric $ \storage ->
             do  out <- evalStateT outFabric storage
                 liftIO $ putStrLn "**** outFabric *******************************************"
                 liftIO $ putStrLn $ render out
                 out <- evalStateT simpleOutFabric storage
                 liftIO $ putStrLn "**** simpleOutFabric *******************************************"
                 liftIO $ putStrLn $ render out
                 evalStateT (evalRandT (
                     do let traversePixel loc = traverseFabric limit (fmap fromIntegral cursor) (fmap fromIntegral loc)
                        img <- createImageM traversePixel canvasSize
                        copyImageToTarget img target
                     ) (mkStdGen frameCount)) storage
