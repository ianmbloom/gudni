{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Serialize
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions used by TraverseShapeTree to serialize a scene into data buffers that can be parsed by
-- the rasterizer kernel and building a partitioned tree of tiles.

module Graphics.Gudni.Raster.Serialize
  ( withSerializedFabric
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Fabric.Storage
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.Storage
import Graphics.Gudni.Raster.Serial.Pile
import Control.Monad.IO.Class

import Control.Monad.State
import Control.Lens

-- | Function for executing a new ConfineMonad
withSerializedFabric :: ( MonadIO m
                        , IsStyle style
                        )
                     => SpaceOf style
                     -> Int
                     -> Maybe (Box (SpaceOf style))
                     -> PixelPile
                     -> Fabric (i style)
                     -> (DagStorage (SpaceOf style) ->  m a)
                     -> m a
withSerializedFabric limit decorationLimit mCanvas pixelPile fabric code =
    do  fabricStorage <- initFabricStorage
        treeStorage   <- initTreeStorage
        primTagIdPile <- liftIO newPile
        let initState = DagStorage
                        { _dagFabricStorage = fabricStorage
                        , _dagTreeStorage   = treeStorage
                        , _dagPrimTagIds    = primTagIdPile
                        , _dagPixelPile     = pixelPile
                        }
        liftIO $ putStrLn "serializeFabric"
        state' <- execStateT (serializeFabric limit decorationLimit fabric) initState
        liftIO $ putStrLn "before code"
        result <- code state'
        liftIO $ putStrLn "after code"
        liftIO $
            do freeFabricStorage $ state' ^. dagFabricStorage
               freeTreeStorage   $ state' ^. dagTreeStorage
               liftIO . freePile $ state' ^. dagPrimTagIds
        return result
