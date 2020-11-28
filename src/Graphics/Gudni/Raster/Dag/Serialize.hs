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

module Graphics.Gudni.Raster.Dag.Serialize
  ( withSerializedFabric
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.FromLayout
import Graphics.Gudni.Layout.Layout
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.FromLayout
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox

import Graphics.Gudni.Raster.Thresholds.Constants
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Storage
import Graphics.Gudni.Raster.Dag.Primitive.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.Serialize.ExtractPrimPass

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.MonadUnique
import Graphics.Gudni.Util.Debug

import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Applicative
import Control.Lens

import Linear.V4

import Foreign.Storable

import qualified Data.Map      as M
import qualified Data.Vector   as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Sequence as S
import Data.Word
import Data.Maybe
import Data.List
import Data.Tuple

-- | Function for executing a new ConfineMonad
withSerializedFabric :: ( MonadIO m
                        , IsStyle style
                        )
                     => SpaceOf style
                     -> Maybe (Box (SpaceOf style))
                     -> PixelPile
                     -> Fabric (PicturePass style)
                     -> (DagStorage (SpaceOf style) -> FabricTagId -> m a)
                     -> m a
withSerializedFabric limit mCanvas pixelPile fabric code =
    do  primStorage   <- initPrimStorage
        fabricStorage <- initFabricStorage
        treeStorage   <- initTreeStorage
        primTagIdPile <- liftIO newPile
        let initState = DagStorage
                        { _dagPrimStorage   = primStorage
                        , _dagFabricStorage = fabricStorage
                        , _dagTreeStorage   = treeStorage
                        , _dagPrimTagIds    = primTagIdPile
                        , _dagPixelPile     = pixelPile
                        }
        liftIO $ putStrLn "serializeFabric"
        (fabricTagId, state') <- evalUniqueT (runStateT (serializeFabric limit mCanvas fabric) initState)
        liftIO $ putStrLn "before code"
        result <- code state' fabricTagId
        liftIO $ putStrLn "after code"
        liftIO $
            do freePrimStorage   $ state' ^. dagPrimStorage
               freeFabricStorage $ state' ^. dagFabricStorage
               freeTreeStorage   $ state' ^. dagTreeStorage
               liftIO . freePile $ state' ^. dagPrimTagIds
        return result

serializeFabric :: ( MonadIO m
                   , IsStyle style
                   )
                => SpaceOf style
                -> Maybe (Box (SpaceOf style))
                -> Fabric (PicturePass style)
                -> DagMonad (SpaceOf style) (UniqueT m) FabricTagId
serializeFabric limit mCanvas fabric =
    do liftIO $ liftIO $ putStrLn $ "===================== Serialize Fabric Start " ++ show (fabricDepth fabric) ++ " ====================="
       f' <- extractPrimPass limit fabric
       liftIO $ liftIO $ putStrLn $ "===================== Serialize Fabric End ======================="
       return f'
