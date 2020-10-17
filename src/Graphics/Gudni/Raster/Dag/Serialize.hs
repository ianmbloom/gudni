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
  , outputDagState
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

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Storage
import Graphics.Gudni.Raster.Dag.Primitive.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.Serialize.ExtractPrimPass

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad
import Control.Monad.State
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
                     => Maybe (Box (SpaceOf style))
                     -> PixelPile
                     -> Fabric (PicturePass style)
                     -> (FabricTagId -> StateT (DagStorage (SpaceOf style)) m a)
                     -> m a
withSerializedFabric mCanvas pixelPile fabric code =
    do  primStorage   <- initPrimStorage
        fabricStorage <- initFabricStorage
        treeStorage   <- initTreeStorage
        primTagIdPile <- liftIO newPile
        let initState = DagState
                        { _dagFabricTokenMap   = M.empty
                        , _dagStorage          =
                              DagStorage
                              { _dagPrimStorage   = primStorage
                              , _dagFabricStorage = fabricStorage
                              , _dagTreeStorage   = treeStorage
                              , _dagPrimTagIds    = primTagIdPile
                              , _dagPixelPile     = pixelPile
                              }
                        }
        (fabricTagId, state') <- runStateT (traverseFabric mCanvas fabric) initState
        result <- evalStateT (code fabricTagId) (state' ^. dagStorage)
        liftIO $
            do freePrimStorage   $ state' ^. dagStorage . dagPrimStorage
               freeFabricStorage $ state' ^. dagStorage . dagFabricStorage
               freeTreeStorage   $ state' ^. dagStorage . dagTreeStorage
               liftIO . freePile $ state' ^. dagStorage . dagPrimTagIds
        return result

findLimit :: FCombineType -> FabricTagId -> FabricTagId -> FabricTagId
findLimit ty aLimit bLimit =
    case ty of
      FMask -> aLimit
      _     -> bLimit

traverseFabric :: ( MonadIO m
                  , IsStyle style
                  )
               => Maybe (Box (SpaceOf style))
               -> Fabric (PicturePass style)
               -> StateT (DagState token (SpaceOf style)) m FabricTagId
traverseFabric mCanvas fabric =
    do liftIO $ putStrLn $ "===================== Serialize Fabric Start ====================="
       f' <- overStateT dagStorage (extractPrimPass fabric)
       liftIO $ putStrLn $ "===================== Serialize Fabric End ======================="
       return f'

outputDagState :: (Show s, Show token, Storable (Facet s))
                   => DagState token s -> IO ()
outputDagState state =
  do  putStrLn $ "dagTokenMap         " ++ (show . view dagFabricTokenMap  $ state)
