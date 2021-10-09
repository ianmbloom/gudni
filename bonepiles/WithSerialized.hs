{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.WithSerialized
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions used by TraverseShapeTree to serialize a scene into data buffers that can be parsed by
-- the rasterizer kernel and building a partitioned tree of tiles.

module Graphics.Gudni.Raster.WithSerialized
  ( withSerializedFabric
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Font

import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Storage
import Graphics.Gudni.Raster.Fabric.FromLayout
import Graphics.Gudni.Raster.Fabric.Pull

import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Storage
import Graphics.Gudni.Raster.ConfineTree.Storage

import Graphics.Gudni.Raster.Serial.Pile

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens
import Data.Traversable

import qualified Data.Map.Strict as M

-- | Function for executing a new ConfineMonad
withSerializedFabric :: ( MonadIO m
                        , IsStyle style
                        , Out style
                        , Show var
                        , Ord var
                        , Out var
                        )
                     => SpaceOf style
                     -> Int
                     -> Maybe (Box (SpaceOf style))
                     -> PixelPile
                     -> Fabric (PicturePass var style)
                     -> (DagStorage (SpaceOf style) -> FontMonad (SpaceOf style) m a)
                     -> FontMonad (SpaceOf style) m a
withSerializedFabric limit decorationLimit mCanvas pixelPile fabric code =
    do  fabricStorage <- initFabricStorage
        treeStorage   <- initTreeStorage
        primTagIdPile <- liftIO newPile
        let initDag = DagStorage
                      { _dagFabricStorage = fabricStorage
                      , _dagTreeStorage   = treeStorage
                      , _dagPrimTagIds    = primTagIdPile
                      , _dagPixelPile     = pixelPile
                      }
        liftIO $ putStrLn "serializeFabric"
        glyphMap <- pullGlyphs fabric
        state' <- execStateT (do glyphTagMap <- mapM (encodeFabric limit decorationLimit M.empty) glyphMap
                                 -- liftIO $ putStrLn $ "glyphTagMap " ++ show glyphTagMap
                                 encodeFabric limit decorationLimit glyphTagMap fabric
                                 return ()
                             ) initDag
        liftIO $ putStrLn "before code"
        result <- code state'
        liftIO $ putStrLn "after code"
        liftIO $
            do freeFabricStorage $ state' ^. dagFabricStorage
               freeTreeStorage   $ state' ^. dagTreeStorage
               liftIO . freePile $ state' ^. dagPrimTagIds
        return result
