{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Ray.Class
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A Ray is an abstraction of the inquiry state traversing through the scene dag.
-- The simplest ray is a point.

module Graphics.Gudni.Raster.Fabric.Ray.Class
  ( RayMonad(..)
  , Ray(..)
  , FabricItem(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Primitive.Stack
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.Fabric.Transformer.Type
import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Query
import Graphics.Gudni.Raster.Storage

import Foreign.Storable
import Control.Monad.IO.Class

import System.Random
import Control.Monad.Random

randomVector = error "convolve not implemented"

type RayMonad s m = RandT StdGen (DagMonad s m)

data FabricItem ray = Item ray FabricTagId deriving (Eq)

instance Eq ray => Ord (FabricItem ray) where
   compare (Item rayL tagL) (Item rayR tagR)
         | tagL <  tagR && rayL == rayR = LT
         | tagL == tagR && rayL == rayR = EQ
         | otherwise                    = GT


transformPoint :: Space s => s -> FTransformer s -> Point2 s -> Point2 s
transformPoint limit trans ray =
  case trans of
      FAffine   forward _ -> applyAffine forward ray
      FFacet    facet     -> inverseFacet limit facet ray
      FConvolve scale     -> ray ^+^ randomVector scale

class ( HasSpace r
      , Storable (SpaceOf r)
      , Storable (Bezier (SpaceOf r))
      , Storable (Facet (SpaceOf r))
      , Eq r
      ) => Ray r where
    rayTraverseTree   :: MonadIO m => (SpaceOf r) -> DecoTagId (SpaceOf r) -> ConfineTagId (SpaceOf r) -> r -> [FabricItem r] -> RayMonad (SpaceOf r) m [FabricItem r]
    rayApplyTransform ::              (SpaceOf r) -> FTransformer (SpaceOf r) -> r -> r
    rayApplyFacet     ::              (SpaceOf r) -> Facet        (SpaceOf r) -> r -> r
    rayToPoint        ::                                                         r -> Point2 (SpaceOf r)

instance (Space s, Storable s) => Ray (Point2 s) where
    rayTraverseTree   limit decoId confineId ray stack = lift $ inTree $ queryConfinePoint limit (Item ray) decoId confineId stack (rayToPoint ray)
    rayApplyTransform limit trans ray = transformPoint limit trans ray
    rayApplyFacet     limit facet ray = inverseFacet limit facet ray
    rayToPoint                    ray = ray
