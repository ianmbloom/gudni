{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}

module Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
  ( RayMonad(..)
  , Ray(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Transformer
import Graphics.Gudni.Raster.Dag.ConfineTree.QueryStorage
import Graphics.Gudni.Raster.Dag.Storage

import Foreign.Storable
import Control.Monad.IO.Class

import System.Random
import Control.Monad.Random

randomVector = error "convolve not implemented"

type RayMonad s m = RandT StdGen (DagMonad s m)

transformPoint :: Space s => s -> FTransformer s -> Point2 s -> Point2 s
transformPoint limit trans ray =
  case trans of
      FAffine   forward _ -> applyAffine forward ray
      FFacet    facet     -> inverseFacet limit facet ray
      FFilter   filt      -> ray
      FConvolve scale     -> ray ^+^ randomVector scale

class ( HasSpace r
      , Storable (SpaceOf r)
      , Storable (Bezier (SpaceOf r))
      , Storable (Facet (SpaceOf r))
      ) => Ray r where
    rayToPoint        ::                                                         r -> Point2 (SpaceOf r)
    rayTraverseTree   :: MonadIO m => (SpaceOf r) -> TreeRoot     (SpaceOf r) -> r -> RayMonad (SpaceOf r) m ShapeStack
    rayApplyTransform ::              (SpaceOf r) -> FTransformer (SpaceOf r) -> r -> r
    rayApplyFacet     ::              (SpaceOf r) -> Facet        (SpaceOf r) -> r -> r

instance (Space s, Storable s) => Ray (Point2 s) where
    rayToPoint                    ray = ray
    rayTraverseTree   limit root  ray = lift $ queryConfineTagPoint limit root (rayToPoint ray)
    rayApplyTransform limit trans ray = transformPoint limit trans ray
    rayApplyFacet     limit facet ray = inverseFacet limit facet ray
