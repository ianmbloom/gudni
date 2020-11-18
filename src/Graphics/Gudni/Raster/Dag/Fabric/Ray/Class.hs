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

transformPoint :: Space s => FTransformer s -> Point2 s -> Point2 s
transformPoint trans ray =
  case trans of
      FAffine   forward _ -> applyAffine forward ray
      FFacet    facet     -> inverseFacet  facet  ray
      FFilter   filt      -> ray
      FConvolve scale     -> ray ^+^ randomVector scale

class ( HasSpace r
      , Storable (SpaceOf r)
      , Storable (Bezier (SpaceOf r))
      , Storable (Facet (SpaceOf r))
      ) => Ray r where
    rayToPoint    :: r -> Point2 (SpaceOf r)
    overTree      :: MonadIO m => ConfineTagId (SpaceOf r) -> DecoTagId (SpaceOf r) -> r -> RayMonad (SpaceOf r) m ShapeStack
    overTransform :: FTransformer (SpaceOf r) -> r -> r
    overFacet     :: Facet (SpaceOf r) -> r -> r

instance (Space s, Storable s) => Ray (Point2 s) where
    rayToPoint                ray = ray
    overTree      cTree dTree ray = lift $ queryConfineTagPoint cTree dTree (rayToPoint ray)
    overTransform trans       ray = transformPoint trans ray
    overFacet     facet       ray = inverseFacet facet ray
