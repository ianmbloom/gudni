{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}

module Graphics.Gudni.Raster.Dag.PrimColorQuery
  ( loadPrimColorS
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Query
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.ConfineTree.Query

import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Dag.Serialize
import Graphics.Gudni.Raster.Dag.ConfineTree.Type

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens
import Foreign.Storable

import Linear.Vector
import Linear.Metric
import Data.Kind

loadSubstanceColorS substance =
  case substance of
      FGeometry _ -> return black
      FConst    c -> return $ fromColor zeroPoint c
      FTexture  t -> return (blueish gray)
      FLinear     -> return (yellowish gray)
      FQuadrance  -> return (yellowish gray)

loadFabricColorS :: ( MonadIO m
                    , Space s
                    , Storable s
                    )
                 => FabricTagId
                 -> FabricMonad s m Color
loadFabricColorS fabricTagId =
  do (parent, fabric) <- loadFabricS fabricTagId
     case fabric of
       FCombine ty a b -> loadFabricColorS b
       FTransform trans child -> loadFabricColorS child
       FLeaf substance -> loadSubstanceColorS substance

loadPrimColorS primTagId =
  do prim <- loadPrimS primTagId
     loadFabricColorS (view primFabricTagId prim)
