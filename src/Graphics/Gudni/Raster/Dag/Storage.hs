{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module Graphics.Gudni.Raster.Dag.Storage
   ( DagStorage(..)
   , dagPrimStorage
   , dagFabricStorage
   , dagTreeStorage
   , dagPrimTagIds
   , dagPixelPile

   , DagMonad(..)
   , DagConstraints(..)
   , allocateFabricTagS
   , allocateFabricCombineTagS
   , setFabricS
   , addFabricS
   , storePrimS
   , addTreeS
   , loadFabricS
   , loadTransformS
   , loadFilterS
   , loadSubstanceS
   , loadFabricTagS
   , loadFabricCutS
   , loadPrimS
   , loadTreeRootS
   , loadTreeTagS
   , loadDecoTagS
   )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Build
import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Storage
import Graphics.Gudni.Raster.Dag.Primitive.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Transformer
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Filter
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Util.Util

import Control.Monad.State
import Control.Lens
import qualified Data.Map as M
import Foreign.Storable

data DagStorage s = DagStorage
    { -- | Piles storing all of the items (beziers, facets and other primitives) that define the scene.
      _dagPrimStorage      :: PrimStorage s
      -- | Piles storing the fabric structure, substances and descriptions.
    , _dagFabricStorage    :: FabricStorage s
      -- | Piles storing the confineTrees
    , _dagTreeStorage      :: TreeStorage s
      -- | Pile of primitive tags used to build trees.
    , _dagPrimTagIds       :: Pile PrimTagId
      -- | All Accumulated Texture data.
    , _dagPixelPile        :: PixelPile
    }
makeLenses ''DagStorage

type DagMonad s m = StateT (DagStorage s) m

type DagConstraints s m = ( MonadIO m
                          , Space s
                          , Storable s
                          )

allocateFabricTagS        :: (DagConstraints s m) =>                                         DagMonad s m FabricTagId
allocateFabricCombineTagS :: (DagConstraints s m) =>                                         DagMonad s m FabricTagId
setFabricS                :: (DagConstraints s m) => FabricTagId -> Fabric (ForStorage s) -> DagMonad s m ()
addFabricS                :: (DagConstraints s m) =>                Fabric (ForStorage s) -> DagMonad s m FabricTagId
storePrimS                :: (DagConstraints s m) => Primitive s                          -> DagMonad s m PrimTagId
addTreeS                  :: (DagConstraints s m) => Slice PrimTagId                      -> DagMonad s m (Reference (TreeRoot s))
allocateFabricTagS         = overStateT dagFabricStorage allocateFabricTag
allocateFabricCombineTagS  = overStateT dagFabricStorage allocateFabricCombineTag
setFabricS location fabric = overStateT dagFabricStorage $ storeFabric location fabric
addFabricS fabric          = overStateT dagFabricStorage $ addFabric            fabric
storePrimS prim            = overStateT dagPrimStorage   $ storePrim   prim
addTreeS   slice           = do pile <- use dagPrimTagIds
                                overStateT dagTreeStorage $ storeTree loadBoxS loadPrimS slice pile

loadFabricS    :: (DagConstraints s m) => FabricTagId            -> DagMonad s m (Fabric (ForStorage s))
loadTransformS :: (DagConstraints s m) => FabricTag              -> DagMonad s m (FTransformer s)
loadFilterS    :: (DagConstraints s m) => FabricTag              -> DagMonad s m FFilter
loadSubstanceS :: (DagConstraints s m) => FabricTag              -> DagMonad s m (FSubstance (ForStorage s))
loadFabricTagS :: (DagConstraints s m) => FabricTagId            -> DagMonad s m FabricTag
loadFabricCutS :: (DagConstraints s m) => FabricTagId            -> DagMonad s m ShapeId
loadPrimS      :: (DagConstraints s m) => PrimTagId              -> DagMonad s m (Primitive s)
loadBoxS       :: (DagConstraints s m) => PrimTagId              -> DagMonad s m (Box s)
loadTreeRootS  :: (DagConstraints s m) => Reference (TreeRoot s) -> DagMonad s m (TreeRoot s)
loadTreeTagS   :: (DagConstraints s m) => ConfineTagId        s  -> DagMonad s m (ConfineTag s)
loadDecoTagS   :: (DagConstraints s m) => DecoTagId           s  -> DagMonad s m (DecoTag    s)
loadFabricS    fabricTagId = overStateT dagFabricStorage $ loadFabric      fabricTagId
loadTransformS fabricTag   = overStateT dagFabricStorage $ loadTransform   fabricTag
loadFilterS    fabricTag   = overStateT dagFabricStorage $ return $ loadFilter      fabricTag
loadSubstanceS fabricTag   = overStateT (dagFabricStorage . fabricHeapPile) $ loadSubstance fabricTag
loadFabricTagS fabricTagId = overStateT dagFabricStorage $ loadFabricTag      fabricTagId
loadFabricCutS fabricTagId = overStateT dagFabricStorage $ loadFabricShapeCutPoint fabricTagId
loadPrimS      primTagId   = overStateT dagPrimStorage   $ loadPrim  primTagId
loadBoxS       primTagId   = boxOf <$>                     loadPrimS primTagId
loadTreeRootS  treeId      = overStateT dagTreeStorage   $ loadTreeRoot treeId
loadTreeTagS   treeId      = overStateT dagTreeStorage   $ loadTreeTag  treeId
loadDecoTagS   treeId      = overStateT dagTreeStorage   $ loadDecoTag  treeId
