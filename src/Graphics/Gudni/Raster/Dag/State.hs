{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module Graphics.Gudni.Raster.Dag.State
   ( DagStorage(..)
   , dagPrimStorage
   , dagFabricStorage
   , dagTreeStorage
   , dagPrimTagIds

   , DagState(..)
   , dagFabricTokenMap
   , dagStorage
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
   , loadPrimS
   , loadTreeS
   )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Build
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Storage
import Graphics.Gudni.Raster.Dag.Primitive.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Storage
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

-- | Constructor for holding the state of serializing substance information from the scene.
data DagState token s = DagState
    { -- | A map from tokens to substance id for later identification of shapes.
      -- The token is any type with an instance of Ord that the client program can use to identify shapes in the scene.
      _dagFabricTokenMap   :: M.Map FabricTagId token
      -- | All the piles of dag information.
    , _dagStorage          :: DagStorage s
    }
makeLenses '' DagState

type DagMonad s m = StateT (DagStorage s) m

type DagConstraints s m = ( MonadIO m
                          , Space s
                          , Storable s
                          )

allocateFabricTagS        :: (DagConstraints s m) =>                                             DagMonad s m FabricTagId
allocateFabricCombineTagS :: (DagConstraints s m) =>                                             DagMonad s m FabricTagId
setFabricS                :: (DagConstraints s m) => FabricTagId -> WithParent (ForStorage s) -> DagMonad s m ()
addFabricS                :: (DagConstraints s m) =>                WithParent (ForStorage s) -> DagMonad s m FabricTagId
storePrimS                :: (DagConstraints s m) => Primitive s                              -> DagMonad s m PrimTagId
addTreeS                  :: (DagConstraints s m) => Slice PrimTagId                          -> DagMonad s m ConfineTreeId
allocateFabricTagS         = overStateT dagFabricStorage allocateFabricTag
allocateFabricCombineTagS  = overStateT dagFabricStorage allocateFabricCombineTag
setFabricS location fabric = overStateT dagFabricStorage $ storeFabric location fabric
addFabricS fabric          = overStateT dagFabricStorage $ addFabric            fabric
storePrimS prim            = overStateT dagPrimStorage   $ storePrim   prim
addTreeS   slice           = do pile <- use dagPrimTagIds
                                overStateT dagTreeStorage $ storeTree loadBoxS loadPrimS slice pile

loadFabricS :: (DagConstraints s m) => FabricTagId   -> DagMonad s m (WithParent (ForStorage s))
loadPrimS   :: (DagConstraints s m) => PrimTagId     -> DagMonad s m (Primitive s)
loadTreeS   :: (DagConstraints s m) => ConfineTreeId -> DagMonad s m (ConfineTree s, DecorateTree s)
loadFabricS fabricTagId = overStateT dagFabricStorage $ loadFabric fabricTagId
loadPrimS   primTagId   = overStateT dagPrimStorage   $ loadPrim   primTagId
loadTreeS   treeId      = overStateT dagTreeStorage   $ loadTree   treeId

loadBoxS :: (DagConstraints s m) => PrimTagId -> DagMonad s m (Box s)
loadBoxS primTagId = boxOf <$> loadPrimS primTagId
