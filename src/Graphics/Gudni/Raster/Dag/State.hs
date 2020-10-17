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

   , FabricMonad(..)
   , FabricConstraints(..)
   , allocateFabricTagS
   , setFabricS
   , addFabricS
   , addLimitS
   , storePrimS
   , addTreeS
   , loadFabricS
   , loadLimitS
   , loadPrimS
   , loadTreeS
   , assignPrimParent
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

type FabricMonad s m q = StateT (DagStorage s) m q

type FabricConstraints s m = ( MonadIO m
                             , Space s
                             , Storable s
                             )

allocateFabricTagS :: (FabricConstraints s m) =>                                                        FabricMonad s m FabricTagId
setFabricS         :: (FabricConstraints s m) => FabricTagId -> FabricTagId -> Fabric (ForStorage s) -> FabricMonad s m ()
addFabricS         :: (FabricConstraints s m) => FabricTagId -> Fabric (ForStorage s)                -> FabricMonad s m FabricTagId
addLimitS          :: (FabricConstraints s m) => FabricTagId                                         -> FabricMonad s m (Reference FabricTagId)
storePrimS         :: (FabricConstraints s m) => Primitive s                                         -> FabricMonad s m PrimTagId
addTreeS           :: (FabricConstraints s m) => Slice PrimTagId                                     -> FabricMonad s m ConfineTreeId
allocateFabricTagS =
    do overStateT dagFabricStorage allocateFabricParent
       overStateT dagFabricStorage allocateFabricTag
setFabricS location parent fabric =
    do overStateT dagFabricStorage $ storeFabricParent location parent
       overStateT dagFabricStorage $ storeFabric       location fabric
addFabricS parent fabric =
    do tagId <- allocateFabricTagS
       setFabricS tagId parent fabric
       return tagId
addLimitS limit = overStateT dagFabricStorage $ storeLimit  limit
storePrimS  prim  = overStateT dagPrimStorage   $ storePrim   prim
addTreeS    slice = do pile <- use dagPrimTagIds
                       overStateT dagTreeStorage $ storeTree loadBoxS loadPrimS slice pile

loadFabricS :: (FabricConstraints s m) => FabricTagId   -> FabricMonad s m (FabricTagId, Fabric (ForStorage s))
loadLimitS  :: (FabricConstraints s m) => FabricTagId   -> FabricMonad s m FabricTagId
loadPrimS   :: (FabricConstraints s m) => PrimTagId     -> FabricMonad s m (Primitive s)
loadTreeS   :: (FabricConstraints s m) => ConfineTreeId -> FabricMonad s m (ConfineTree s, DecorateTree s)
loadFabricS fabricTagId = do parent <- overStateT dagFabricStorage $ loadFabricParent fabricTagId
                             fabric <- overStateT dagFabricStorage $ loadFabric fabricTagId
                             return (parent, fabric)
loadLimitS  fabricTagId = overStateT dagFabricStorage $ loadLimit  fabricTagId
loadPrimS   primTagId   = overStateT dagPrimStorage   $ loadPrim   primTagId
loadTreeS   treeId      = overStateT dagTreeStorage   $ loadTree   treeId

loadBoxS :: (FabricConstraints s m) => PrimTagId -> FabricMonad s m (Box s)
loadBoxS primTagId = boxOf <$> loadPrimS primTagId

assignPrimParent :: MonadIO m => FabricTagId -> PrimTagId -> FabricMonad s m ()
assignPrimParent fabricTagId primTagId =
    do tag <- fromPileS (dagPrimStorage . primTagPile) . unPrimTagId $ primTagId
       let assignedTag = assignFabricTagToPrimTag tag fabricTagId
       toPileS (dagPrimStorage . primTagPile) (unPrimTagId primTagId) assignedTag
