{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Graphics.Gudni.Raster.Dag.Storage
   ( DagStorage(..)
   , dagFabricStorage
   , dagTreeStorage
   , dagPrimTagIds
   , dagPixelPile
   , DagMonad(..)
   , inTree
   , DagConstraints(..)
   , fabricCodeStart
   , storageCodeStart
   , addTreeS
   , loadFabricTagS
   , loadColorS
   , loadAffineS
   , loadFacetS
   , loadConvolveS
   , loadPixelS
   )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Build
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Storage
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Dag.Fabric.Filter.Type
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad.State
import Control.Lens
import qualified Data.Map as M
import Foreign.Storable
import Foreign.C.Types (CChar)

data DagStorage s = DagStorage
    { -- | Piles storing the fabric structure, substances and descriptions.
      _dagFabricStorage    :: FabricStorage s
      -- | Piles storing the confineTrees
    , _dagTreeStorage      :: TreeStorage s
      -- | Pile of primitive tags used to build trees.
    , _dagPrimTagIds       :: Pile PrimTagId
      -- | All Accumulated Texture data.
    , _dagPixelPile        :: PixelPile
    }
makeLenses ''DagStorage

type DagMonad s m = StateT (DagStorage s) m

inTree :: (MonadState (DagStorage s) m, MonadIO m, Storable s, Space s) => StateT (TreeStorage s) m a -> m a
inTree = overStateT dagTreeStorage

type DagConstraints s m = ( MonadIO m
                          , Space s
                          , Storable s
                          )

fabricCodeStart :: DagConstraints s m => DagMonad s m FabricTagId
fabricCodeStart = storageCodeStart <$> use id

storageCodeStart :: DagStorage s -> FabricTagId
storageCodeStart = FabricTagId . (subtract 1) . view (dagFabricStorage . fabricTagPile . pileCursor)

addTreeS :: (DagConstraints s m) => s -> Int -> Slice PrimTagId -> DagMonad s m (DecoTagId s, ConfineTagId s)
addTreeS limit decorationLimit slice =
    do pile <- use dagPrimTagIds
       inTree $ buildConfineTree limit decorationLimit slice pile

loadFabricTagS  :: (DagConstraints s m) => FabricTagId -> DagMonad s m FabricTag
loadFabricTagS  fabricTagId = overStateT dagFabricStorage $ loadFabricTag      fabricTagId

loadColorS :: (MonadIO m, Storable s, Space s) => FabricTag -> DagMonad s m (Color s)
loadColorS tag =
    do (AsBytes (color :: Color s)) <- fromPileS (dagFabricStorage . fabricHeapPile) (Ref . fabTagSubstanceRef $ tag)
       return color

loadTransS :: (MonadIO m, Storable s, CanLoad CChar a) => FabricTag -> DagMonad s m a
loadTransS tag = fromPileS (dagFabricStorage . fabricHeapPile) (Ref . unTransformId . fabTagTransformId $ tag)

loadAffineS :: (MonadIO m, Storable s) => FabricTag -> DagMonad s m (FTransformer s)
loadAffineS tag =
    do (AsBytes ((forward, back) :: (Affine s, Affine s))) <- loadTransS tag
       return $ FAffine forward back

loadFacetS :: (MonadIO m, Storable s) => FabricTag -> DagMonad s m (FTransformer s)
loadFacetS tag =
    do (AsBytes (facet :: Facet s)) <- loadTransS tag
       return $ FFacet facet

loadConvolveS :: (MonadIO m, Storable s) => FabricTag -> DagMonad s m (FTransformer s)
loadConvolveS tag =
    do (AsBytes (scale :: s)) <- loadTransS tag
       return (FConvolve scale)

loadPixelS :: (DagConstraints s m) => FabricTag -> Point2 s -> DagMonad s m (Color s)
loadPixelS tag point =
  do let pixel = fmap floor point
     (AsBytes memRef) <- fromPileS (dagFabricStorage . fabricHeapPile) (Ref . fromIntegral . fabTagSubstanceRef $ tag)
     pixelPile <- use dagPixelPile
     lift $ getPixelColor pixelPile memRef pixel
