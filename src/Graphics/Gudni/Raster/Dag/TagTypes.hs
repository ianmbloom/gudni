{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Raster.Dag.TagTypes
    ( PrimTag(..)
    , PrimTagId(..)
    , FabricTagId(..)
    , FabricTag(..)
    , BezierId(..)
    , FacetId(..)
    , ConfineTreeId(..)
    , TransformId(..)
    , SubstanceTag(..)
    , nullFabricTagId
    )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Dag.Constants

import Graphics.Gudni.Util.StorableM

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import Foreign.Storable
import GHC.Ptr

newtype PrimTagId     = PrimTagId     {unPrimTagId     :: Reference PrimTag     } deriving (Eq, Ord, Generic       )
newtype PrimTag       = PrimTag       {unPrimTag       :: PrimTag_              } deriving (Eq, Ord, Generic       )
newtype FabricTagId   = FabricTagId   {unFabricTagId   :: Reference FabricTag   } deriving (Eq, Ord, Generic, Show )
newtype FabricTag     = FabricTag     {unFabricTag     :: FabricTag_            } deriving (Eq, Ord, Generic       )
newtype BezierId s    = BezierId      {unBezierId      :: Reference (Bezier s)  } deriving (Eq, Ord, Generic       )
newtype FacetId s     = FacetId       {unFacetId       :: Reference (Facet s)   } deriving (Eq, Ord, Generic       )
newtype ConfineTreeId = ConfineTreeId {unConfineTreeId :: StorageId_            } deriving (Num, Eq, Ord, Generic, Show )
newtype TransformId   = TransformId   {unTransformId   :: StorageId_            } deriving (Eq, Ord, Generic, Show )
newtype SubstanceTag  = SubstanceTag  {unSubstanceTag  :: FabricTag_            } deriving (Eq, Ord, Generic       )

nullFabricTagId :: FabricTagId
nullFabricTagId = FabricTagId (Ref $ nULLfABRICtAGiD)

instance Show (BezierId s) where
    show (BezierId i) = show i ++ "bid"

instance Show (FacetId s) where
    show (FacetId i) = show i ++ "fid"

instance Show PrimTagId where
  show = show . unRef . unPrimTagId

instance Out PrimTag
instance Out PrimTagId
instance Out FabricTagId
instance Out FabricTag
instance Out s => Out (BezierId s)
instance Out s => Out (FacetId  s)
instance Out ConfineTreeId
instance Out TransformId
instance Out SubstanceTag

instance Storable PrimTagId where
  sizeOf    (PrimTagId i) = sizeOf    (undefined :: Reference PrimTag )
  alignment (PrimTagId i) = alignment (undefined :: Reference PrimTag )
  peek ptr =  PrimTagId <$>  peek (castPtr ptr)
  poke ptr   (PrimTagId i) = poke (castPtr ptr) i

instance Storable PrimTag where
  sizeOf    (PrimTag i) = sizeOf    (undefined :: PrimTag_ )
  alignment (PrimTag i) = alignment (undefined :: PrimTag_ )
  peek ptr =  PrimTag <$>  peek (castPtr ptr)
  poke ptr   (PrimTag i) = poke (castPtr ptr) i

instance Storable FabricTagId where
  sizeOf    (FabricTagId i) = sizeOf    (undefined :: Reference FabricTag )
  alignment (FabricTagId i) = alignment (undefined :: Reference FabricTag )
  peek ptr = FabricTagId <$>  peek (castPtr ptr)
  poke ptr  (FabricTagId i) = poke (castPtr ptr) i

instance Storable FabricTag where
  sizeOf    (FabricTag i) = sizeOf    (undefined :: FabricTag_ )
  alignment (FabricTag i) = alignment (undefined :: FabricTag_ )
  peek ptr = FabricTag <$>  peek (castPtr ptr)
  poke ptr  (FabricTag i) = poke (castPtr ptr) i

instance Storable s => Storable (BezierId s) where
  sizeOf    (BezierId i) = sizeOf    (undefined :: Reference (Bezier s) )
  alignment (BezierId i) = alignment (undefined :: Reference (Bezier s) )
  peek ptr = BezierId <$>  peek (castPtr ptr)
  poke ptr  (BezierId i) = poke (castPtr ptr) i

instance Storable s => Storable (FacetId  s) where
  sizeOf    (FacetId i) = sizeOf    (undefined :: Reference (Facet s) )
  alignment (FacetId i) = alignment (undefined :: Reference (Facet s) )
  peek ptr = FacetId <$>  peek (castPtr ptr)
  poke ptr  (FacetId i) = poke (castPtr ptr) i

instance Storable ConfineTreeId where
  sizeOf    (ConfineTreeId i) = sizeOf    (undefined :: StorageId_ )
  alignment (ConfineTreeId i) = alignment (undefined :: StorageId_ )
  peek ptr = ConfineTreeId <$>  peek (castPtr ptr)
  poke ptr  (ConfineTreeId i) = poke (castPtr ptr) i

instance Storable TransformId where
  sizeOf    (TransformId i) = sizeOf    (undefined :: StorageId_ )
  alignment (TransformId i) = alignment (undefined :: StorageId_ )
  peek ptr = TransformId <$>  peek (castPtr ptr)
  poke ptr  (TransformId i) = poke (castPtr ptr) i

instance Storable SubstanceTag where
  sizeOf    (SubstanceTag i) = sizeOf    (undefined :: FabricTag_ )
  alignment (SubstanceTag i) = alignment (undefined :: FabricTag_ )
  peek ptr = SubstanceTag <$>  peek (castPtr ptr)
  poke ptr  (SubstanceTag i) = poke (castPtr ptr) i
