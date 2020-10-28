 {-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Raster.Dag.TagTypes
    ( ShapeId(..)
    , nullShapeId
    , PrimTag(..)
    , PrimTagId(..)
    , FabricTagId(..)
    , nullFabricTagId
    , FabricTag(..)
    , BezierId(..)
    , FacetId(..)
    , BoxId(..)
    , ConfineTreeId(..)
    , TransformId(..)
    , SubstanceTag(..)
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

-- | The word "tag" is used to describe a bitfield that usually includes type metadata and pointers to other data.

newtype ShapeId       = ShapeId       {unShapeId       :: ShapeId_              } deriving (Eq, Ord, Generic,       Num)
newtype PrimTagId     = PrimTagId     {unPrimTagId     :: Reference PrimTag     } deriving (Eq, Ord, Generic       )
newtype PrimTag       = PrimTag       {unPrimTag       :: PrimTag_              } deriving (Eq, Ord, Generic       )
newtype FabricTagId   = FabricTagId   {unFabricTagId   :: Reference FabricTag   } deriving (Eq, Ord, Generic       )
newtype FabricTag     = FabricTag     {unFabricTag     :: FabricTag_            } deriving (Eq, Ord, Generic       )
newtype BezierId s    = BezierId      {unBezierId      :: Reference (Bezier s)  } deriving (Eq, Ord, Generic, Show )
newtype FacetId s     = FacetId       {unFacetId       :: Reference (Facet s)   } deriving (Eq, Ord, Generic, Show )
newtype BoxId s       = BoxId         {unBoxId         :: Reference (Box s)     } deriving (Eq, Ord, Generic, Show )
newtype ConfineTreeId = ConfineTreeId {unConfineTreeId :: StorageId_            } deriving (Eq, Ord, Generic,       Num)
newtype TransformId   = TransformId   {unTransformId   :: StorageId_            } deriving (Eq, Ord, Generic, Show )
newtype SubstanceTag  = SubstanceTag  {unSubstanceTag  :: FabricTag_            } deriving (Eq, Ord, Generic       )

nullFabricTagId :: FabricTagId
nullFabricTagId = FabricTagId (Ref $ nULLfABRICtAGiD)

nullShapeId :: ShapeId
nullShapeId = ShapeId nULLsHAPEiD

instance Show ShapeId where
  show shapeId = if shapeId == nullShapeId
                 then "null"
                 else show . unShapeId $ shapeId

instance Show FabricTagId where
  show fabricTagId = if fabricTagId == nullFabricTagId
                     then "null"
                     else show . unRef . unFabricTagId $ fabricTagId

instance Show PrimTagId where
  show = show . unRef . unPrimTagId

instance Show ConfineTreeId where
  show = show . unConfineTreeId

instance Out ShapeId where
    doc tagId = if tagId == nullShapeId
                then text "null"
                else text "Sh" <+> (doc . unShapeId $ tagId)
    docPrec _ = doc
instance Out PrimTag
instance Out PrimTagId where
    doc tagId = doc . unRef . unPrimTagId $ tagId
    docPrec _ = doc
instance Out FabricTagId where
    doc tagId = if tagId == nullFabricTagId
                then text "null"
                else text "Fb" <+> (doc . unRef . unFabricTagId $ tagId)
    docPrec _ = doc
instance Out FabricTag
instance Out s => Out (BezierId s)
instance Out s => Out (FacetId  s)
instance Out s => Out (BoxId  s)
instance Out ConfineTreeId where
    doc treeId = text "Tx" <+> (doc . unConfineTreeId $ treeId)
    docPrec _  = doc
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

instance Storable s => Storable (BoxId  s) where
  sizeOf    (BoxId i) = sizeOf    (undefined :: Reference (Box s) )
  alignment (BoxId i) = alignment (undefined :: Reference (Box s) )
  peek ptr = BoxId <$>  peek (castPtr ptr)
  poke ptr  (BoxId i) = poke (castPtr ptr) i

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
