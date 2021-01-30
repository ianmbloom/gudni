{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Raster.TagTypes
    ( CodeCounter(..)
    , FabricTagId(..)
    , nullFabricTagId
    , FabricTag(..)
    , TransformId(..)
    )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Constants

import Foreign.Storable
import Foreign.Ptr
-- import Numeric (showHex)

-- | The word "tag" is used to describe a bitfield that usually includes type metadata and pointers to other data.

newtype CodeCounter = CodeCounter {unCodeCounter :: CodeCounter_        } deriving (Eq, Ord, Generic, Num  )
newtype FabricTagId = FabricTagId {unFabricTagId :: Reference FabricTag } deriving (Eq, Ord, Generic, Num  )
newtype FabricTag   = FabricTag   {unFabricTag   :: FabricTag_          } deriving (Eq, Ord, Generic       )
newtype TransformId = TransformId {unTransformId :: TransformId_        } deriving (Eq, Ord, Generic, Show )

nullFabricTagId :: FabricTagId
nullFabricTagId = FabricTagId (Ref $ nULLfABRICtAGiD)

-- instance Show FabricTag where
--   show tag = (showHex . unFabricTag $ tag) ""

instance Show FabricTagId where
  show fabricTagId = if fabricTagId == nullFabricTagId
                     then "null"
                     else show . unRef . unFabricTagId $ fabricTagId


instance Show CodeCounter where
  show = show . unCodeCounter

instance Out CodeCounter where
  doc = text . show
  docPrec _ = doc

instance Out FabricTagId where
    doc tagId = if tagId == nullFabricTagId
                then text "null"
                else text "Fb" <+> (doc . unRef . unFabricTagId $ tagId)
    docPrec _ = doc
instance Out FabricTag
instance Out TransformId

instance Storable CodeCounter where
  sizeOf    (CodeCounter i) = sizeOf    (undefined :: CodeCounter_ )
  alignment (CodeCounter i) = alignment (undefined :: CodeCounter_ )
  peek ptr =  CodeCounter <$>  peek (castPtr ptr)
  poke ptr   (CodeCounter i) = poke (castPtr ptr) i

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

instance Storable TransformId where
  sizeOf    (TransformId i) = sizeOf    (undefined :: TransformId_ )
  alignment (TransformId i) = alignment (undefined :: TransformId_ )
  peek ptr = TransformId <$>  peek (castPtr ptr)
  poke ptr  (TransformId i) = poke (castPtr ptr) i
