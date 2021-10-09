{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.ItemInfo
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructors for attaching metadata to shapes∘

module Graphics.Gudni.Raster.Thresholds.ItemInfo
    ( StrandReference(..)
    , StrandId(..)
    , FacetId(..)
    , PrimTag(..)
    , ItemTagId(..)
    , nullItemTagId
    , ItemInfo (..)
    , strandInfoTag
    , facetInfoTag
    , tagToSubstanceTagId
    , tagIsFacet
    )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.StorableM

import Graphics.Gudni.ShapeTree.STree
import Graphics.Gudni.Raster.Thresholds.Constants
import Graphics.Gudni.Raster.Thresholds.SubstanceInfo
import Graphics.Gudni.Raster.Thresholds.StrandReference

import Data.Bits
import Numeric

import Control.Lens

import Foreign.C.Types (CUInt, CULong, CChar)
import Foreign.Ptr

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

newtype StrandId = StrandId {unStrandId :: Reference StrandReference} deriving (Ord, Eq, Num, Enum)

instance Show StrandId where
    show (StrandId i) = show i ++ "gid"

newtype FacetId = FacetId {unFacetId :: Reference (Facet SubSpace)} deriving (Ord, Eq, Num, Enum)

instance Show FacetId where
    show (FacetId i) = show i ++ "fid"

-- | The max SubstanceTagId is determined by the maximum number of unique values that can be stored in
-- 30 bits of the PrimTag - 1.
mAXsUBSTANCEiD = fromIntegral (iTEMtAGsUBSTANCEIDbITMASK `shiftR` 32 :: CULong) - 1 :: CUInt

type ItemTag_ = CULong

newtype PrimTag
    = PrimTag
    { unItemTag :: ItemTag_
    } deriving (Ord, Eq, Num, Enum)

type ItemTagId_ = Reference PrimTag
newtype ItemTagId = ItemTagId {unItemTagId :: ItemTagId_} deriving (Eq, Ord, Generic)
instance Show ItemTagId where
  show = show . unRef . unItemTagId
nullItemTagId = ItemTagId nullReference
instance Out ItemTagId where
    doc x = text . show $ x
    docPrec _ = doc

itemSubstanceTagIdBits :: SubstanceTagId -> ItemTag_
itemSubstanceTagIdBits (SubstanceTagId substanceId) =
    (fromIntegral substanceId `shiftL` iTEMtAGsUBSTANCEIDsHIFT) .&. iTEMtAGsUBSTANCEIDbITMASK

-- | Extract the 'SubstanceTagId' from the 'ShapeTag'.
tagToSubstanceTagId :: PrimTag -> SubstanceTagId
tagToSubstanceTagId (PrimTag tag) = SubstanceTagId . fromIntegral $ ((tag .&. iTEMtAGsUBSTANCEIDbITMASK) `shiftR` iTEMtAGsUBSTANCEIDsHIFT)


strandInfoTag :: Compound -> SubstanceTagId -> StrandReference -> PrimTag
strandInfoTag compound substanceId strandRef =
      let combineFlag =
              case compound of
                  CompoundAdd      -> iTEMtAGcOMPOUNDaDD
                  CompoundSubtract -> iTEMtAGcOMPOUNDsUBTRACT
      in  if (unRef . unSubstanceTagId $ substanceId) <= mAXsUBSTANCEiD
          then PrimTag (   iTEMtAGiSbEZIER
                       .|. combineFlag
                       .|. itemSubstanceTagIdBits substanceId
                       .|. (fromIntegral (strandRef ^. unStrandRef) .&. iTEMtAGrEFERENCEbITMASK)
                       )
          else error "shapeID out of bounds"

facetInfoTag :: FacetId -> SubstanceTagId -> PrimTag
facetInfoTag (FacetId facetId) substanceId =
    PrimTag (   iTEMtAGiSfACET
            .|. itemSubstanceTagIdBits substanceId
            .|. (fromIntegral facetId .&. iTEMtAGrEFERENCEbITMASK)
            )

tagIsFacet :: PrimTag -> Bool
tagIsFacet tag = (unItemTag tag .&. iTEMtAGtYPEbITMASK) == iTEMtAGiSfACET

-- | Extract the 'Compound' from a 'ShapeTag'.
tagToCompound :: PrimTag -> Compound
tagToCompound tag = tagBitsToCompound (unItemTag tag .&. iTEMtAGcOMPOUNDbITMASK)

-- | Select the 'Compound' from the masked 'ShapeTag_'.
tagBitsToCompound :: ItemTag_ -> Compound
tagBitsToCompound tagBits
  | tagBits == iTEMtAGcOMPOUNDaDD      = CompoundAdd
  | tagBits == iTEMtAGcOMPOUNDsUBTRACT = CompoundSubtract
  | otherwise = error "bitMask does not correspond to a valid Compound."

-- | Extract the 'FacetId' from the 'PrimTag'.
tagToFacetId :: PrimTag -> FacetId
tagToFacetId (PrimTag tag) = FacetId $ Ref $ fromIntegral (tag .&. iTEMtAGrEFERENCEbITMASK)

-- | Extract the 'StrandId' from the 'PrimTag'.
tagToStrandId :: PrimTag -> StrandId
tagToStrandId (PrimTag tag) = StrandId $ Ref $ fromIntegral (tag .&. iTEMtAGrEFERENCEbITMASK)

-- | ItemInfo includes substance flags and the combination method for a particular shape or facet.
data ItemInfo
    = ShapeInfo
    { _itemCombine        :: Compound
    , _itemGeometry       :: StrandId
    , _itemSubstanceTagId  :: SubstanceTagId
    }
    | FacetInfo
    { _itemFacetId        :: FacetId
    , _itemFacetSubstance :: SubstanceTagId
    }
makeLenses ''ItemInfo

instance Show ItemInfo where
  show (ShapeInfo compound strandId substanceId) = "ShapeInfo "++show compound ++ " " ++ show strandId ++ " " ++ show substanceId
  show (FacetInfo facetId substanceId) = "FacetInfo " ++ show facetId ++ " " ++ show substanceId

-- | Extract the 'ItemInfo' from the 'PrimTag'.
extractItemInfo :: PrimTag -> ItemInfo
extractItemInfo tag =
  let substanceId = tagToSubstanceTagId tag
  in
  if tagIsFacet tag
  then let facetId = tagToFacetId tag
       in  FacetInfo facetId substanceId
  else
       let compound = tagToCompound tag
           strandId       = tagToStrandId tag
       in  ShapeInfo compound strandId substanceId

instance Show PrimTag where
  show = show . extractItemInfo

instance StorableM PrimTag where
  sizeOfM _ =
    do sizeOfM (undefined :: ItemTag_   ) -- picture references must fit into data size of half4.
  alignmentM _ =
    do alignmentM (undefined :: ItemTag_  )
  peekM =
    do  tag <- peekM
        return (PrimTag tag)
  pokeM (PrimTag tag) =
        pokeM tag

instance Storable ItemTagId where
  sizeOf (ItemTagId i) = sizeOf i
  alignment (ItemTagId i) = alignment i
  peek i = ItemTagId <$> peek (castPtr i)
  poke i (ItemTagId a) = poke (castPtr i) a

instance Storable PrimTag where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
