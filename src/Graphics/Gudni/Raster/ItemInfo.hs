{-# LANGUAGE TemplateHaskell            #-}
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

module Graphics.Gudni.Raster.ItemInfo
  ( GeoReference(..)
  , GeoId(..)
  , FacetId(..)
  , ItemTag(..)
  , ItemTagId(..)
  , ItemInfo (..)
  , shapeInfoTag
  , facetInfoTag
  , tagToSubstanceTagId
  , tagIsFacet
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Facet

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.SubstanceInfo
import Graphics.Gudni.Raster.GeoReference

import Data.Bits
import Numeric

import Control.Lens
import Control.DeepSeq

import Foreign.C.Types (CUInt, CULong, CChar)
import Foreign.Ptr

newtype GeoId = GeoId {unGeoId :: Reference GeoReference} deriving (Ord, Eq, Num, Enum)

instance Show GeoId where
    show (GeoId i) = show i ++ "gid"

newtype FacetId = FacetId {unFacetId :: Reference (HardFacet_ SubSpace TextureSpace)} deriving (Ord, Eq, Num, Enum)

instance Show FacetId where
    show (FacetId i) = show i ++ "fid"

-- | The max SubstanceTagId is determined by the maximum number of unique values that can be stored in
-- 30 bits of the ItemTag - 1.
mAXsUBSTANCEiD = fromIntegral (iTEMtAGsUBSTANCEIDbITMASK `shiftR` 32 :: CULong) - 1 :: CUInt

type ItemTag_ = CULong

newtype ItemTag
    = ItemTag
    { unItemTag :: ItemTag_
    } deriving (Ord, Eq, Num, Enum)

type ItemTagId_ = Reference ItemTag
newtype ItemTagId = ItemTagId {unItemTagId :: ItemTagId_} deriving (Show, Eq, Ord)

itemSubstanceTagIdBits :: SubstanceTagId -> ItemTag_
itemSubstanceTagIdBits (SubstanceTagId substanceId) =
    (fromIntegral substanceId `shiftL` iTEMtAGsUBSTANCEIDsHIFT) .&. iTEMtAGsUBSTANCEIDbITMASK

-- | Extract the 'SubstanceTagId' from the 'ShapeTag'.
tagToSubstanceTagId :: ItemTag -> SubstanceTagId
tagToSubstanceTagId (ItemTag tag) = SubstanceTagId . fromIntegral $ ((tag .&. iTEMtAGsUBSTANCEIDbITMASK) `shiftR` iTEMtAGsUBSTANCEIDsHIFT)


shapeInfoTag :: Compound -> SubstanceTagId -> GeoId -> ItemTag
shapeInfoTag combineType substanceId geoId =
      let combineFlag =
              case combineType of
                  CompoundAdd      -> iTEMtAGcOMPOUNDtYPEaDD
                  CompoundSubtract -> iTEMtAGcOMPOUNDtYPEsUBTRACT
      in  if (unRef . unSubstanceTagId $ substanceId) <= mAXsUBSTANCEiD
          then ItemTag (   iTEMtAGiSsHAPE
                       .|. combineFlag
                       .|. itemSubstanceTagIdBits substanceId
                       .|. (fromIntegral (unGeoId geoId) .&. iTEMtAGiTEMrEFbITMASK)
                       )
          else error "shapeID out of bounds"

facetInfoTag :: FacetId -> SubstanceTagId -> ItemTag
facetInfoTag (FacetId facetId) substanceId =
    ItemTag (   iTEMtAGiSfACET
            .|. itemSubstanceTagIdBits substanceId
            .|. (fromIntegral facetId .&. iTEMtAGiTEMrEFbITMASK)
            )

tagIsFacet :: ItemTag -> Bool
tagIsFacet tag = (unItemTag tag .&. iTEMtAGiSfACETbITMASK) == iTEMtAGiSfACET

-- | Extract the 'Compound' from a 'ShapeTag'.
tagToCompound :: ItemTag -> Compound
tagToCompound tag = tagBitsToCompound (unItemTag tag .&. iTEMtAGcOMPOUNDtYPEbITMASK)

-- | Select the 'Compound' from the masked 'ShapeTag_'.
tagBitsToCompound :: ItemTag_ -> Compound
tagBitsToCompound tagBits
  | tagBits == iTEMtAGcOMPOUNDtYPEaDD      = CompoundAdd
  | tagBits == iTEMtAGcOMPOUNDtYPEsUBTRACT = CompoundSubtract
  | otherwise = error "bitMask does not correspond to a valid Compound."

-- | Extract the 'FacetId' from the 'ItemTag'.
tagToFacetId :: ItemTag -> FacetId
tagToFacetId (ItemTag tag) = FacetId $ Ref $ fromIntegral (tag .&. iTEMtAGiTEMrEFbITMASK)

-- | Extract the 'GeoId' from the 'ItemTag'.
tagToGeoId :: ItemTag -> GeoId
tagToGeoId (ItemTag tag) = GeoId $ Ref $ fromIntegral (tag .&. iTEMtAGiTEMrEFbITMASK)

-- | ItemInfo includes substance flags and the combination method for a particular shape or facet.
data ItemInfo
    = ShapeInfo
    { _itemCombine        :: Compound
    , _itemGeometry       :: GeoId
    , _itemSubstanceTagId    :: SubstanceTagId
    }
    | FacetInfo
    { _itemFacetId        :: FacetId
    , _itemFacetSubstance :: SubstanceTagId
    }
makeLenses ''ItemInfo

instance Show ItemInfo where
  show (ShapeInfo compound geoId substanceId) = "ShapeInfo "++show compound ++ " " ++ show geoId ++ " " ++ show substanceId
  show (FacetInfo facetId substanceId) = "FacetInfo " ++ show facetId ++ " " ++ show substanceId

-- | Extract the 'ItemInfo' from the 'ItemTag'.
extractItemInfo :: ItemTag -> ItemInfo
extractItemInfo tag =
  let substanceId = tagToSubstanceTagId tag
  in
  if tagIsFacet tag
  then let facetId = tagToFacetId tag
       in  FacetInfo facetId substanceId
  else
       let combineType = tagToCompound tag
           geoId       = tagToGeoId tag
       in  ShapeInfo combineType geoId substanceId

instance Show ItemTag where
  show = show . extractItemInfo

instance NFData GeoId where
  rnf (GeoId a) = a `deepseq` ()

instance NFData ItemInfo where
  rnf (ShapeInfo a b c) = a `deepseq` b `deepseq` c `deepseq` ()
  rnf (FacetInfo a b )  = a `deepseq` b `deepseq` ()

instance NFData ItemTag where
  rnf (ItemTag i) = i `deepseq` ()

instance StorableM ItemTag where
  sizeOfM _ =
    do sizeOfM (undefined :: ItemTag_   ) -- picture references must fit into data size of half4.
  alignmentM _ =
    do alignmentM (undefined :: ItemTag_  )
  peekM =
    do  tag <- peekM
        return (ItemTag tag)
  pokeM (ItemTag tag) =
        pokeM tag

instance Storable ItemTagId where
  sizeOf (ItemTagId i) = sizeOf i
  alignment (ItemTagId i) = alignment i
  peek i = ItemTagId <$> peek (castPtr i)
  poke i (ItemTagId a) = poke (castPtr i) a

instance NFData ItemTagId where
  rnf (ItemTagId a) = a `deepseq` ()

instance Storable ItemTag where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance NFData FacetId where
  rnf (FacetId i) = i `deepseq` ()
