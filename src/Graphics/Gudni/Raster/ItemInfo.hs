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
  , ItemInfo (..)
  , shapeInfoTag
  , facetInfoTag
  , tagToSubstanceId
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

-- | The max SubstanceId is determined by the maximum number of unique values that can be stored in
-- 30 bits of the ItemTag - 1.
mAXsUBSTANCEiD = fromIntegral (iTEMtAGsUBSTANCEIDbITMASK `shiftR` 32 :: CULong) - 1 :: CUInt

type ItemTag_ = CULong

newtype ItemTag
    = ItemTag
    { unItemTag :: ItemTag_
    } deriving (Show, Ord, Eq, Num, Enum)

itemSubstanceIdBits :: SubstanceId -> ItemTag_
itemSubstanceIdBits (SubstanceId substanceId) =
    (fromIntegral substanceId `shiftL` iTEMtAGsUBSTANCEIDsHIFT) .&. iTEMtAGsUBSTANCEIDbITMASK

-- | Extract the 'SubstanceId' from the 'ShapeTag'.
tagToSubstanceId :: ItemTag -> SubstanceId
tagToSubstanceId (ItemTag tag) = SubstanceId . fromIntegral $ ((tag .&. iTEMtAGsUBSTANCEIDbITMASK) `shiftR` iTEMtAGsUBSTANCEIDsHIFT)


shapeInfoTag :: Compound -> SubstanceId -> GeoId -> ItemTag
shapeInfoTag combineType substanceId geoId =
      let combineFlag =
              case combineType of
                  CompoundAdd      -> iTEMtAGcOMPOUNDtYPEaDD
                  CompoundSubtract -> iTEMtAGcOMPOUNDtYPEsUBTRACT
      in  if (unRef . unSubstanceId $ substanceId) <= mAXsUBSTANCEiD
          then ItemTag (   iTEMtAGiSsHAPE
                       .|. combineFlag
                       .|. itemSubstanceIdBits substanceId
                       .|. (fromIntegral (unGeoId geoId) .&. iTEMtAGiTEMiDbITMASK)
                       )
          else error "shapeID out of bounds"

facetInfoTag :: FacetId -> SubstanceId -> ItemTag
facetInfoTag (FacetId facetId) substanceId =
    ItemTag (   iTEMtAGiSfACET
            .|. itemSubstanceIdBits substanceId
            .|. (fromIntegral facetId .&. iTEMtAGiTEMiDbITMASK)
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
tagToFacetId (ItemTag tag) = FacetId $ Ref $ fromIntegral (tag .&. iTEMtAGiTEMiDbITMASK)

-- | Extract the 'GeoId' from the 'ItemTag'.
tagToGeoId :: ItemTag -> GeoId
tagToGeoId (ItemTag tag) = GeoId $ Ref $ fromIntegral (tag .&. iTEMtAGiTEMiDbITMASK)

-- | ItemInfo includes substance flags and the combination method for a particular shape or facet.
data ItemInfo
    = ShapeInfo
    { _itemCombine        :: Compound
    , _itemGeometry       :: GeoId
    , _itemSubstanceId    :: SubstanceId
    }
    | FacetInfo
    { _itemFacetId        :: FacetId
    , _itemFacetSubstance :: SubstanceId
    }
    deriving (Show)
makeLenses ''ItemInfo

-- | Extract the 'ItemInfo' from the 'ItemTag'.
extractItemInfo :: ItemTag -> ItemInfo
extractItemInfo tag =
  let substanceId = tagToSubstanceId tag
  in
  if tagIsFacet tag
  then let facetId = tagToFacetId tag
       in  FacetInfo facetId substanceId
  else
       let combineType = tagToCompound tag
           geoId       = tagToGeoId tag
       in  ShapeInfo combineType geoId substanceId

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

instance Storable ItemTag where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance NFData FacetId where
  rnf (FacetId i) = i `deepseq` ()
