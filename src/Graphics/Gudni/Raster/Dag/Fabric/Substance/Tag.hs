-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.CompoundTag
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructors for attaching metadata to shapes∘

module Graphics.Gudni.Raster.Dag.Fabric.Substance.Tag
    (  makeSubstanceTagGeometry
    ,  makeSubstanceTagConstant
    ,  makeSubstanceTagTexture
    ,  makeSubstanceTagLinear
    ,  makeSubstanceTagQuadrance
    ,  substanceTagIsGeometry
    ,  substanceTagIsConstant
    ,  substanceTagIsTexture
    ,  substanceTagIsLinear
    ,  substanceTagIsQuadrance
    ,  substanceTagCase
    ,  substanceTagDescription
    )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.Dag.TagTypes

import Graphics.Gudni.Raster.Serial.Reference

import Data.Bits
import Numeric

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

-- | The word "tag" is used to describe a bitfield that usually includes type metadata and pointers to other data.
-- | The word "case" is used here like "type" to determine the type of substance represented by the tag.

type SubstanceTagData_ = SubstanceTag_
type SubstanceTagCase_ = SubstanceTag_

makeSubstanceData :: SubstanceTagData_ -> SubstanceTag_
makeSubstanceData i = i .&. fABRICtAGdATAbITMASK

fromSubstanceData :: SubstanceTag_ -> SubstanceTagData_
fromSubstanceData tag = tag .&. fABRICtAGdATAbITMASK

makeSubstanceTagGeometry  ::                SubstanceTag
makeSubstanceTagConstant  :: Reference t -> SubstanceTag
makeSubstanceTagTexture   :: Reference t -> SubstanceTag
makeSubstanceTagLinear    ::                SubstanceTag
makeSubstanceTagQuadrance ::                SubstanceTag
makeSubstanceTagGeometry      = SubstanceTag $ sUBSTANCEiSgEOMETRY
makeSubstanceTagConstant  ref = SubstanceTag $ sUBSTANCEiScONSTANT .|. makeSubstanceData (fromIntegral . unRef $ ref)
makeSubstanceTagTexture   ref = SubstanceTag $ sUBSTANCEiStEXTURE  .|. makeSubstanceData (fromIntegral . unRef $ ref)
makeSubstanceTagLinear        = SubstanceTag $ sUBSTANCEiSlINEAR
makeSubstanceTagQuadrance     = SubstanceTag $ sUBSTANCEiSqUADRANCE

substanceTagIsGeometry   :: SubstanceTag -> Bool
substanceTagIsConstant   :: SubstanceTag -> Bool
substanceTagIsTexture    :: SubstanceTag -> Bool
substanceTagIsLinear     :: SubstanceTag -> Bool
substanceTagIsQuadrance  :: SubstanceTag -> Bool
substanceTagIsGeometry   tag = substanceTagCase tag == sUBSTANCEiSgEOMETRY
substanceTagIsConstant   tag = substanceTagCase tag == sUBSTANCEiScONSTANT
substanceTagIsTexture    tag = substanceTagCase tag == sUBSTANCEiStEXTURE
substanceTagIsLinear     tag = substanceTagCase tag == sUBSTANCEiSlINEAR
substanceTagIsQuadrance  tag = substanceTagCase tag == sUBSTANCEiSqUADRANCE

substanceTagCase :: SubstanceTag -> SubstanceTagCase_
substanceTagCase tag = unSubstanceTag tag .&. sUBSTANCEtAGtYPEbITMASK

substanceTagDescription :: SubstanceTag -> SubstanceTagData_
substanceTagDescription tag = unSubstanceTag tag .&. sUBSTANCEdATArEFbITMASK

showSubstanceCase tag
    | substanceTagIsConstant  tag = "Constant"
    | substanceTagIsTexture   tag = "Texture"
    | substanceTagIsLinear    tag = "Linear"
    | substanceTagIsQuadrance tag = "Quadrance"

instance Show SubstanceTag where
  show tag = showSubstanceCase tag ++ show (substanceTagDescription tag)
