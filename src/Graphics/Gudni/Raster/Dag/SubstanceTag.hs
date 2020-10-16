{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

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

module Graphics.Gudni.Raster.Dag.SubstanceTag
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
    ,  substanceTagType
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

type SubstanceTagData_ = SubstanceTag_
type SubstanceTagType_ = SubstanceTag_

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
substanceTagIsGeometry   tag = substanceTagType tag == sUBSTANCEiSgEOMETRY
substanceTagIsConstant   tag = substanceTagType tag == sUBSTANCEiScONSTANT
substanceTagIsTexture    tag = substanceTagType tag == sUBSTANCEiStEXTURE
substanceTagIsLinear     tag = substanceTagType tag == sUBSTANCEiSlINEAR
substanceTagIsQuadrance  tag = substanceTagType tag == sUBSTANCEiSqUADRANCE

substanceTagType :: SubstanceTag -> SubstanceTagType_
substanceTagType tag = unSubstanceTag tag .&. sUBSTANCEtAGtYPEbITMASK

substanceTagDescription :: SubstanceTag -> SubstanceTagData_
substanceTagDescription tag = unSubstanceTag tag .&. sUBSTANCEdATArEFbITMASK

showSubstanceType tag
    | substanceTagIsConstant  tag = "Constant"
    | substanceTagIsTexture   tag = "Texture"
    | substanceTagIsLinear    tag = "Linear"
    | substanceTagIsQuadrance tag = "Quadrance"

instance Show SubstanceTag where
  show tag = showSubstanceType tag ++ show (substanceTagDescription tag)
