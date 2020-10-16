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

module Graphics.Gudni.Raster.Dag.FabricTag
    ( makeFabHigh
    , makeFabLow
    , fromFabHigh
    , fromFabLow

    , fabTagType

    , makeFabTagTree
    , makeFabTagTransformAffine
    , makeFabTagTransformFacet
    , makeFabTagTransformFilter
    , makeFabTagTransformConvolve
    , makeFabTagSubstance

    , fabTagIsTree
    , fabTagIsTransformAffine
    , fabTagIsTransformFacet
    , fabTagIsTransformFilter
    , fabTagIsTransformConvolve
    , fabTagIsSubstance

    , fabTagIsBinaryOp
    , fabTagIsComposite
    , fabTagIsMult
    , fabTagIsAdd
    , fabTagIsFloatOr
    , fabTagIsFloatXor

    , makeFabTagComposite
    , makeFabTagMult
    , makeFabTagAdd
    , makeFabTagFloatOr
    , makeFabTagFloatXor

    , fabTagTreeId
    , fabTagTransformId
    , fabTagSubstanceTag
    , fabTagParentId
    , fabTagChildId
    )
where

import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.SubstanceTag
import Graphics.Gudni.Util.StorableM

import Graphics.Gudni.Raster.Serial.Reference

import Data.Bits
import Numeric

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

type FabricType_ = FabricTag_
type FabricData_ = FabricTag_
type FabricHigh_ = StorageId_
type FabricLow_  = StorageId_

makeFabHigh :: StorageId_ -> FabricTag_
makeFabHigh i = (fromIntegral i `shiftL` fABRICtAGhIGHiDsHIFT) .&. fABRICtAGhIGHiDbITMASK

fromFabHigh :: FabricTag_ -> FabricHigh_
fromFabHigh i = fromIntegral $ (i .&. fABRICtAGhIGHiDbITMASK) `shiftR` fABRICtAGhIGHiDsHIFT

makeFabLow :: FabricLow_ -> FabricTag_
makeFabLow i = fromIntegral i .&. fABRICtAGlOWiDbITMASK

fromFabLow :: FabricTag_ -> FabricLow_
fromFabLow i = fromIntegral $ i .&. fABRICtAGlOWiDbITMASK

fromFabData :: FabricTag_ -> FabricData_
fromFabData tag = tag .&. fABRICtAGdATAbITMASK

toFabData :: FabricTag_ -> FabricData_
toFabData i = i .&. fABRICtAGdATAbITMASK

makeFabTagTree :: ConfineTreeId -> FabricTagId -> FabricTag
makeFabTagTree treeId childId =
    FabricTag $ fABRICiStREE .|. (makeFabHigh . unConfineTreeId $ treeId) .|. (makeFabLow . unRef . unFabricTagId $ childId)

makeFabTagTransform :: FabricType_ -> TransformId -> FabricTagId -> FabricTag
makeFabTagTransform ty transformId fabricTagId =
    FabricTag $ ty .|. (makeFabHigh . unTransformId $ transformId) .|. (makeFabLow  . unRef . unFabricTagId $ fabricTagId)

makeFabTagTransformAffine   :: TransformId -> FabricTagId -> FabricTag
makeFabTagTransformFacet    :: TransformId -> FabricTagId -> FabricTag
makeFabTagTransformFilter   :: TransformId -> FabricTagId -> FabricTag
makeFabTagTransformConvolve :: TransformId -> FabricTagId -> FabricTag
makeFabTagTransformAffine   = makeFabTagTransform fABRICiStRANSFORMaFFINE
makeFabTagTransformFacet    = makeFabTagTransform fABRICiStRANSFORMfACET
makeFabTagTransformFilter   = makeFabTagTransform fABRICiStRANSFORMfILTER
makeFabTagTransformConvolve = makeFabTagTransform fABRICiStRANSFORMcONVOLVE

makeFabTagSubstance :: SubstanceTag -> FabricTag
makeFabTagSubstance substanceTag = FabricTag $ fABRICiSsUBSTANCE .|. (unSubstanceTag substanceTag .&. fABRICtAGdATAbITMASK)

makeBinaryOp :: FabricType_ -> FabricTagId -> FabricTagId -> FabricTag
makeBinaryOp ty parent child = FabricTag $ ty
                               .|. makeFabHigh (unRef . unFabricTagId $ parent)
                               .|. makeFabLow  (unRef . unFabricTagId $ child )

makeFabTagComposite :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagMult      :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagAdd       :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagComposite = makeBinaryOp fABRICiScOMPOSITE
makeFabTagMult      = makeBinaryOp fABRICiSmULT
makeFabTagAdd       = makeBinaryOp fABRICiSaDD
makeFabTagFloatOr   = makeBinaryOp fABRICiSfLOAToR
makeFabTagFloatXor  = makeBinaryOp fABRICiSfLOATxOR


fabTagType :: FabricTag -> FabricType_
fabTagType fabricTag = unFabricTag fabricTag .&. fABRICtAGtYPEbITMASK

matchType :: FabricType_ -> FabricTag -> Bool
matchType match fabricTag = fabTagType fabricTag == match

fabTagIsBinaryOp :: FabricTag -> Bool
fabTagIsBinaryOp fabricTag = fabTagType fabricTag >= fABRICiScOMPOSITE && fabTagType fabricTag <= fABRICiSaDD

fabTagIsTree              :: FabricTag -> Bool
fabTagIsTransformAffine   :: FabricTag -> Bool
fabTagIsTransformFacet    :: FabricTag -> Bool
fabTagIsTransformFilter   :: FabricTag -> Bool
fabTagIsTransformConvolve :: FabricTag -> Bool
fabTagIsSubstance         :: FabricTag -> Bool
fabTagIsTree              = matchType fABRICiStREE
fabTagIsTransformAffine   = matchType fABRICiStRANSFORMaFFINE
fabTagIsTransformFacet    = matchType fABRICiStRANSFORMfACET
fabTagIsTransformFilter   = matchType fABRICiStRANSFORMfILTER
fabTagIsTransformConvolve = matchType fABRICiStRANSFORMcONVOLVE
fabTagIsSubstance         = matchType fABRICiSsUBSTANCE

fabTagIsComposite :: FabricTag -> Bool
fabTagIsMult      :: FabricTag -> Bool
fabTagIsAdd       :: FabricTag -> Bool
fabTagIsFloatOr   :: FabricTag -> Bool
fabTagIsFloatXor  :: FabricTag -> Bool
fabTagIsComposite = matchType fABRICiScOMPOSITE
fabTagIsMult      = matchType fABRICiSmULT
fabTagIsAdd       = matchType fABRICiSaDD
fabTagIsFloatOr   = matchType fABRICiSfLOAToR
fabTagIsFloatXor  = matchType fABRICiSfLOATxOR

fabTagTreeId :: FabricTag -> ConfineTreeId
fabTagTreeId = ConfineTreeId . fromFabHigh . unFabricTag

fabTagTransformId :: FabricTag -> TransformId
fabTagTransformId = TransformId . fromFabHigh . unFabricTag

fabTagSubstanceTag :: FabricTag -> SubstanceTag
fabTagSubstanceTag = SubstanceTag . fromFabData . unFabricTag

fabTagParentId :: FabricTag -> FabricTagId
fabTagParentId = FabricTagId . Ref . fromFabHigh . unFabricTag

fabTagChildId :: FabricTag -> FabricTagId
fabTagChildId = FabricTagId . Ref . fromFabLow . unFabricTag

showFabType tag
    | fabTagIsTree              tag = "Tree"
    | fabTagIsTransformAffine   tag = "Affine"
    | fabTagIsTransformFacet    tag = "Facet"
    | fabTagIsTransformFilter   tag = "Filter"
    | fabTagIsTransformConvolve tag = "Convolve"
    | fabTagIsSubstance         tag = "Substance"
    | fabTagIsBinaryOp          tag = "Op"

showFabData tag
    | fabTagIsTree              tag = (show . fabTagTreeId      $ tag)
    | fabTagIsTransformAffine   tag = (show . fabTagTransformId $ tag) ++ "->" ++ show (fabTagChildId tag)
    | fabTagIsTransformFacet    tag = (show . fabTagTransformId $ tag) ++ "->" ++ show (fabTagChildId tag)
    | fabTagIsTransformFilter   tag = (show . fabTagTransformId $ tag) ++ "->" ++ show (fabTagChildId tag)
    | fabTagIsTransformConvolve tag = (show . fabTagTransformId $ tag) ++ "->" ++ show (fabTagChildId tag)
    | fabTagIsSubstance         tag = show $ fabTagSubstanceTag tag
    | fabTagIsBinaryOp          tag = (show . fabTagParentId  $ tag) ++ "->" ++ show (fabTagChildId tag)

instance Show FabricTag where
  show tag = showFabType tag ++ ":" ++ showFabData tag
