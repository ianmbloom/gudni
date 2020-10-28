-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Fabric.Tag
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for constructing nodes in a fabric DAG.

module Graphics.Gudni.Raster.Dag.Fabric.Tag
    ( makeFabHigh
    , makeFabLow
    , fromFabHigh
    , fromFabLow

    , fabTagCase

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

    , makeLimits
    , fromLimits
    )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Tag
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Debug

import Graphics.Gudni.Raster.Serial.Reference

import Data.Bits
import Numeric

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

-- | The word "tag" is used to describe a bitfield that usually includes type metadata and pointers to other data.
-- | The word "case" is used here like "type" the fabric tags cast determines what type of fabric node it represents.
-- | The tags can be traversed in memory on different devices to traverse the DAG representing a scene.

type FabricCase_ = FabricTag_
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

makeFabTagTransform :: FabricCase_ -> TransformId -> FabricTagId -> FabricTag
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

makeBinaryOp :: FabricCase_ -> FabricTagId -> FabricTagId -> FabricTag
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


fabTagCase :: FabricTag -> FabricCase_
fabTagCase fabricTag = unFabricTag fabricTag .&. fABRICtAGtYPEbITMASK

matchCase :: FabricCase_ -> FabricTag -> Bool
matchCase match fabricTag = fabTagCase fabricTag == match

fabTagIsBinaryOp :: FabricTag -> Bool
fabTagIsBinaryOp fabricTag = fabTagCase fabricTag >= fABRICiScOMPOSITE && fabTagCase fabricTag <= fABRICiSaDD

fabTagIsTree              :: FabricTag -> Bool
fabTagIsTransformAffine   :: FabricTag -> Bool
fabTagIsTransformFacet    :: FabricTag -> Bool
fabTagIsTransformFilter   :: FabricTag -> Bool
fabTagIsTransformConvolve :: FabricTag -> Bool
fabTagIsSubstance         :: FabricTag -> Bool
fabTagIsTree              = matchCase fABRICiStREE
fabTagIsTransformAffine   = matchCase fABRICiStRANSFORMaFFINE
fabTagIsTransformFacet    = matchCase fABRICiStRANSFORMfACET
fabTagIsTransformFilter   = matchCase fABRICiStRANSFORMfILTER
fabTagIsTransformConvolve = matchCase fABRICiStRANSFORMcONVOLVE
fabTagIsSubstance         = matchCase fABRICiSsUBSTANCE

fabTagIsComposite :: FabricTag -> Bool
fabTagIsMult      :: FabricTag -> Bool
fabTagIsAdd       :: FabricTag -> Bool
fabTagIsFloatOr   :: FabricTag -> Bool
fabTagIsFloatXor  :: FabricTag -> Bool
fabTagIsComposite = matchCase fABRICiScOMPOSITE
fabTagIsMult      = matchCase fABRICiSmULT
fabTagIsAdd       = matchCase fABRICiSaDD
fabTagIsFloatOr   = matchCase fABRICiSfLOAToR
fabTagIsFloatXor  = matchCase fABRICiSfLOATxOR

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


makeHighLimit :: ShapeId_ -> FabricTag_
makeHighLimit shapeId = fromIntegral shapeId `shiftL` 32

makeLowLimit :: ShapeId_ -> FabricTag_
makeLowLimit = fromIntegral

makeLimits :: ShapeId -> ShapeId -> FabricTag
makeLimits aRef bRef = FabricTag $ (makeHighLimit . unShapeId $ tr "makeLimits aRef" aRef) .|. (makeLowLimit . unShapeId $ tr "makeLimits bRef" bRef)

fromHighLimit :: FabricTag_ -> ShapeId_
fromHighLimit fabricTag = fromIntegral $ fabricTag `shiftR` 32

fromLowLimit :: FabricTag_ -> ShapeId_
fromLowLimit tag = fromIntegral tag .&. nULLsHAPEiD

fromLimits :: FabricTag -> (ShapeId, ShapeId)
fromLimits (FabricTag tag) = ( ShapeId . fromHighLimit $ tag
                             , ShapeId . fromLowLimit  $ tag
                             )

showFabCase tag
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
  show tag = showFabCase tag ++ ":" ++ showFabData tag
