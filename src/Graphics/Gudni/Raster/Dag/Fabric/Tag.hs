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
    ( makeFabTagTree                 --

    , makeFabTagTransformAffine      --
    , makeFabTagTransformFacet       --
    , makeFabTagTransformConvolve    --

    , makeFabTagSqrt                 --
    , makeFabTagInvert               --
    , makeFabTagCos                  --
    , makeFabTagSin                  --
    , makeFabTagClamp                --

    , makeFabTagComposite            --
    , makeFabTagMult                 --
    , makeFabTagAdd                  --
    , makeFabTagFloatOr              --
    , makeFabTagFloatXor             --
    , makeFabTagMin                  --
    , makeFabTagMax                  --
    , makeFabTagHsvAdjust            --
    , makeFabTagTranparent           --

    , makeFabTagConstant             --
    , makeFabTagTexture              --
    , makeFabTagLinear               --
    , makeFabTagQuadrance            --

    , fabTagIsLeaf                   --
    , fabTagIsUnaryPre               --
    , fabTagIsUnaryPost              --
    , fabTagIsBinaryOp               --

    , fabTagIsConstant               --
    , fabTagIsTexture                --
    , fabTagIsLinear                 --
    , fabTagIsQuadrance              --

    , fabTagIsTree                   --
    , fabTagIsTransformAffine        --
    , fabTagIsTransformFacet         --
    , fabTagIsTransformConvolve      --

    , fabTagIsSqrt                   --
    , fabTagIsInvert                 --
    , fabTagIsCos                    --
    , fabTagIsSin                    --
    , fabTagIsClamp                  --

    , fabTagIsComposite              --
    , fabTagIsMult                   --
    , fabTagIsAdd                    --
    , fabTagIsFloatOr                --
    , fabTagIsFloatXor               --
    , fabTagIsMin                    --
    , fabTagIsMax                    --
    , fabTagIsHsvAdjust              --
    , fabTagIsTranparent             --

    , fabTagTreeId                   --
    , fabTagTransformId              --
    , fabTagSubstanceRef             --
    , fabTagAboveId                  --
    , fabTagBelowId                  --
    , fabTagChildId                  --

    , makeCutPoint                   --
    , fromCutPoint                   --
    )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Serial.Reference

import Data.Bits
import Numeric

-- | The word "tag" is used to describe a bitfield that usually includes type metadata and pointers to other data.
-- | The word "case" is used here like "type" the fabric tags cast determines what type of fabric node it represents.
-- | The tags can be traversed in memory on different devices to traverse the DAG representing a scene.

type FabricNodeType_    = FabricTag_
type FabricNodeSubType_ = FabricTag_
type FabricSubType_     = FabricTag_
type FabricData_        = FabricTag_
type FabricHigh_        = StorageId_
type FabricLow_         = StorageId_

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

makeFabTagTree :: Reference (TreeRoot s) -> FabricTagId -> FabricTag
makeFabTagTree treeId childId =
    FabricTag
    $   fABRICiSuNARYpRE
    .|. fABRICiStREE
    .|. (makeFabHigh . unRef                 $ treeId )
    .|. (makeFabLow  . unRef . unFabricTagId $ childId)

makeFabTagTransform :: FabricSubType_ -> TransformId -> FabricTagId -> FabricTag
makeFabTagTransform ty transformId fabricTagId =
    FabricTag
    $   fABRICiSuNARYpRE
    .|. ty
    .|. (makeFabHigh .         unTransformId $ transformId)
    .|. (makeFabLow  . unRef . unFabricTagId $ fabricTagId)

makeFabTagTransformAffine   :: TransformId -> FabricTagId -> FabricTag
makeFabTagTransformFacet    :: TransformId -> FabricTagId -> FabricTag
makeFabTagTransformConvolve :: TransformId -> FabricTagId -> FabricTag
makeFabTagTransformAffine   = makeFabTagTransform fABRICiStRANSFORMaFFINE
makeFabTagTransformFacet    = makeFabTagTransform fABRICiStRANSFORMfACET
makeFabTagTransformConvolve = makeFabTagTransform fABRICiStRANSFORMcONVOLVE

makeFabTagFilter :: FabricSubType_ -> FabricTagId -> FabricTag
makeFabTagFilter ty childId =
    FabricTag
    $   fABRICiSuNARYpOST
    .|. ty
    .|. (makeFabLow  . unRef . unFabricTagId $ childId)

makeFabTagSqrt   :: FabricTagId -> FabricTag
makeFabTagInvert :: FabricTagId -> FabricTag
makeFabTagCos    :: FabricTagId -> FabricTag
makeFabTagSin    :: FabricTagId -> FabricTag
makeFabTagClamp  :: FabricTagId -> FabricTag
makeFabTagSqrt   = makeFabTagFilter fABRICiSsQRT
makeFabTagInvert = makeFabTagFilter fABRICiSiNVERT
makeFabTagCos    = makeFabTagFilter fABRICiScOS
makeFabTagSin    = makeFabTagFilter fABRICiSsIN
makeFabTagClamp  = makeFabTagFilter fABRICiScLAMP

makeBinaryOp :: FabricSubType_ -> FabricTagId -> FabricTagId -> FabricTag
makeBinaryOp ty parent child =
    FabricTag
    $   fABRICiSbINARY
    .|. ty
    .|. makeFabHigh (unRef . unFabricTagId $ parent)
    .|. makeFabLow  (unRef . unFabricTagId $ child )

makeFabTagComposite  :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagMult       :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagAdd        :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagFloatOr    :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagFloatXor   :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagMin        :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagMax        :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagHsvAdjust  :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagTranparent :: FabricTagId -> FabricTagId -> FabricTag
makeFabTagComposite  = makeBinaryOp fABRICiScOMPOSITE
makeFabTagMult       = makeBinaryOp fABRICiSmULT
makeFabTagAdd        = makeBinaryOp fABRICiSaDD
makeFabTagFloatOr    = makeBinaryOp fABRICiSfLOAToR
makeFabTagFloatXor   = makeBinaryOp fABRICiSfLOATxOR
makeFabTagMin        = makeBinaryOp fABRICiSmIN
makeFabTagMax        = makeBinaryOp fABRICiSmAX
makeFabTagHsvAdjust  = makeBinaryOp fABRICiShSVaDJUST
makeFabTagTranparent = makeBinaryOp fABRICiStRANSPARENT

makeSubstanceData :: FabricData_ -> SubstanceTag_
makeSubstanceData i = i .&. fABRICtAGdATAbITMASK

fromSubstanceData :: FabricTag_ -> FabricData_
fromSubstanceData tag = tag .&. fABRICtAGdATAbITMASK

makeSubstanceTagRef :: FabricSubType_ -> Reference t -> FabricTag
makeSubstanceTagRef ty ref = FabricTag $ ty .|. makeSubstanceData (fromIntegral . unRef $ ref)

makeSubstanceTag :: FabricSubType_ -> FabricTag
makeSubstanceTag ty = FabricTag $ ty

makeFabTagConstant  :: Reference t -> FabricTag
makeFabTagTexture   :: Reference t -> FabricTag
makeFabTagLinear    ::                FabricTag
makeFabTagQuadrance ::                FabricTag
makeFabTagConstant  = makeSubstanceTagRef fABRICiScONSTANT
makeFabTagTexture   = makeSubstanceTagRef fABRICiStEXTURE
makeFabTagLinear    = makeSubstanceTag    fABRICiSlINEAR
makeFabTagQuadrance = makeSubstanceTag    fABRICiSqUADRANCE

fabTagNodeType :: FabricTag -> FabricNodeType_
fabTagNodeType fabricTag = unFabricTag fabricTag .&. fABRICtYPEbITMASK

matchNodeType :: FabricNodeType_ -> FabricTag -> Bool
matchNodeType ty fabricTag = fabTagNodeType fabricTag == ty

fabTagIsLeaf      :: FabricTag -> Bool
fabTagIsUnaryPre  :: FabricTag -> Bool
fabTagIsUnaryPost :: FabricTag -> Bool
fabTagIsBinaryOp  :: FabricTag -> Bool
fabTagIsLeaf      = matchNodeType fABRICiSlEAF
fabTagIsUnaryPre  = matchNodeType fABRICiSuNARYpRE
fabTagIsUnaryPost = matchNodeType fABRICiSuNARYpOST
fabTagIsBinaryOp  = matchNodeType fABRICiSbINARY

fabTagSubType :: FabricTag -> FabricNodeSubType_
fabTagSubType fabricTag = unFabricTag fabricTag .&. fABRICsUBtYPEbITMASK

matchSubType :: FabricSubType_ -> FabricTag -> Bool
matchSubType match fabricTag = fabTagSubType fabricTag == match

substanceTagRef :: FabricTag -> FabricData_
substanceTagRef tag = unFabricTag tag .&. fABRICtAGdATAbITMASK

fabTagIsConstant   :: FabricTag -> Bool
fabTagIsTexture    :: FabricTag -> Bool
fabTagIsLinear     :: FabricTag -> Bool
fabTagIsQuadrance  :: FabricTag -> Bool
fabTagIsConstant  = matchSubType fABRICiScONSTANT
fabTagIsTexture   = matchSubType fABRICiStEXTURE
fabTagIsLinear    = matchSubType fABRICiSlINEAR
fabTagIsQuadrance = matchSubType fABRICiSqUADRANCE

fabTagIsTree              :: FabricTag -> Bool
fabTagIsTransformAffine   :: FabricTag -> Bool
fabTagIsTransformFacet    :: FabricTag -> Bool
fabTagIsTransformConvolve :: FabricTag -> Bool
fabTagIsTree              = matchSubType fABRICiStREE
fabTagIsTransformAffine   = matchSubType fABRICiStRANSFORMaFFINE
fabTagIsTransformFacet    = matchSubType fABRICiStRANSFORMfACET
fabTagIsTransformConvolve = matchSubType fABRICiStRANSFORMcONVOLVE

fabTagIsSqrt   :: FabricTag -> Bool
fabTagIsInvert :: FabricTag -> Bool
fabTagIsCos    :: FabricTag -> Bool
fabTagIsSin    :: FabricTag -> Bool
fabTagIsClamp  :: FabricTag -> Bool
fabTagIsSqrt   = matchSubType fABRICiSsQRT
fabTagIsInvert = matchSubType fABRICiSiNVERT
fabTagIsCos    = matchSubType fABRICiScOS
fabTagIsSin    = matchSubType fABRICiSsIN
fabTagIsClamp  = matchSubType fABRICiScLAMP

fabTagIsComposite :: FabricTag -> Bool
fabTagIsMult      :: FabricTag -> Bool
fabTagIsAdd       :: FabricTag -> Bool
fabTagIsFloatOr   :: FabricTag -> Bool
fabTagIsFloatXor  :: FabricTag -> Bool
fabTagIsComposite  = matchSubType fABRICiScOMPOSITE
fabTagIsMult       = matchSubType fABRICiSmULT
fabTagIsAdd        = matchSubType fABRICiSaDD
fabTagIsFloatOr    = matchSubType fABRICiSfLOAToR
fabTagIsFloatXor   = matchSubType fABRICiSfLOATxOR
fabTagIsMin        = matchSubType fABRICiSmIN
fabTagIsMax        = matchSubType fABRICiSmAX
fabTagIsHsvAdjust  = matchSubType fABRICiShSVaDJUST
fabTagIsTranparent = matchSubType fABRICiStRANSPARENT

fabTagTreeId :: FabricTag -> Reference (TreeRoot s)
fabTagTreeId = Ref . fromFabHigh . unFabricTag

fabTagTransformId :: FabricTag -> TransformId
fabTagTransformId = TransformId . fromFabHigh . unFabricTag

fabTagSubstanceRef :: FabricTag -> FabricData_
fabTagSubstanceRef = fromFabData . unFabricTag

fabTagAboveId :: FabricTag -> FabricTagId
fabTagAboveId = FabricTagId . Ref . fromFabHigh . unFabricTag

fabTagBelowId :: FabricTag -> FabricTagId
fabTagBelowId = fabTagChildId

fabTagChildId :: FabricTag -> FabricTagId
fabTagChildId = FabricTagId . Ref . fromFabLow . unFabricTag

makeLowCutPoint :: ShapeId_ -> FabricTag_
makeLowCutPoint = fromIntegral

makeCutPoint :: ShapeId -> FabricTag
makeCutPoint belowShape = FabricTag . makeLowCutPoint . unShapeId $ belowShape

fromLowCutPoint :: FabricTag_ -> ShapeId_
fromLowCutPoint tag = fromIntegral tag .&. nULLsHAPEiD

fromCutPoint :: FabricTag -> ShapeId
fromCutPoint (FabricTag tag) = ShapeId . fromLowCutPoint $ tag

showBinaryTag :: FabricTag -> String
showBinaryTag tag = (show . fabTagAboveId $ tag) ++ " X " ++ (show . fabTagChildId $ tag)

instance Show FabricTag where
    show tag
       | fabTagIsLeaf      tag && fabTagIsConstant           tag = "Constant"  ++ (show . fabTagSubstanceRef $ tag)
       | fabTagIsLeaf      tag && fabTagIsTexture            tag = "Texture"   ++ (show . fabTagSubstanceRef $ tag)
       | fabTagIsLeaf      tag && fabTagIsLinear             tag = "Linear"
       | fabTagIsLeaf      tag && fabTagIsQuadrance          tag = "Quadrance"
       | fabTagIsUnaryPre  tag && fabTagIsTree               tag = "Tree"      ++ " " ++ (show . fabTagTreeId      $ tag) ++ "->" ++ show (fabTagChildId tag)
       | fabTagIsUnaryPre  tag && fabTagIsTransformAffine    tag = "Affine"    ++ " " ++ (show . fabTagTransformId $ tag) ++ "->" ++ show (fabTagChildId tag)
       | fabTagIsUnaryPre  tag && fabTagIsTransformFacet     tag = "Facet"     ++ " " ++ (show . fabTagTransformId $ tag) ++ "->" ++ show (fabTagChildId tag)
       | fabTagIsUnaryPre  tag && fabTagIsTransformConvolve  tag = "Convolve"  ++ " " ++ (show . fabTagTransformId $ tag) ++ "->" ++ show (fabTagChildId tag)
       | fabTagIsUnaryPost tag && fabTagIsSqrt               tag = "Sqrt"
       | fabTagIsUnaryPost tag && fabTagIsInvert             tag = "Invert"
       | fabTagIsUnaryPost tag && fabTagIsCos                tag = "Cos"
       | fabTagIsUnaryPost tag && fabTagIsSin                tag = "Sin"
       | fabTagIsBinaryOp  tag && fabTagIsComposite          tag = "Composite"  ++ " " ++ showBinaryTag tag
       | fabTagIsBinaryOp  tag && fabTagIsMult               tag = "Mult"       ++ " " ++ showBinaryTag tag
       | fabTagIsBinaryOp  tag && fabTagIsAdd                tag = "Add"        ++ " " ++ showBinaryTag tag
       | fabTagIsBinaryOp  tag && fabTagIsFloatOr            tag = "FloatOr"    ++ " " ++ showBinaryTag tag
       | fabTagIsBinaryOp  tag && fabTagIsFloatXor           tag = "FloatXor"   ++ " " ++ showBinaryTag tag
       | fabTagIsBinaryOp  tag && fabTagIsMin                tag = "Min"        ++ " " ++ showBinaryTag tag
       | fabTagIsBinaryOp  tag && fabTagIsMax                tag = "Max"        ++ " " ++ showBinaryTag tag
       | fabTagIsBinaryOp  tag && fabTagIsHsvAdjust          tag = "HsvAdjust"  ++ " " ++ showBinaryTag tag
       | fabTagIsBinaryOp  tag && fabTagIsTranparent         tag = "Tranparent" ++ " " ++ showBinaryTag tag
