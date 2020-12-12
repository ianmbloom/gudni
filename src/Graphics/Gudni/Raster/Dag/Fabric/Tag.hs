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
-- Functions for constructing the fabric DAG bytecode.

module Graphics.Gudni.Raster.Dag.Fabric.Tag
    ( makeFabTagDecoTree             --
    , makeFabTagConfineTree          --

    , makeFabTagStacker              --
    , makeFabTagAffine               --
    , makeFabTagFacet                --
    , makeFabTagConvolve             --

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

    , makeSubstanceTagRef            --

    , makeFabTagReturn               --
    , makeFabTagConstant             --
    , makeFabTagTexture              --

    , makeFabTagLinear               --
    , makeFabTagQuadrance            --

    , fabTagIsReturn                 --
    , fabTagIsConstant               --
    , fabTagIsTexture                --
    , fabTagIsFunction               --
    , fabTagIsBinary                 --
    , fabTagIsUnaryPost              --
    , fabTagIsDecoTree               --
    , fabTagIsConfineTree            --
    , fabTagIsStacker                --
    , fabTagIsAffine                 --
    , fabTagIsFacet                  --
    , fabTagIsConvolve               --

    , fabTagIsLinear                 --
    , fabTagIsQuadrance              --

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

    , fabTagDecoId                   --
    , fabTagConfineId                --
    , fabTagStackerId                --
    , fabTagTransformId              --
    , fabTagSubstanceRef             --
    )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Serial.Reference

import Data.Bits
import Numeric

-- | The word "tag" is used to describe a bitfield that usually includes type metadata and pointers to other data.
-- | The word "case" is used here like "type" the fabric tags cast determines what type of fabric node it represents.
-- | The tags can be traversed in memory on different devices to traverse the DAG representing a scene.

type FabricNodeType_ = FabricTag_
type FabricSubType_  = FabricTag_
type FabricData_     = FabricTag_

fromFabData :: FabricTag_ -> FabricData_
fromFabData tag = tag .&. fABRICtAGdATAbITMASK

toFabData :: FabricTag_ -> FabricData_
toFabData i = i .&. fABRICtAGdATAbITMASK

makeFabTagDecoTree :: DecoTagId s -> FabricTag
makeFabTagDecoTree decoId =
    FabricTag
    $   fABRICiSdECOtREE
    .|. (toFabData . unRef . unDecoTagId $ decoId )

makeFabTagConfineTree :: ConfineTagId s -> FabricTag
makeFabTagConfineTree confineId =
    FabricTag
    $   fABRICiScONFINEtREE
    .|. (toFabData . unRef . unConfineTagId $ confineId)

makeFabTagStacker :: FabricTagId -> FabricTag
makeFabTagStacker tagId =
    FabricTag
    $   fABRICiSsTACKER
    .|. (toFabData . unRef . unFabricTagId $ tagId   )

makeFabTagTransform :: FabricSubType_ -> TransformId -> FabricTag
makeFabTagTransform ty transformId =
    FabricTag
    $   ty
    .|. (toFabData . unTransformId $ transformId)

makeFabTagAffine   :: TransformId -> FabricTag
makeFabTagFacet    :: TransformId -> FabricTag
makeFabTagConvolve :: TransformId -> FabricTag
makeFabTagAffine   = makeFabTagTransform fABRICiSaFFINE
makeFabTagFacet    = makeFabTagTransform fABRICiSfACET
makeFabTagConvolve = makeFabTagTransform fABRICiScONVOLVE

makeFabTagFilter :: FabricSubType_ -> FabricTag
makeFabTagFilter ty =
    FabricTag
    $   fABRICiSuNARYpOST
    .|. ty

makeFabTagSqrt   :: FabricTag
makeFabTagInvert :: FabricTag
makeFabTagCos    :: FabricTag
makeFabTagSin    :: FabricTag
makeFabTagClamp  :: FabricTag
makeFabTagSqrt   = makeFabTagFilter fABRICiSsQRT
makeFabTagInvert = makeFabTagFilter fABRICiSiNVERT
makeFabTagCos    = makeFabTagFilter fABRICiScOS
makeFabTagSin    = makeFabTagFilter fABRICiSsIN
makeFabTagClamp  = makeFabTagFilter fABRICiScLAMP

makeBinaryOp :: FabricSubType_ -> FabricTag
makeBinaryOp ty =
    FabricTag
    $   fABRICiSbINARY
    .|. ty

makeFabTagComposite  :: FabricTag
makeFabTagMult       :: FabricTag
makeFabTagAdd        :: FabricTag
makeFabTagFloatOr    :: FabricTag
makeFabTagFloatXor   :: FabricTag
makeFabTagMin        :: FabricTag
makeFabTagMax        :: FabricTag
makeFabTagHsvAdjust  :: FabricTag
makeFabTagTranparent :: FabricTag
makeFabTagComposite  = makeBinaryOp fABRICiScOMPOSITE
makeFabTagMult       = makeBinaryOp fABRICiSmULT
makeFabTagAdd        = makeBinaryOp fABRICiSaDD
makeFabTagFloatOr    = makeBinaryOp fABRICiSfLOAToR
makeFabTagFloatXor   = makeBinaryOp fABRICiSfLOATxOR
makeFabTagMin        = makeBinaryOp fABRICiSmIN
makeFabTagMax        = makeBinaryOp fABRICiSmAX
makeFabTagHsvAdjust  = makeBinaryOp fABRICiShSVaDJUST
makeFabTagTranparent = makeBinaryOp fABRICiStRANSPARENT

makeSubstanceTagRef :: FabricSubType_ -> Reference t -> FabricTag
makeSubstanceTagRef ty ref = FabricTag $ ty .|. toFabData (fromIntegral . unRef $ ref)

makeFabTagReturn :: FabricTag
makeFabTagReturn = FabricTag fABRICiSrETURN

makeFabTagConstant  :: Reference t -> FabricTag
makeFabTagTexture   :: Reference t -> FabricTag
makeFabTagConstant  = makeSubstanceTagRef fABRICiScONSTANT
makeFabTagTexture   = makeSubstanceTagRef fABRICiStEXTURE

makeFunctionTag :: FabricSubType_ -> FabricTag
makeFunctionTag ty = FabricTag $ fABRICiSfUNCTION .|. ty

makeFabTagLinear    :: FabricTag
makeFabTagQuadrance :: FabricTag
makeFabTagLinear    = makeFunctionTag fABRICiSlINEAR
makeFabTagQuadrance = makeFunctionTag fABRICiSqUADRANCE

fabTagNodeType :: FabricTag -> FabricNodeType_
fabTagNodeType fabricTag = unFabricTag fabricTag .&. fABRICtYPEbITMASK

matchNodeType :: FabricNodeType_ -> FabricTag -> Bool
matchNodeType ty fabricTag = fabTagNodeType fabricTag == ty

fabTagIsReturn      :: FabricTag -> Bool
fabTagIsConstant    :: FabricTag -> Bool
fabTagIsTexture     :: FabricTag -> Bool
fabTagIsFunction    :: FabricTag -> Bool
fabTagIsBinary      :: FabricTag -> Bool
fabTagIsUnaryPost   :: FabricTag -> Bool
fabTagIsDecoTree    :: FabricTag -> Bool
fabTagIsConfineTree :: FabricTag -> Bool
fabTagIsStacker     :: FabricTag -> Bool
fabTagIsAffine      :: FabricTag -> Bool
fabTagIsFacet       :: FabricTag -> Bool
fabTagIsConvolve    :: FabricTag -> Bool
fabTagIsReturn      = matchNodeType fABRICiSrETURN
fabTagIsConstant    = matchNodeType fABRICiScONSTANT
fabTagIsTexture     = matchNodeType fABRICiStEXTURE
fabTagIsFunction    = matchNodeType fABRICiSfUNCTION
fabTagIsBinary      = matchNodeType fABRICiSbINARY
fabTagIsUnaryPost   = matchNodeType fABRICiSuNARYpOST
fabTagIsDecoTree    = matchNodeType fABRICiSdECOtREE
fabTagIsConfineTree = matchNodeType fABRICiScONFINEtREE
fabTagIsStacker     = matchNodeType fABRICiSsTACKER
fabTagIsAffine      = matchNodeType fABRICiSaFFINE
fabTagIsFacet       = matchNodeType fABRICiSfACET
fabTagIsConvolve    = matchNodeType fABRICiScONVOLVE

fabTagSubType :: FabricTag -> FabricSubType_
fabTagSubType fabricTag = unFabricTag fabricTag .&. fABRICtAGdATAbITMASK

matchSubType :: FabricSubType_ -> FabricTag -> Bool
matchSubType match fabricTag = fabTagSubType fabricTag == match

fabTagIsLinear    :: FabricTag -> Bool
fabTagIsQuadrance :: FabricTag -> Bool
fabTagIsLinear    = matchSubType fABRICiSlINEAR
fabTagIsQuadrance = matchSubType fABRICiSqUADRANCE

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

fabTagIsComposite  :: FabricTag -> Bool
fabTagIsMult       :: FabricTag -> Bool
fabTagIsAdd        :: FabricTag -> Bool
fabTagIsFloatOr    :: FabricTag -> Bool
fabTagIsFloatXor   :: FabricTag -> Bool
fabTagIsMin        :: FabricTag -> Bool
fabTagIsMax        :: FabricTag -> Bool
fabTagIsHsvAdjust  :: FabricTag -> Bool
fabTagIsTranparent :: FabricTag -> Bool
fabTagIsComposite  = matchSubType fABRICiScOMPOSITE
fabTagIsMult       = matchSubType fABRICiSmULT
fabTagIsAdd        = matchSubType fABRICiSaDD
fabTagIsFloatOr    = matchSubType fABRICiSfLOAToR
fabTagIsFloatXor   = matchSubType fABRICiSfLOATxOR
fabTagIsMin        = matchSubType fABRICiSmIN
fabTagIsMax        = matchSubType fABRICiSmAX
fabTagIsHsvAdjust  = matchSubType fABRICiShSVaDJUST
fabTagIsTranparent = matchSubType fABRICiStRANSPARENT


fabTagDecoId :: FabricTag -> DecoTagId s
fabTagDecoId = DecoTagId . Ref . fromFabData . unFabricTag

fabTagConfineId :: FabricTag -> ConfineTagId s
fabTagConfineId = ConfineTagId . Ref . fromFabData . unFabricTag

fabTagStackerId :: FabricTag -> FabricTagId
fabTagStackerId = FabricTagId . Ref . fromFabData . unFabricTag

fabTagTransformId :: FabricTag -> TransformId
fabTagTransformId = TransformId . fromFabData . unFabricTag

fabTagSubstanceRef :: FabricTag -> FabricData_
fabTagSubstanceRef = fromFabData . unFabricTag


instance Show FabricTag where
    show tag
       | fabTagIsReturn      tag                           = "Return"
       | fabTagIsConstant    tag                           = "Constant"    ++ "->" ++ (show . fabTagSubstanceRef $ tag)
       | fabTagIsTexture     tag                           = "Texture"     ++ "->" ++ (show . fabTagSubstanceRef $ tag)
       | fabTagIsFunction    tag && fabTagIsLinear     tag = "Linear"
       | fabTagIsFunction    tag && fabTagIsQuadrance  tag = "Quadrance"
       | fabTagIsDecoTree    tag                           = "DecoTree"    ++ "->" ++ (show . fabTagDecoId      $ tag)
       | fabTagIsConfineTree tag                           = "ConfineTree" ++ "->" ++ (show . fabTagConfineId   $ tag)
       | fabTagIsStacker     tag                           = "Stacker"     ++ "->" ++ (show . fabTagStackerId   $ tag)
       | fabTagIsAffine      tag                           = "Affine"      ++ "->" ++ (show . fabTagTransformId $ tag)
       | fabTagIsFacet       tag                           = "Facet"       ++ "->" ++ (show . fabTagTransformId $ tag)
       | fabTagIsConvolve    tag                           = "Convolve"    ++ "->" ++ (show . fabTagTransformId $ tag)
       | fabTagIsUnaryPost tag && fabTagIsSqrt         tag = "Sqrt"
       | fabTagIsUnaryPost tag && fabTagIsInvert       tag = "Invert"
       | fabTagIsUnaryPost tag && fabTagIsCos          tag = "Cos"
       | fabTagIsUnaryPost tag && fabTagIsSin          tag = "Sin"
       | fabTagIsBinary    tag && fabTagIsComposite    tag = "Composite"
       | fabTagIsBinary    tag && fabTagIsMult         tag = "Mult"
       | fabTagIsBinary    tag && fabTagIsAdd          tag = "Add"
       | fabTagIsBinary    tag && fabTagIsFloatOr      tag = "FloatOr"
       | fabTagIsBinary    tag && fabTagIsFloatXor     tag = "FloatXor"
       | fabTagIsBinary    tag && fabTagIsMin          tag = "Min"
       | fabTagIsBinary    tag && fabTagIsMax          tag = "Max"
       | fabTagIsBinary    tag && fabTagIsHsvAdjust    tag = "HsvAdjust"
       | fabTagIsBinary    tag && fabTagIsTranparent   tag = "Tranparent"
