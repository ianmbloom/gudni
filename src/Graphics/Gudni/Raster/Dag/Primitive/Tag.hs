{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.ConfineTree.PrimTag
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructors for attaching metadata to shapes∘

module Graphics.Gudni.Raster.Dag.Primitive.Tag
    ( makeFacetPrimTag
    , makeBezierPrimTag
    , makeRectPrimTag
    , makeElipsePrimTag
    , primTagType
    , primTagIsBezier
    , primTagIsFacet
    , primTagIsRect
    , primTagIsElipse
    , primTagBezierId
    , primTagFacetId
    , primTagBoxId
    , primTagShapeId
    )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Thresholds.SubstanceInfo

import Data.Bits
import Numeric

import Control.Lens

type PrimType_ = PrimTag_

makePrimStorage :: StorageId_ -> PrimTag_
makePrimStorage i = (fromIntegral i `shiftL` pRIMtAGsTORAGEiDsHIFT) .&. pRIMtAGsTORAGEiDbITMASK

makePrimShapeId :: ShapeId_ -> PrimTag_
makePrimShapeId i = fromIntegral i .&. pRIMtAGfABRICiDbITMASK

fromPrimStorage :: PrimTag_ -> StorageId_
fromPrimStorage i = fromIntegral $ i `shiftR` pRIMtAGsTORAGEiDsHIFT

fromPrimShapeId :: PrimTag_ -> ShapeId_
fromPrimShapeId i = fromIntegral $ i .&. pRIMtAGfABRICiDbITMASK

primTagType :: PrimTag -> PrimType_
primTagType tag = unPrimTag tag .&. pRIMtAGtYPEbITMASK


-- Facets point to ShapeIds
makeFacetPrimTag :: FacetId s -> ShapeId -> PrimTag
makeFacetPrimTag facetId shapeId =
    PrimTag $ pRIMtAGiSfACET
           .|. (makePrimStorage . unRef . unFacetId $ facetId )
           .|. (makePrimShapeId . unShapeId $ shapeId )

-- Other shapes point to compoundTagIds
makeBasicPrim :: StorageId_ -> ShapeId -> PrimType_ -> PrimTag
makeBasicPrim storageId shapeId ty =
    PrimTag $  ty
           .|. makePrimStorage storageId
           .|. (makePrimShapeId . unShapeId $ shapeId)

makeBezierPrimTag :: BezierId s -> ShapeId -> PrimTag
makeBezierPrimTag bezId shapeId =  makeBasicPrim (unRef . unBezierId $ bezId) shapeId pRIMtAGiSbEZIER

makeRectPrimTag :: BoxId s -> ShapeId -> PrimTag
makeRectPrimTag rectId shapeId =  makeBasicPrim (unRef . unBoxId $ rectId) shapeId pRIMtAGiSrECTANGLE

makeElipsePrimTag :: BoxId s -> ShapeId -> PrimTag
makeElipsePrimTag rectId shapeId = makeBasicPrim (unRef . unBoxId $ rectId) shapeId pRIMtAGiSeLIPSE

primTagIsBezier :: PrimTag -> Bool
primTagIsBezier    tag = primTagType tag == pRIMtAGiSbEZIER
primTagIsFacet  :: PrimTag -> Bool
primTagIsFacet     tag = primTagType tag == pRIMtAGiSfACET
primTagIsRect :: PrimTag -> Bool
primTagIsRect tag = primTagType tag == pRIMtAGiSrECTANGLE
primTagIsElipse :: PrimTag -> Bool
primTagIsElipse    tag = primTagType tag == pRIMtAGiSeLIPSE

primTagBezierId :: PrimTag -> BezierId s
primTagBezierId = BezierId . Ref . fromPrimStorage . unPrimTag

primTagFacetId :: PrimTag -> FacetId s
primTagFacetId = FacetId . Ref . fromPrimStorage . unPrimTag

primTagBoxId :: PrimTag -> BoxId s
primTagBoxId = BoxId . Ref . fromPrimStorage . unPrimTag

primTagShapeId :: PrimTag -> ShapeId
primTagShapeId = ShapeId . fromPrimShapeId . unPrimTag

instance Show PrimTag where
  show tag = show (primTagType tag `shiftR` 60, primTagBezierId tag, primTagShapeId tag)
