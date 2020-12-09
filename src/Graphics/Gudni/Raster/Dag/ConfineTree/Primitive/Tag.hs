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

module Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Tag
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
    , primTagFabricTagId
    )
where

import Graphics.Gudni.Figure

--import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Constants
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Serial.Reference


import Graphics.Gudni.Raster.Thresholds.SubstanceInfo

import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Type

import Data.Bits
import Numeric

import Control.Lens

type PrimType_ = PrimTag_

makePrimStorage :: StorageId_ -> PrimTag_
makePrimStorage i = (fromIntegral i `shiftL` pRIMtAGsTORAGEiDsHIFT) .&. pRIMtAGsTORAGEiDbITMASK

makePrimFabricTagId :: Reference FabricTag -> PrimTag_
makePrimFabricTagId i = fromIntegral i .&. pRIMtAGfABRICiDbITMASK

fromPrimStorage :: PrimTag_ -> StorageId_
fromPrimStorage i = fromIntegral $ i `shiftR` pRIMtAGsTORAGEiDsHIFT

fromPrimFabricTagId :: PrimTag_ -> Reference FabricTag
fromPrimFabricTagId i = fromIntegral $ i .&. pRIMtAGfABRICiDbITMASK

primTagType :: PrimTag -> PrimType_
primTagType tag = unPrimTag tag .&. pRIMtAGtYPEbITMASK


-- Facets point to ShapeIds
makeFacetPrimTag :: FacetId s -> FabricTagId -> PrimTag
makeFacetPrimTag facetId fabricTagId =
    PrimTag $ pRIMtAGiSfACET
           .|. (makePrimStorage . unRef . unFacetId $ facetId )
           .|. (makePrimFabricTagId . unFabricTagId $ fabricTagId )

-- Other shapes point to compoundTagIds
makeBasicPrim :: StorageId_ -> FabricTagId -> PrimType_ -> PrimTag
makeBasicPrim storageId fabricTagId ty =
    PrimTag $  ty
           .|. makePrimStorage storageId
           .|. (makePrimFabricTagId . unFabricTagId $ fabricTagId)

makeBezierPrimTag :: BezierId s -> FabricTagId -> PrimTag
makeBezierPrimTag bezId fabricTagId =  makeBasicPrim (unRef . unBezierId $ bezId) fabricTagId pRIMtAGiSbEZIER

makeRectPrimTag :: BoxId s -> FabricTagId -> PrimTag
makeRectPrimTag rectId fabricTagId =  makeBasicPrim (unRef . unBoxId $ rectId) fabricTagId pRIMtAGiSrECTANGLE

makeElipsePrimTag :: BoxId s -> FabricTagId -> PrimTag
makeElipsePrimTag rectId fabricTagId = makeBasicPrim (unRef . unBoxId $ rectId) fabricTagId pRIMtAGiSeLIPSE

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

primTagFabricTagId :: PrimTag -> FabricTagId
primTagFabricTagId = FabricTagId . fromPrimFabricTagId . unPrimTag

instance Show PrimTag where
  show tag = show (primTagType tag `shiftR` 60, primTagBezierId tag, primTagFabricTagId tag)
