
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Fabric.Combine.Tag
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for converting combine type two and from fabricTags

module Graphics.Gudni.Raster.Dag.Fabric.Combine.Tag
   ( combineTypeToFabTag
   , fabTagCombineType
   )
where

import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.TagTypes

combineTypeToFabTag :: FCombineType -> FabricTag
combineTypeToFabTag op =
    case op of
        FComposite   -> makeFabTagComposite  --
        FMask        -> makeFabTagMult       --
        FAdd         -> makeFabTagAdd        --
        FFloatOr     -> makeFabTagFloatOr    --
        FFloatXor    -> makeFabTagFloatXor   --
        FMin         -> makeFabTagMin        --
        FMax         -> makeFabTagMax        --
        FHsvAdjust   -> makeFabTagHsvAdjust  --
        FTransparent -> makeFabTagTranparent --

fabTagCombineType :: FabricTag -> FCombineType
fabTagCombineType tag
    | fabTagIsComposite  tag = FComposite   --
    | fabTagIsMult       tag = FMask        --
    | fabTagIsAdd        tag = FAdd         --
    | fabTagIsFloatOr    tag = FFloatOr     --
    | fabTagIsFloatXor   tag = FFloatXor    --
    | fabTagIsMin        tag = FMin         --
    | fabTagIsMax        tag = FMax         --
    | fabTagIsHsvAdjust  tag = FHsvAdjust   --
    | fabTagIsTranparent tag = FTransparent --
