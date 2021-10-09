
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Transformer.Storage
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for storing and loading transformer tags and their corresponding
-- Description data.

module Graphics.Gudni.Raster.Fabric.Transformer.Storage
  ( storeTransform
  )
where

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile
import Graphics.Gudni.Raster.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Fabric.Tag
import Graphics.Gudni.Raster.Fabric.Storage
import Graphics.Gudni.Raster.TagTypes

import Graphics.Gudni.Raster.Storage

storeTransform :: ( DagConstraints s m
                  )
               => FTransformer s
               -> FabricStorageMonad s m FabricTag
storeTransform trans =
    case trans of
        FAffine forward back ->
             do  affineRef <- addToPileS fabricHeapPile (AsBytes (forward, back))
                 return $ makeFabTagAffine (TransformId . unRef $ affineRef)
        FFacet facet ->
             do  facetRef <- addToPileS fabricHeapPile (AsBytes facet)
                 return $ makeFabTagFacet (TransformId . unRef $ facetRef)
        FConvolve scale ->
             do  scaleRef <- addToPileS fabricHeapPile (AsBytes scale)
                 return $ makeFabTagConvolve (TransformId . unRef $ scaleRef)
