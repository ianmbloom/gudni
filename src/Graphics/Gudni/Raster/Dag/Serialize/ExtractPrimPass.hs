{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Serialize
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions used by TraverseShapeTree to serialize a scene into data buffers that can be parsed by
-- the rasterizer kernel and building a partitioned tree of tiles.

module Graphics.Gudni.Raster.Dag.Serialize.ExtractPrimPass
  ( ExtractPrimPass(..)
  , extractPrimPass
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.Fabric
import Graphics.Gudni.Raster.Dag.FromLayout
import Graphics.Gudni.Layout.Layout
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.FromLayout
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive
import Graphics.Gudni.Raster.Dag.PrimStorage
import Graphics.Gudni.Raster.Dag.PrimTag
import Graphics.Gudni.Raster.Dag.FabricStorage
import Graphics.Gudni.Raster.Dag.FabricTag
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.TreeStorage

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Lens

import Linear.V4

import Foreign.Storable
import qualified Data.Vector as V
import Data.Maybe

data ExtractPrimPass style

instance HasSpace style => HasSpace (ExtractPrimPass style) where
    type SpaceOf (ExtractPrimPass style) = SpaceOf style

instance HasSpace style => FabricType (ExtractPrimPass style) where
    type FChild     (ExtractPrimPass style) = (Maybe (Slice PrimTagId), Fabric (ExtractPrimPass style))
    type FTex       (ExtractPrimPass style) = PictureMemoryReference
    type FGeometry  (ExtractPrimPass style) = Slice PrimTagId
    type FQuery     (ExtractPrimPass style) = Color
    type FCombiner  (ExtractPrimPass style) = ProximityMeld style FCombineType

transBoundary :: Space s => FTransformer s -> Box s -> Box s
transBoundary trans box =
  let boxPoints = boxToV4Points box
  in  case trans of
         FAffineRay a -> boxOf $ fmap (applyAffine a) boxPoints
         FFacet facet -> boxOf $ facet ^. facetInput
         FFilter filt -> box
         FConvolve scale -> addMarginsBox scale box

buildMaybeTree :: ( MonadIO m
                  , Space s
                  , Storable s
                  )
               => FabricTagId
               -> Maybe (Slice PrimTagId)
               -> FabricTagId
               -> FabricMonad s m FabricTagId
buildMaybeTree parent mSlice childId =
  case mSlice of
      Just slice -> do treeId <- addTreeS slice
                       leafId <- addFabricS parent (FLeaf $ FGeometry $ FTree treeId childId)
                       return leafId
      Nothing -> return childId


extractPrimPass :: forall m style
                .  ( MonadIO m
                   , IsStyle style
                   )
                => Fabric (PicturePass style)
                -> FabricMonad (SpaceOf style) m FabricTagId
extractPrimPass fabric =
    do (mSlice, _, childId) <- go nullFabricTagId fabric
       buildMaybeTree nullFabricTagId mSlice childId
    where
    go :: FabricTagId
       -> Fabric (PicturePass style)
       -> FabricMonad (SpaceOf style) m ( Maybe (Slice PrimTagId)
                                        , Maybe (Box (SpaceOf style))
                                        , FabricTagId)
    go parent fabric =
        case fabric of
            FCombine ty a b ->
                do fabricTagId <- allocateFabricTagS
                   (mSliceA, mBoxA, childIdA) <- go fabricTagId a
                   (mSliceB, mBoxB, childIdB) <- go fabricTagId b
                   if proximityWillTransform (ty ^. proxType) && isJust mBoxA && isJust mBoxB
                   then do treeIdA <- buildMaybeTree fabricTagId mSliceA childIdA
                           treeIdB <- buildMaybeTree fabricTagId mSliceB childIdB
                           let transA = treeIdA
                               transB = treeIdB
                           setFabricS fabricTagId parent (FCombine (ty ^. proxMeld) treeIdA treeIdB)
                           return ( Nothing
                                  , eitherMaybe minMaxBox mBoxA mBoxB
                                  , fabricTagId)
                   else do setFabricS fabricTagId parent (FCombine (ty ^. proxMeld) childIdA childIdB)
                           return ( eitherMaybe combineSlices mSliceA mSliceB
                                  , eitherMaybe minMaxBox mBoxA mBoxB
                                  , fabricTagId)
            FTransform trans child ->
               do fabricTagId <- allocateFabricTagS
                  (mSlice, mBox, childId) <- go fabricTagId child
                  let tBox = fmap (transBoundary trans) mBox
                  leafId <- buildMaybeTree fabricTagId mSlice childId
                  setFabricS fabricTagId parent (FTransform trans leafId)
                  return (Nothing, tBox, fabricTagId)
            FLeaf leaf -> buildLeaf parent leaf

-- buildProximity treeIdA treeIdB
   -- do let (aP, bP) = applyProximity (ty ^. proxStyle) (ty ^. proxType) (fromJust mBoxA, fromJust mBoxB)
   --    transA <- addFabricS fabricTagId (FTransform (FAffineRay (affineTranslate $ negate aP)) treeIdA)
   --    transB <- addFabricS fabricTagId (FTransform (FAffineRay (affineTranslate $ negate bP)) treeIdB)
   --    rectA <- storePrimS (PrimRect (fromJust mBoxA))
   --    rectB <- storePrimS (PrimRect (fromJust mBoxb))

buildLeaf :: ( MonadIO m
             , IsStyle style
             )
          => FabricTagId
          -> FSubstance (PicturePass style)
          -> FabricMonad (SpaceOf style) m (Maybe (Slice PrimTagId), Maybe (Box (SpaceOf style)), FabricTagId)
buildLeaf parent leaf =
    case leaf of
        FGeometry (WithBox shape box) ->
            do let outlines = view shapeOutlines . mapOverPoints (fmap clampReasonable) $ shape
                   boundingBox = minMaxBoxes . fmap boxOf $ outlines
                   prims = V.concat .
                           map (V.map (PrimBezier parent) .
                                       view outlineSegments
                                       ) $
                                       outlines
               primTagIds <- V.mapM storePrimS prims
               slice <- foldIntoPileS dagPrimTagIds primTagIds
               return (Just slice, Just boundingBox, nullFabricTagId)
        _ -> do leafId <- addFabricS parent (FLeaf $ leafForStorage leaf)
                return (Nothing, Nothing, leafId)

leafForStorage :: FSubstance (PicturePass style) -> FSubstance (ForStorage (SpaceOf style))
leafForStorage leaf =
  case leaf of
    FGeometry s -> error "geometry id stored as null"
    FConst    q -> FConst    q
    FTexture  t -> FTexture  t
    FLinear     -> FLinear
    FQuadrance  -> FQuadrance
