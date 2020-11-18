{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.FromLayout
import Graphics.Gudni.Layout.Layout
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.FromLayout
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox

import Graphics.Gudni.Raster.Thresholds.Constants
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Storage
import Graphics.Gudni.Raster.Dag.Primitive.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Transformer
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.MonadUnique
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
    type FRootType     (ExtractPrimPass style) = Fabric (ExtractPrimPass style)
    type FChildType    (ExtractPrimPass style) = (Maybe (Slice PrimTagId), Fabric (ExtractPrimPass style), ShapeId)
    type FLeafType     (ExtractPrimPass style) = FLeaf (PicturePass style)
    type FCombinerType (ExtractPrimPass style) = ProximityMeld style FCombineType

instance HasSpace style => SubstanceType (ExtractPrimPass style) where
    type FTex      (ExtractPrimPass style) = PictureMemoryReference
    type FQuery    (ExtractPrimPass style) = Color (SpaceOf style)

transBoundary :: Space s => FTransformer s -> Box s -> Box s
transBoundary trans box =
  let boxPoints = boxToV4Points box
  in  case trans of
          FAffine ray back -> boxOf $ fmap (applyAffine back) boxPoints
          FFacet facet -> boxOf $ facet ^. facetInput
          FFilter filt -> box
          FConvolve scale -> addMarginsBox scale box

buildMaybeTree :: (DagConstraints s m)
               => Maybe (Slice PrimTagId)
               -> FabricTagId
               -> DagMonad s m FabricTagId
buildMaybeTree mSlice childId =
  case mSlice of
      Just slice -> do treeId <- addTreeS slice
                       leafId <- addFabricS (FLeaf $ FTree treeId childId)
                       return leafId
      Nothing -> return childId

combineMinimums :: FCombineType -> ShapeId -> ShapeId -> ShapeId
combineMinimums op a b
   | a == nullShapeId &&
     b == nullShapeId    = nullShapeId
   | b == nullShapeId    = case op of
                             FMask -> a
                             _     -> nullShapeId
   | b == nullShapeId    = case op of
                               FComposite -> b
                               _     -> nullShapeId
   | otherwise           = min a b

extractPrimPass :: forall m style
                .  ( MonadIO m
                   , IsStyle style
                   )
                => Fabric (PicturePass style)
                -> DagMonad (SpaceOf style) (UniqueT m) FabricTagId
extractPrimPass fabric =
    do (mSlice, _, childId, _) <- go fabric
       buildMaybeTree mSlice childId
    where
    go :: Fabric (PicturePass style)
       -> DagMonad (SpaceOf style) (UniqueT m)
                   ( Maybe (Slice PrimTagId)
                   , Maybe (Box (SpaceOf style))
                   , FabricTagId
                   , ShapeId
                   )
    go fabric =
        case fabric of
            FCombine ty a b ->
                do  fabricTagId <- allocateFabricCombineTagS
                    (mSliceA, mBoxA, childIdA, childShapeMinA) <- go a
                    (mSliceB, mBoxB, childIdB, childShapeMinB) <- go b
                    let shapeMin = combineMinimums (ty ^. proxMeld) childShapeMinA childShapeMinB
                        melder = (ty ^. proxMeld, childShapeMinA, childShapeMinB)
                        combinedBoxes = eitherMaybe minMaxBox mBoxA mBoxB
                    --if proximityWillTransform (ty ^. proxType) && isJust mBoxA && isJust mBoxB
                    --then do treeIdA <- buildMaybeTree fabricTagId mSliceA childIdA
                    --        treeIdB <- buildMaybeTree fabricTagId mSliceB childIdB
                    --        let transA = treeIdA
                    --            transB = treeIdB
                    --        setFabricS fabricTagId (WithParent parent $ FCombine melder treeIdA treeIdB)
                    --        return ( Nothing
                    --               , combinedBoxes
                    --               , fabricTagId
                    --               , shapeMin
                    --               )
                    -- else
                    setFabricS fabricTagId (FCombine melder childIdA childIdB)
                    return ( eitherMaybe combineSlices mSliceA mSliceB
                           , combinedBoxes
                           , fabricTagId
                           , shapeMin
                           )
            FTransform trans child ->
                do  fabricTagId <- allocateFabricTagS
                    (mSlice, mBox, childId, childShapeId) <- go child
                    let tBox = fmap (transBoundary trans) mBox
                    leafId <- buildMaybeTree mSlice childId
                    setFabricS fabricTagId (FTransform trans leafId)
                    return ( Nothing
                           , tBox
                           , fabricTagId
                           , nullShapeId
                           )
            FLeaf leaf -> buildLeaf leaf

-- buildProximity treeIdA treeIdB
   -- do let (aP, bP) = applyProximity (ty ^. proxStyle) (ty ^. proxType) (fromJust mBoxA, fromJust mBoxB)
   --    transA <- addFabricS fabricTagId (FTransform (FAffine (affineTranslate $ negate aP)) treeIdA)
   --    transB <- addFabricS fabricTagId (FTransform (FAffine (affineTranslate $ negate bP)) treeIdB)
   --    rectA <- storePrimS (PrimRect (fromJust mBoxA))
   --    rectB <- storePrimS (PrimRect (fromJust mBoxb))


newShapeId :: Monad m => DagMonad s (UniqueT m) ShapeId
newShapeId = ShapeId . fromIntegral <$> lift fresh

buildLeaf :: ( MonadIO m
             , IsStyle style
             )
          => FLeafType (PicturePass style)
          -> DagMonad (SpaceOf style) (UniqueT m)
                      ( Maybe (Slice PrimTagId)
                      , Maybe (Box (SpaceOf style))
                      , FabricTagId
                      , ShapeId
                      )
buildLeaf leaf =
    case leaf of
        FShape (WithBox shape box) ->
            do shapeId <- newShapeId
               let outlines = view shapeOutlines . mapOverPoints (fmap clampReasonable) $ shape
                   prims = V.concat .
                           map (V.map (Prim shapeId . PrimBezier) .
                                       view outlineSegments
                                       ) $
                                       outlines
                   boundingBox = minMaxBoxes . fmap boxOf $ prims
               primTagIds <- V.mapM storePrimS prims
               slice <- foldIntoPileS dagPrimTagIds primTagIds
               return (Just slice, Just boundingBox, nullFabricTagId, shapeId)
        FSubstance substance ->
            do leafId <- addFabricS (FLeaf . FTreeSubstance . substanceForStorage $ substance)
               return (Nothing, Nothing, leafId, nullShapeId)

substanceForStorage :: FSubstance (PicturePass style) -> FSubstance (ForStorage (SpaceOf style))
substanceForStorage leaf =
  case leaf of
    FConst    q -> FConst    q
    FTexture  t -> FTexture  t
    FLinear     -> FLinear
    FQuadrance  -> FQuadrance
