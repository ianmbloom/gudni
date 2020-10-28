{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Raster.Dag.Fabric.Storage
  ( FabricStorage(..)
  , fabricTagPile
  , fabricParentPile
  , fabricHeapPile
  , initFabricStorage
  , freeFabricStorage
  , allocateFabricTag
  , allocateFabricCombineTag
  , addFabric
  , storeFabric
  , loadFabric
  , ForStorage(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Filter
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Transformer
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Tag
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Tag
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Util.Util

import Foreign.Storable
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

type FabricStorageMonad s m q = StateT (FabricStorage s) m q

data FabricStorage s
    = FabricStorage
      -- | A list of every tag defining the fabric (or structure of the scene DAG)
    { _fabricTagPile    :: Pile FabricTag
      -- | A list of every parent of every fabric tag.
    , _fabricParentPile :: Pile FabricTagId
      -- | A heap of all extra data used by the dag including transformations and boundaries
    , _fabricHeapPile   :: BytePile
    }
makeLenses ''FabricStorage

instance Space s => SubstanceType (ForStorage s) where
    type FTex   (ForStorage s) = PictureMemoryReference
    type FQuery (ForStorage s) = Color s


initFabricStorage :: MonadIO m => m (FabricStorage s)
initFabricStorage =
   do fabricTagPile    <- newPile
      fabricParentPile <- newPile
      fabricHeapPile   <- newPile
      return $ FabricStorage
               { _fabricTagPile    = fabricTagPile
               , _fabricParentPile = fabricParentPile
               , _fabricHeapPile   = fabricHeapPile
               }

freeFabricStorage :: MonadIO m => FabricStorage s -> m ()
freeFabricStorage storage =
  do freePile $ storage ^. fabricTagPile
     freePile $ storage ^. fabricParentPile
     freePile $ storage ^. fabricHeapPile

addFabric :: ( MonadIO m
             , Storable s
             , Space s
             )
          => WithParent (ForStorage s)
          -> FabricStorageMonad s m FabricTagId
addFabric fabric =
   case fabric of
       WithParent _ (FCombine {}) -> error "shouldn't be adding combine use allocateFabricCombineTag"
       _ -> do tagId <- allocateFabricTag
               storeFabric tagId fabric
               return tagId

storeFabricParent :: MonadIO m => FabricTagId -> FabricTagId -> FabricStorageMonad s m ()
storeFabricParent fabricTagId pFabricTagId = toPileS fabricParentPile (Ref . unRef . unFabricTagId $ fabricTagId) pFabricTagId

storeLimit :: MonadIO m => FabricTagId -> ShapeId -> ShapeId -> FabricStorageMonad s m ()
storeLimit fabricTagId aboveShape belowShape = toPileS fabricTagPile (unFabricTagId fabricTagId + 1) (makeLimits aboveShape belowShape)

storeFabric :: ( MonadIO m
               , Storable s
               , Space s
               )
            => FabricTagId
            -> WithParent (ForStorage s)
            -> FabricStorageMonad s m ()
storeFabric fabricTagId (WithParent parent fabric) =
  do storeFabricParent fabricTagId parent
     tag <- case fabric of
                FTransform trans child ->
                    case trans of
                        FAffine forward back ->
                             do  affineRef <- addToPileS fabricHeapPile (AsBytes (forward, back))
                                 return $ makeFabTagTransformAffine (TransformId . unRef $ affineRef) child
                        FFacet facet ->
                             do  facetRef <- addToPileS fabricHeapPile (AsBytes facet)
                                 return $ makeFabTagTransformFacet (TransformId . unRef $ facetRef) child
                        FFilter filt ->
                             do  filtRef <- addToPileS fabricHeapPile (AsBytes $ fromEnum filt)
                                 return $ makeFabTagTransformFilter (TransformId . unRef $ filtRef) child
                        FConvolve scale ->
                             do  scaleRef <- addToPileS fabricHeapPile (AsBytes scale)
                                 return $ makeFabTagTransformConvolve (TransformId . unRef $ scaleRef) child
                FLeaf leaf ->
                    case leaf of
                      FTree treeId childId ->
                          return $ makeFabTagTree treeId childId
                      FTreeSubstance substance ->
                          do  substanceTag <- overStateT fabricHeapPile $ storeSubstance substance
                              return $ makeFabTagSubstance substanceTag
                FCombine (op, aboveShape, belowShape) aboveId belowId ->
                     do storeLimit fabricTagId aboveShape belowShape
                        return $ opToFabTag op aboveId belowId
     toPileS fabricTagPile (unFabricTagId fabricTagId) tag

allocateFabricParent :: MonadIO m => FabricStorageMonad s m FabricTagId
allocateFabricParent = FabricTagId . Ref . unRef <$> allocatePileS fabricParentPile

allocateFabricCombineTag :: MonadIO m => FabricStorageMonad s m FabricTagId
allocateFabricCombineTag = do fabricTagId <- FabricTagId <$> allocatePileS fabricTagPile
                              allocatePileS fabricTagPile
                              return fabricTagId

allocateFabricTag :: MonadIO m => FabricStorageMonad s m FabricTagId
allocateFabricTag = FabricTagId <$> allocatePileS fabricTagPile

loadFabricParent :: MonadIO m => FabricTagId -> FabricStorageMonad s m FabricTagId
loadFabricParent  fabricTagId = fromPileS fabricParentPile (Ref . unRef . unFabricTagId $ fabricTagId)

loadLimit :: MonadIO m => FabricTagId -> FabricStorageMonad s m (ShapeId, ShapeId)
loadLimit  fabricTagId = fromLimits <$> fromPileS fabricTagPile (unFabricTagId fabricTagId + 1)

opToFabTag :: FCombineType -> FabricTagId -> FabricTagId -> FabricTag
opToFabTag op parentId childId =
    case op of
        FComposite -> makeFabTagComposite parentId childId
        FMask      -> makeFabTagMult      parentId childId
        FAdd       -> makeFabTagAdd       parentId childId
        FFloatOr   -> makeFabTagFloatOr   parentId childId
        FFloatXor  -> makeFabTagFloatXor  parentId childId

fabTagOp :: FabricTag -> FCombineType
fabTagOp tag
       | fabTagIsComposite tag = FComposite
       | fabTagIsMult      tag = FMask
       | fabTagIsAdd       tag = FAdd
       | fabTagIsFloatOr   tag = FFloatOr
       | fabTagIsFloatXor  tag = FFloatXor

loadFabric :: forall s m
           .  ( MonadIO m
              , Storable s
              , Space s
              )
           => FabricTagId
           -> FabricStorageMonad s m (WithParent (ForStorage s))
loadFabric fabricTagId =
  do tag <- fromPileS fabricTagPile (unFabricTagId fabricTagId)
     let buildFabric
             | fabTagIsTree      tag = do let treeId  = fabTagTreeId  tag
                                              childId = fabTagChildId tag
                                          return $ FLeaf $ FTree treeId childId
             | fabTagIsTransformAffine   tag = do let transformId = fabTagTransformId tag
                                                      childId = fabTagChildId tag
                                                  (AsBytes (forward, back)) <- fromPileS fabricHeapPile (Ref . unTransformId $ transformId)
                                                  return $ FTransform (FAffine forward back) childId
             | fabTagIsTransformFacet    tag = do let transformId = fabTagTransformId tag
                                                      childId = fabTagChildId tag
                                                  (AsBytes (facet :: Facet s)) <- fromPileS fabricHeapPile (Ref . unTransformId $ transformId)
                                                  return $ FTransform (FFacet facet) childId
             | fabTagIsTransformFilter   tag = do let transformId = fabTagTransformId tag
                                                      childId = fabTagChildId tag
                                                  (AsBytes (filt :: Int)) <- fromPileS fabricHeapPile (Ref . unTransformId $ transformId)
                                                  return $ FTransform (FFilter (toEnum filt)) childId
             | fabTagIsTransformConvolve tag = do let transformId = fabTagTransformId tag
                                                      childId = fabTagChildId tag
                                                  (AsBytes (scale:: s)) <- fromPileS fabricHeapPile (Ref . unTransformId $ transformId)
                                                  return $ FTransform (FConvolve scale) childId
             | fabTagIsSubstance         tag = do let substanceTag = fabTagSubstanceTag tag
                                                  fabSubstance <- overStateT fabricHeapPile $ loadSubstance substanceTag
                                                  return . FLeaf . FTreeSubstance $ fabSubstance
             | fabTagIsBinaryOp          tag = do let parentId = fabTagParentId tag
                                                      childId  = fabTagChildId  tag
                                                      op = fabTagOp tag
                                                  (shapeAbove, shapeBelow) <- fromLimits <$> fromPileS fabricTagPile (unFabricTagId fabricTagId + 1)
                                                  return $ FCombine (op, shapeAbove, shapeBelow) parentId childId
             | otherwise = error "unsupported fabricType"
     fabric <- buildFabric
     parent <- loadFabricParent fabricTagId
     return $ WithParent parent fabric
