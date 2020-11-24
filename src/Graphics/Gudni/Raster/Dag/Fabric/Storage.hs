{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Raster.Dag.Fabric.Storage
  ( FabricStorage(..)
  , fabricTagPile
  , fabricHeapPile
  , initFabricStorage
  , freeFabricStorage
  , allocateFabricTag
  , allocateFabricCombineTag
  , addFabric
  , combineTypeToFabTag
  , fabTagCombineType
  , storeFabric
  , loadFabricTag
  , loadFabricShapeCutPoint
  , loadTransform
  , loadFilter
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
      fabricHeapPile   <- newPile
      return $ FabricStorage
               { _fabricTagPile    = fabricTagPile
               , _fabricHeapPile   = fabricHeapPile
               }

freeFabricStorage :: MonadIO m => FabricStorage s -> m ()
freeFabricStorage storage =
  do freePile $ storage ^. fabricTagPile
     freePile $ storage ^. fabricHeapPile

addFabric :: ( MonadIO m
             , Storable s
             , Space s
             )
          => Fabric (ForStorage s)
          -> FabricStorageMonad s m FabricTagId
addFabric fabric =
   case fabric of
       FCombine {} -> error "shouldn't be adding combine use allocateFabricCombineTag"
       _ -> do tagId <- allocateFabricTag
               storeFabric tagId fabric
               return tagId

storeCutPoint :: MonadIO m => FabricTagId -> ShapeId -> FabricStorageMonad s m ()
storeCutPoint fabricTagId belowShape = toPileS fabricTagPile (unFabricTagId fabricTagId + 1) (makeCutPoint belowShape)

storeFabric :: ( MonadIO m
               , Storable s
               , Space s
               )
            => FabricTagId
            -> Fabric (ForStorage s)
            -> FabricStorageMonad s m ()
storeFabric fabricTagId fabric =
  do tag <- case fabric of
                FTransform trans child ->
                    storeTransform trans child
                FLeaf leaf ->
                    case leaf of
                      FTree treeId childId ->
                          return $ makeFabTagTree treeId childId
                      FTreeSubstance substance ->
                          overStateT fabricHeapPile $ storeSubstance substance
                FCombine (op, belowShape) aboveId belowId ->
                     do storeCutPoint fabricTagId belowShape
                        return $ combineTypeToFabTag op aboveId belowId
     toPileS fabricTagPile (unFabricTagId fabricTagId) tag

allocateFabricCombineTag :: MonadIO m => FabricStorageMonad s m FabricTagId
allocateFabricCombineTag = do fabricTagId <- FabricTagId <$> allocatePileS fabricTagPile
                              allocatePileS fabricTagPile
                              return fabricTagId

allocateFabricTag :: MonadIO m => FabricStorageMonad s m FabricTagId
allocateFabricTag = FabricTagId <$> allocatePileS fabricTagPile

combineTypeToFabTag :: FCombineType -> FabricTagId -> FabricTagId -> FabricTag
combineTypeToFabTag op parentId childId =
    case op of
        FComposite   -> makeFabTagComposite  parentId childId
        FMask        -> makeFabTagMult       parentId childId
        FAdd         -> makeFabTagAdd        parentId childId
        FFloatOr     -> makeFabTagFloatOr    parentId childId
        FFloatXor    -> makeFabTagFloatXor   parentId childId
        FMin         -> makeFabTagMin        parentId childId
        FMax         -> makeFabTagMax        parentId childId
        FHsvAdjust   -> makeFabTagHsvAdjust  parentId childId
        FTransparent -> makeFabTagTranparent parentId childId

fabTagCombineType :: FabricTag -> FCombineType
fabTagCombineType tag
    | fabTagIsComposite  tag = FComposite
    | fabTagIsMult       tag = FMask
    | fabTagIsAdd        tag = FAdd
    | fabTagIsFloatOr    tag = FFloatOr
    | fabTagIsFloatXor   tag = FFloatXor
    | fabTagIsMin        tag = FMin
    | fabTagIsMax        tag = FMax
    | fabTagIsHsvAdjust  tag = FHsvAdjust
    | fabTagIsTranparent tag = FTransparent

loadFabricTag :: MonadIO m => FabricTagId -> FabricStorageMonad s m FabricTag
loadFabricTag fabricTagId = fromPileS fabricTagPile (unFabricTagId fabricTagId)

loadFabricShapeCutPoint :: MonadIO m => FabricTagId -> FabricStorageMonad s m ShapeId
loadFabricShapeCutPoint fabricTagId = fromCutPoint <$> fromPileS fabricTagPile (unFabricTagId fabricTagId + 1)

loadFabric :: forall s m
           .  ( MonadIO m
              , Storable s
              , Space s
              )
           => FabricTagId
           -> FabricStorageMonad s m (Fabric (ForStorage s))
loadFabric fabricTagId =
  do tag <- loadFabricTag fabricTagId
     let buildFabric
             | fabTagIsLeaf      tag = do fabSubstance <- overStateT fabricHeapPile $ loadSubstance tag
                                          return . FLeaf . FTreeSubstance $ fabSubstance
             | fabTagIsUnaryPre  tag = loadUnaryPre  tag
             | fabTagIsUnaryPost tag = loadUnaryPost tag
             | fabTagIsBinaryOp  tag = do let parentId = fabTagAboveId tag
                                              childId  = fabTagChildId  tag
                                              op = fabTagCombineType tag
                                          shapeCutPoint <- loadFabricShapeCutPoint fabricTagId
                                          return $ FCombine (op, shapeCutPoint) parentId childId
             | otherwise = error "unsupported fabricType"
     fabric <- buildFabric
     return fabric

loadUnaryPre :: ( MonadIO m
                , Storable s
                , Space s
                )
             => FabricTag
             -> FabricStorageMonad s m (Fabric (ForStorage s))
loadUnaryPre tag
    | fabTagIsTree tag = do let treeId  = fabTagTreeId  tag
                                childId = fabTagChildId tag
                            return $ FLeaf $ FTree treeId childId
    | otherwise        = do transform <- loadTransform tag
                            let childId = fabTagChildId tag
                            return $ FTransform transform childId

loadUnaryPost :: MonadIO m => FabricTag -> FabricStorageMonad s m (Fabric (ForStorage s))
loadUnaryPost tag = return $ FTransform (FFilter . loadFilter $ tag) (fabTagChildId tag)

loadFilter :: FabricTag -> FFilter
loadFilter tag
  | fabTagIsSqrt   tag = FSqrt
  | fabTagIsInvert tag = FInvert
  | fabTagIsCos    tag = FCos
  | fabTagIsSin    tag = FSin
  | fabTagIsClamp  tag = FClamp

storeUnaryPost :: Fabric (ForStorage s) -> FabricTag
storeUnaryPost fabric =
    case fabric of
        FTransform (FFilter filt) childId -> storeFilter filt childId
        _ -> error "not a unaryPost"

storeFilter :: FFilter -> FabricTagId -> FabricTag
storeFilter filt =
  case filt of
      FSqrt   -> makeFabTagSqrt
      FInvert -> makeFabTagInvert
      FCos    -> makeFabTagCos
      FSin    -> makeFabTagSin
      FClamp  -> makeFabTagClamp

storeTransform :: ( MonadIO m
                  , Storable s
                  , Space s
                  )
               => FTransformer s
               -> FabricTagId
               -> FabricStorageMonad s m (FabricTag)
storeTransform trans child =
    case trans of
        FAffine forward back ->
             do  affineRef <- addToPileS fabricHeapPile (AsBytes (forward, back))
                 return $ makeFabTagTransformAffine (TransformId . unRef $ affineRef) child
        FFacet facet ->
             do  facetRef <- addToPileS fabricHeapPile (AsBytes facet)
                 return $ makeFabTagTransformFacet (TransformId . unRef $ facetRef) child
        FFilter filt ->
             do  return $ storeFilter filt child
        FConvolve scale ->
             do  scaleRef <- addToPileS fabricHeapPile (AsBytes scale)
                 return $ makeFabTagTransformConvolve (TransformId . unRef $ scaleRef) child

loadTransform :: forall s m
              .  ( MonadIO m
                 , Storable s
                 , Space s
                 )
              => FabricTag
              -> FabricStorageMonad s m (FTransformer s)
loadTransform tag
     | fabTagIsTransformAffine   tag = do (AsBytes (forward, back)) <- fromPileS fabricHeapPile (Ref . unTransformId $ transformId)
                                          return $ FAffine forward back
     | fabTagIsTransformFacet    tag = do (AsBytes (facet :: Facet s)) <- fromPileS fabricHeapPile (Ref . unTransformId $ transformId)
                                          return $ FFacet facet
     | fabTagIsTransformConvolve tag = do (AsBytes (scale:: s)) <- fromPileS fabricHeapPile (Ref . unTransformId $ transformId)
                                          return $ FConvolve scale
     where transformId = fabTagTransformId tag
