{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Raster.Dag.FabricStorage
  ( FabricStorage(..)
  , fabricTagPile
  , fabricParentPile
  , fabricLimitPile
  , fabricHeapPile
  , initFabricStorage
  , freeFabricStorage
  , allocateFabricParent
  , allocateFabricTag
  , storeFabricParent
  , storeFabric
  , loadFabric
  , storeLimit
  , loadLimit
  , loadFabricParent
  , FTree(..)
  , ForStorage(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree
import Graphics.Gudni.Raster.Dag.Fabric

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile
import Graphics.Gudni.Raster.Dag.Primitive
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.PrimTag
import Graphics.Gudni.Raster.Dag.SubstanceTag
import Graphics.Gudni.Raster.Dag.FabricTag
import Graphics.Gudni.Raster.TextureReference

import Foreign.Storable
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

type FabricStorageMonad s m q = StateT (FabricStorage s) m q

data FabricStorage s
    = FabricStorage
      -- | A list of every tag defining the fabric (or structure of the scene DAG)
    { _fabricTagPile :: Pile FabricTag
      -- | A list of every parent of every fabric tag.
    , _fabricParentPile :: Pile FabricTagId
      -- | And extension to the fabric tag that stores the limit above which a tag must be active for this tag to be active.
    , _fabricLimitPile :: Pile FabricTagId
      -- | A heap of all extra data used by the dag including transformations and boundaries
    , _fabricHeapPile :: BytePile
    }
makeLenses ''FabricStorage

initFabricStorage :: MonadIO m => m (FabricStorage s)
initFabricStorage =
   do fabricTagPile    <- newPile
      fabricParentPile <- newPile
      fabricLimitPile  <- newPile
      fabricHeapPile   <- newPile
      return $ FabricStorage
               { _fabricTagPile    = fabricTagPile
               , _fabricParentPile = fabricParentPile
               , _fabricLimitPile  = fabricLimitPile
               , _fabricHeapPile   = fabricHeapPile
               }

freeFabricStorage :: MonadIO m => FabricStorage s -> m ()
freeFabricStorage storage =
  do freePile $ storage ^. fabricTagPile
     freePile $ storage ^. fabricParentPile
     freePile $ storage ^. fabricLimitPile
     freePile $ storage ^. fabricHeapPile

data FTree = FTree ConfineTreeId FabricTagId

data ForStorage s

instance Space s => HasSpace (ForStorage s) where
    type SpaceOf   (ForStorage s) = s

instance Space s => FabricType (ForStorage s) where
    type FChild    (ForStorage s) = FabricTagId
    type FTex      (ForStorage s) = PictureMemoryReference
    type FGeometry (ForStorage s) = FTree
    type FQuery    (ForStorage s) = Color
    type FCombiner (ForStorage s) = FCombineType

storeFabricParent :: MonadIO m => FabricTagId -> FabricTagId -> FabricStorageMonad s m ()
storeFabricParent fabricTagId pFabricTagId = toPileS fabricParentPile (Ref . unRef . unFabricTagId $ fabricTagId) pFabricTagId

storeFabric :: ( MonadIO m
               , Storable s
               )
            => FabricTagId
            -> Fabric (ForStorage s)
            -> FabricStorageMonad s m ()
storeFabric fabricTagId fabric =
  do tag <- case fabric of
                FTransform trans child ->
                    case trans of
                        FAffineRay affine ->
                             do  affineRef <- addToPileS fabricHeapPile (AsBytes affine)
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
                FLeaf substance ->
                    case substance of
                      FGeometry (FTree treeId childId) -> return $ makeFabTagTree treeId childId
                      _ -> do  substanceTag <- storeSubstance substance
                               return $ makeFabTagSubstance substanceTag
                FCombine op parentId childId ->
                        return $ opToFabTag op parentId childId
     toPileS fabricTagPile (unFabricTagId fabricTagId) tag

allocateFabricParent :: MonadIO m => FabricStorageMonad s m FabricTagId
allocateFabricParent = FabricTagId . Ref . unRef <$> allocatePileS fabricParentPile

allocateFabricTag :: MonadIO m => FabricStorageMonad s m FabricTagId
allocateFabricTag = FabricTagId <$> allocatePileS fabricTagPile

loadFabricParent :: MonadIO m => FabricTagId -> FabricStorageMonad s m FabricTagId
loadFabricParent  fabricTagId = fromPileS fabricParentPile (Ref . unRef . unFabricTagId $ fabricTagId)

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
              )
           => FabricTagId
           -> StateT (FabricStorage s) m (Fabric (ForStorage s))
loadFabric fabricTagId =
  do tag <- fromPileS fabricTagPile (unFabricTagId fabricTagId)
     let buildFabric
             | fabTagIsTree      tag = do let treeId  = fabTagTreeId  tag
                                              childId = fabTagChildId tag
                                          return $ FLeaf $ FGeometry $ FTree treeId childId
             | fabTagIsTransformAffine   tag = do let transformId = fabTagTransformId tag
                                                      childId = fabTagChildId tag
                                                  (AsBytes affine) <- fromPileS fabricHeapPile (Ref . unTransformId $ transformId)
                                                  return $ FTransform (FAffineRay affine) childId
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
                                                  fabSubstance <- loadSubstance substanceTag
                                                  return $ FLeaf fabSubstance
             | fabTagIsBinaryOp          tag = do let parentId = fabTagParentId tag
                                                      childId  = fabTagChildId  tag
                                                      op = fabTagOp tag
                                                  return $ FCombine op parentId childId
             | otherwise = error "unsupported fabricType"
     buildFabric

storeLimit :: MonadIO m => FabricTagId -> StateT (FabricStorage s) m (Reference FabricTagId)
storeLimit limit = addToPileS fabricLimitPile limit

loadLimit :: MonadIO m => FabricTagId -> StateT (FabricStorage s) m FabricTagId
loadLimit  fabricTagId = fromPileS fabricLimitPile $ Ref . unRef . unFabricTagId $ fabricTagId

storeSubstance :: (MonadIO m, Storable s, Storable (FTex i), Storable (FQuery i))
               => FSubstance i
               -> StateT (FabricStorage s) m SubstanceTag
storeSubstance substance =
  case substance of
    FConst query -> do queryId <- addToPileS fabricHeapPile (AsBytes query)
                       return $ makeSubstanceTagConstant queryId
    FTexture tex -> do pictureRef <- addToPileS fabricHeapPile (AsBytes tex)
                       return $ makeSubstanceTagTexture pictureRef
    FLinear      -> return $ makeSubstanceTagLinear
    FQuadrance   -> return $ makeSubstanceTagQuadrance

loadSubstance :: ( MonadIO m
                 , Storable (FTex i)
                 , Storable (FQuery i)
                 )
              => SubstanceTag
              -> StateT (FabricStorage s) m (FSubstance i)
loadSubstance tag
   | substanceTagIsConstant  tag = do (AsBytes query) <- fromPileS fabricHeapPile (Ref . fromIntegral . substanceTagDescription $ tag)
                                      return $ FConst query
   | substanceTagIsTexture   tag = do (AsBytes tex) <- fromPileS fabricHeapPile (Ref . fromIntegral . substanceTagDescription $ tag)
                                      return $ FTexture tex
   | substanceTagIsLinear    tag = return FLinear
   | substanceTagIsQuadrance tag = return FQuadrance
