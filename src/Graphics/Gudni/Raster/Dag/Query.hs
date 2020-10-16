{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}

module Graphics.Gudni.Raster.Dag.Query
  ( QueryValue(..)
  , Ray(..)
  , queryFabric
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Raster.Dag.Primitive
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.PrimTag
import Graphics.Gudni.Raster.Dag.PrimStorage
import Graphics.Gudni.Raster.Dag.SubstanceTag
import Graphics.Gudni.Raster.Dag.FabricTag
import Graphics.Gudni.Raster.Dag.FabricStorage
import Graphics.Gudni.Raster.Dag.TreeStorage
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.Fabric
import Graphics.Gudni.Raster.ConfineTree.Query

import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Dag.Serialize
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Util.Util

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens
import Foreign.Storable

import Linear.Vector
import Linear.Metric
import Data.Kind

getPixelS :: (FabricConstraints s m)
          => PictureMemoryReference
          -> Point2 PixelSpace
          -> FabricMonad s m Color
getPixelS memRef p = do pixelPile <- use dagPixelPile
                        getPixelColor pixelPile memRef p

class QueryValue q where
    emptyQuery    :: q
    queryStop     :: q -> Bool
    overlapQuery  :: q -> q -> q
    fromColor     :: Point2 (SpaceOf q) -> Color -> q
    fromTexture   :: MonadIO m => Point2 (SpaceOf q) -> PictureMemoryReference -> FabricMonad (SpaceOf q) m q
    fromLinear    :: Point2 (SpaceOf q) -> q
    fromQuadrance :: Point2 (SpaceOf q) -> q
    overFabOp     :: FCombineType -> q -> q -> q

instance QueryValue Color where
    emptyQuery = clearBlack
    queryStop color = isOpaque color
    overlapQuery = composite
    fromColor   ray color = color
    fromTexture ray memRef = let p = fmap floor $ rayToPoint ray
                             in  getPixelS memRef p
    fromLinear    p = Color $ pure (subSpaceToFloat $ fromAlong Vertical $ p ^. pY)     -- y < 0 = (1,1,1,1), y >= 1 = (0,0,0,0) interpolate between
    fromQuadrance p = Color $ pure (subSpaceToFloat $ quadrance p) -- origin = (1,1,1,1), distance to origin >= 1 = (0,0,0,0)
    overFabOp fabOp (Color a) (Color b) =
       case fabOp of
           FComposite -> composite (Color a) (Color b)
           FMask      -> Color $ a * b
           FAdd       -> Color $ a ^+^ b
           FFloatOr   -> Color $ a ^+^ b ^-^ (a * b)
           FFloatXor  -> Color $ a ^+^ b ^-^ (2 *^ (a * b))

class ( HasSpace r
      , Storable (SpaceOf r)
      , Storable (Bezier (SpaceOf r))
      , Storable (Facet (SpaceOf r))
      ) => Ray r where
    rayToPoint    :: r -> Point2 (SpaceOf r)
    overTree      :: MonadIO m => ConfineTree (SpaceOf r) -> DecorateTree (SpaceOf r) -> r -> FabricMonad (SpaceOf r) m PrimStack
    overTransform :: FTransformer (SpaceOf r) -> r -> r
    overFacet     :: Facet (SpaceOf r) -> r -> r

instance (Space s, Storable s) => Ray (Point2 s) where
    rayToPoint           ray = ray
    overTree cTree dTree ray = queryConfineTreePoint loadCurveS cTree dTree (rayToPoint ray)
    overTransform trans  ray = transformPoint trans ray
    overFacet     facet  ray = inverseFacet facet ray

randomVector = error "convolve not implemented"

transformPoint :: Space s => FTransformer s -> Point2 s -> Point2 s
transformPoint trans ray =
  case trans of
      FAffineRay affine -> applyAffine affine ray
      FFacet     facet  -> inverseFacet  facet  ray
      FFilter    filt   -> ray
      FConvolve  scale  -> ray ^+^ randomVector scale

queryFabric :: forall m ray q
            .  ( MonadIO m
               , Ray ray
               , QueryValue q
               , SpaceOf q ~ SpaceOf ray
               )
            => ray
            -> FabricTagId
            -> FabricMonad (SpaceOf ray) m q
queryFabric ray fabricTagId =
  do (parent, fabric) <- loadFabricS fabricTagId
     case fabric of
       FCombine op parentId childId ->
          do parentQ <- queryFabric ray parentId
             childQ  <- queryFabric ray childId
             return $ overFabOp op parentQ childQ
       FTransform trans childId ->
           let ray' = overTransform trans ray
           in  queryFabric ray' childId
       FLeaf substance -> querySubstance ray substance

querySubstance :: ( MonadIO m
                  , Ray ray
                  , QueryValue q
                  , SpaceOf q ~ SpaceOf ray
                  )
               => ray
               -> FSubstance (ForStorage (SpaceOf ray))
               -> FabricMonad (SpaceOf ray) m q
querySubstance ray substance =
  case substance of
    FGeometry  (FTree tree child) -> do primStack <- queryTree ray tree
                                        queryPrimStack ray primStack
    FConst     q    -> return $ fromColor (rayToPoint ray) q
    FTexture   tex  -> fromTexture (rayToPoint ray) tex
    FLinear         -> return $ fromLinear    (rayToPoint ray)
    FQuadrance      -> return $ fromQuadrance (rayToPoint ray)

queryTree :: ( MonadIO m
             , Ray ray
             )
          => ray
          -> ConfineTreeId
          -> FabricMonad (SpaceOf ray) m PrimStack
queryTree ray confineTreeId =
    do  (confineTree, decoTree) <- loadTreeS confineTreeId
        overTree confineTree decoTree ray

queryPrimStack :: ( MonadIO m
                  , Ray ray
                  , QueryValue q
                  , SpaceOf q ~ SpaceOf ray
                  )
               => ray
               -> [PrimTagId]
               -> FabricMonad (SpaceOf ray) m q
queryPrimStack ray primStack =
      case primStack of
          prim:rest -> do value <- queryPrim ray prim
                          if queryStop value
                          then return value
                          else do nextValue <- queryPrimStack ray primStack
                                  return $ overlapQuery value nextValue
          [] -> return emptyQuery

queryPrim :: ( MonadIO m
             , Ray ray
             , QueryValue q
             , SpaceOf q ~ SpaceOf ray
             )
          => ray
          -> PrimTagId
          -> FabricMonad (SpaceOf ray) m q
queryPrim ray primTagId =
  do prim <- loadPrimS primTagId
     case prim of
       PrimBezier fabricTagId _ ->
           queryFabric ray fabricTagId
       PrimFacet fabricTagId facet ->
           let ray' = overFacet facet ray
           in  queryFabric ray' fabricTagId
