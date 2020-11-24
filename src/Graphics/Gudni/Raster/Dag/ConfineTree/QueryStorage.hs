{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.QueryStorage
  ( queryConfineTagPoint
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.ConfineTree.Decorate
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Cross
import Graphics.Gudni.Raster.Dag.Primitive.Stack
import Graphics.Gudni.Raster.Dag.Primitive.WithinBox

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.TraverseStorage
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Util.Debug

import Foreign.Storable
import Control.Lens
import Control.Monad
import Control.Monad.State

modifyItemStackIfCrossed :: (Space s, Storable s, MonadIO m) => Point2 s -> Point2 s -> PrimTagId -> StateT ShapeStack (DagMonad s m) ()
modifyItemStackIfCrossed start end primTagId =
  do prim <- lift $ loadPrimS primTagId
     when (crossesPrim start end prim) $ modify (toggleShapeActive (prim ^. primShapeId))

buildStack :: (MonadIO m) => Slice ShapeId -> StateT (Point2 s, ShapeStack) (DagMonad s m) ()
buildStack slice = do newStack <- mapSliceM (lift . fromPileS (dagTreeStorage . treeCrossingPile)) slice
                      modify (over _2 (combineShapeStacks newStack))

holdAnchor :: (Monad m) => Point2 s -> StateT (Point2 s, ShapeStack) m ()
holdAnchor anchor = modify (set _1 anchor)

getAnchorStack :: (Space s, Storable s, MonadIO m) => Point2 s -> DecoTagId s -> DagMonad s m (Point2 s, ShapeStack)
getAnchorStack point decoTree = execStateT (traverseDecorateTreeTag buildStack holdAnchor point decoTree) (zeroPoint, [])

secondLeg :: (Space s, Storable s, MonadIO m) => Point2 s -> Point2 s -> ConfineTagId s -> ShapeStack -> DagMonad s m ShapeStack
secondLeg anchor point confineTagId anchorStack = execStateT (traverseCTagBetweenPoints (modifyItemStackIfCrossed anchor point) anchor point confineTagId) anchorStack

queryConfineTagPoint :: forall s m . (Space s, Storable s, MonadIO m) => TreeRoot s -> Point2 s -> DagMonad s m ShapeStack
queryConfineTagPoint root point =
    do  let (confineTreeId, decoTreeId) = root
        (anchor, anchorStack) <- getAnchorStack point decoTreeId
        stack <- secondLeg anchor point confineTreeId anchorStack
        return stack
