{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Query
  ( queryConfineTreePoint
  , queryConfineTreePointWithInfo
  , queryConfineTreeBox
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Decorate
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Cross
import Graphics.Gudni.Raster.Dag.Primitive.Stack
import Graphics.Gudni.Raster.Dag.Primitive.WithinBox

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Traverse
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
import Control.Monad.State

collectIfVisited :: Monad m => (PrimTagId -> m (Primitive s)) -> PrimTagId -> StateT [TPrim s] m ()
collectIfVisited getPrim primTagId =
  do prim <- lift $ getPrim primTagId
     modify (TPrim primTagId prim:)

collectIfCrossed :: (Space s, Monad m) => s -> Point2 s -> Point2 s -> TPrim s -> StateT [TPrim s] m ()
collectIfCrossed limit start end prim =
  when (crossesPrim False limit start end (prim ^. tPrim)) $ modify (prim:)

modifyItemStackIfCrossed :: (Space s, Monad m) => s -> (PrimTagId -> m (Primitive s)) -> Point2 s -> Point2 s -> PrimTagId -> StateT ShapeStack m ()
modifyItemStackIfCrossed limit getPrim start end primTagId =
  do prim <- lift $ getPrim primTagId
     when (crossesPrim False limit start end prim) $ modify (toggleShapeActive (prim ^. primShapeId))

buildStack :: (Monad m) => ShapeStack -> StateT (Point2 s, ShapeStack) m ()
buildStack branchShapeStack = modify (over _2 (combineShapeStacks branchShapeStack))

holdAnchor :: (Monad m) => Point2 s -> StateT (Point2 s, ShapeStack) m ()
holdAnchor anchor = modify (set _1 anchor)

getAnchorStack :: (Space s) => Point2 s -> DecorateTree s -> (Point2 s, ShapeStack)
getAnchorStack point decoTree = execState (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, [])

secondLeg :: (Space s, Monad m) => s -> (PrimTagId -> m (Primitive s)) -> Point2 s -> Point2 s -> ConfineTree s -> ShapeStack -> m ShapeStack
secondLeg limit getPrim anchor point confineTree anchorStack = execStateT (traverseCTBetweenPoints (modifyItemStackIfCrossed limit getPrim anchor point) anchor point confineTree) anchorStack

queryConfineTreePoint :: forall s m . (Space s, Monad m) => s -> (PrimTagId -> m (Primitive s)) -> ConfineTree s -> DecorateTree s -> Point2 s -> m ShapeStack
queryConfineTreePoint limit getPrim confineTree decoTree point =
    do  let (anchor, anchorStack) = getAnchorStack point decoTree
        stack <- secondLeg limit getPrim anchor point confineTree anchorStack
        return stack

queryConfineTreePointWithInfo :: forall s m . (Space s, Monad m) => s -> (PrimTagId -> m (Primitive s)) -> ConfineTree s -> DecorateTree s -> Point2 s -> m (Point2 s, ShapeStack, ShapeStack, [TPrim s])
queryConfineTreePointWithInfo limit getPrim confineTree decoTree point =
    do let (anchor, anchorStack) = execState (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, [])
       consideredPrims <- execStateT (traverseCTBetweenPoints (collectIfVisited getPrim) anchor point confineTree) []
       stack <- secondLeg limit getPrim anchor point confineTree anchorStack
       return (anchor, anchorStack, stack, consideredPrims)

collectIfPrimWithinBox :: (Space s, Monad m) => s -> (PrimTagId -> m (Primitive s)) -> Box s -> PrimTagId -> StateT [TPrim s] m ()
collectIfPrimWithinBox limit getPrim box primTagId =
    do prim <- lift $ getPrim primTagId
       modify (map (TPrim primTagId) (primsWithinBox limit box prim) ++)

queryConfineTreeBox :: forall s m . (Space s, Monad m) => s -> (PrimTagId -> m (Primitive s)) -> ConfineTree s -> DecorateTree s -> Box s -> m (ShapeStack, [TPrim s])
queryConfineTreeBox limit getPrim confineTree decoTree box =
  do  let point = box ^. minBox
          (anchor, anchorStack) = getAnchorStack point decoTree
      stack <- secondLeg limit getPrim anchor point confineTree anchorStack
      primsInBox <- execStateT (traverseCTBox (collectIfPrimWithinBox limit getPrim box) box confineTree) []
      return (stack, primsInBox)
