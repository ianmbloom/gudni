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

collectIfCrossed :: (Space s, Monad m) => Point2 s -> Point2 s -> TPrim s -> StateT [TPrim s] m ()
collectIfCrossed start end prim =
  when (crossesPrim start end (prim ^. tPrim)) $ modify (prim:)

modifyItemStackIfCrossed :: (Space s, Monad m) => (PrimTagId -> m (Primitive s)) -> Point2 s -> Point2 s -> PrimTagId -> StateT PrimStack m ()
modifyItemStackIfCrossed getPrim start end primTagId =
  do prim <- lift $ getPrim primTagId
     when (crossesPrim start end prim) $ modify (toggleItem primTagId)

buildStack :: (Monad m) => PrimStack -> StateT (Point2 s, PrimStack) m ()
buildStack branchItemStack = modify (over _2 (combineItemStacks branchItemStack))

holdAnchor :: (Monad m) => Point2 s -> StateT (Point2 s, PrimStack) m ()
holdAnchor anchor = modify (set _1 anchor)

getAnchorStack :: (Space s) => Point2 s -> DecorateTree s -> (Point2 s, PrimStack)
getAnchorStack point decoTree = execState (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, [])

secondLeg :: (Space s, Monad m) => (PrimTagId -> m (Primitive s)) -> Point2 s -> Point2 s -> ConfineTree s -> PrimStack -> m PrimStack
secondLeg getPrim anchor point confineTree anchorStack = execStateT (traverseCTBetweenPoints (modifyItemStackIfCrossed getPrim anchor point) anchor point confineTree) anchorStack

queryConfineTreePoint :: forall s m . (Space s, Monad m) => (PrimTagId -> m (Primitive s)) -> ConfineTree s -> DecorateTree s -> Point2 s -> m PrimStack
queryConfineTreePoint getPrim confineTree decoTree point =
    do  let (anchor, anchorStack) = getAnchorStack point decoTree
        stack <- secondLeg getPrim anchor point confineTree anchorStack
        return stack

queryConfineTreePointWithInfo :: forall s m . (Space s, Monad m) => (PrimTagId -> m (Primitive s)) -> ConfineTree s -> DecorateTree s -> Point2 s -> m (Point2 s, PrimStack, PrimStack, [TPrim s])
queryConfineTreePointWithInfo getPrim confineTree decoTree point =
    do let (anchor, anchorStack) = execState (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, [])
       consideredPrims <- execStateT (traverseCTBetweenPoints (collectIfVisited getPrim) anchor point confineTree) []
       stack <- secondLeg getPrim anchor point confineTree anchorStack
       return (anchor, anchorStack, stack, consideredPrims)

collectIfPrimWithinBox :: (Space s, Monad m) => (PrimTagId -> m (Primitive s)) -> Box s -> PrimTagId -> StateT [TPrim s] m ()
collectIfPrimWithinBox getPrim box primTagId =
    do prim <- lift $ getPrim primTagId
       modify (map (TPrim primTagId) (primsWithinBox box prim) ++)

queryConfineTreeBox :: forall s m . (Space s, Monad m) => (PrimTagId -> m (Primitive s)) -> ConfineTree s -> DecorateTree s -> Box s -> m (PrimStack, [TPrim s])
queryConfineTreeBox getPrim confineTree decoTree box =
  do  let point = box ^. minBox
          (anchor, anchorStack) = getAnchorStack point decoTree
      stack <- secondLeg getPrim anchor point confineTree anchorStack
      primsInBox <- execStateT (traverseCTBox (collectIfPrimWithinBox getPrim box) box confineTree) []
      return (stack, primsInBox)
