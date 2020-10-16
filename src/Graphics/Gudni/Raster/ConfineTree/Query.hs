{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.ConfineTree.Query
  ( queryConfineTreePoint
  , queryConfineTreePointWithInfo
  , queryConfineTreeBox
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Decorate
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.Dag.PrimStack
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Traverse
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
import Control.Monad.State

collectIfVisited :: Monad m => (PrimTagId -> m (Bezier s)) -> PrimTagId -> StateT [TBezier s] m ()
collectIfVisited getCurve primTagId =
  do curve <- lift $ getCurve primTagId
     modify (TBezier primTagId curve:)

collectIfCrossed :: (Space s, Monad m) => Point2 s -> Point2 s -> TBezier s -> StateT [TBezier s] m ()
collectIfCrossed start end curve =
  when (crosses start end (curve ^. tBez)) $ modify (curve:)

modifyItemStackIfCrossed :: (Space s, Monad m) => (PrimTagId -> m (Bezier s)) -> Point2 s -> Point2 s -> PrimTagId -> StateT PrimStack m ()
modifyItemStackIfCrossed getCurve start end primTagId =
  do bez <- lift $ getCurve primTagId
     when (crosses start end bez) $ modify (toggleItem primTagId)

buildStack :: (Monad m) => PrimStack -> StateT (Point2 s, PrimStack) m ()
buildStack branchItemStack = modify (over _2 (combineItemStacks branchItemStack))

holdAnchor :: (Monad m) => Point2 s -> StateT (Point2 s, PrimStack) m ()
holdAnchor anchor = modify (set _1 anchor)

getAnchorStack :: (Space s) => Point2 s -> DecorateTree s -> (Point2 s, PrimStack)
getAnchorStack point decoTree = execState (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, [])

secondLeg :: (Space s, Monad m) => (PrimTagId -> m (Bezier s)) -> Point2 s -> Point2 s -> ConfineTree s -> PrimStack -> m PrimStack
secondLeg getCurve anchor point confineTree anchorStack = execStateT (traverseCTBetweenPoints (modifyItemStackIfCrossed getCurve anchor point) anchor point confineTree) anchorStack

queryConfineTreePoint :: forall s m . (Space s, Monad m) => (PrimTagId -> m (Bezier s)) -> ConfineTree s -> DecorateTree s -> Point2 s -> m PrimStack
queryConfineTreePoint getCurve confineTree decoTree point =
    do  let (anchor, anchorStack) = getAnchorStack point decoTree
        stack <- secondLeg getCurve anchor point confineTree anchorStack
        return stack

queryConfineTreePointWithInfo :: forall s m . (Space s, Monad m) => (PrimTagId -> m (Bezier s)) -> ConfineTree s -> DecorateTree s -> Point2 s -> m (Point2 s, PrimStack, PrimStack, [TBezier s])
queryConfineTreePointWithInfo getCurve confineTree decoTree point =
    do let (anchor, anchorStack) = execState (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, [])
       consideredCurves <- execStateT (traverseCTBetweenPoints (collectIfVisited getCurve) anchor point confineTree) []
       stack <- secondLeg getCurve anchor point confineTree anchorStack
       return (anchor, anchorStack, stack, consideredCurves)

collectIfCurveWithinBox :: (Space s, Monad m) => (PrimTagId -> m (Bezier s)) -> Box s -> PrimTagId -> StateT [TBezier s] m ()
collectIfCurveWithinBox getCurve box primTagId =
    do curve <- lift $ getCurve primTagId
       modify (map (TBezier primTagId) (curveWithinBox box curve) ++)

queryConfineTreeBox :: forall s m . (Space s, Monad m) => (PrimTagId -> m (Bezier s)) -> ConfineTree s -> DecorateTree s -> Box s -> m (PrimStack, [TBezier s])
queryConfineTreeBox getCurve confineTree decoTree box =
  do  let point = box ^. minBox
          (anchor, anchorStack) = getAnchorStack point decoTree
      stack <- secondLeg getCurve anchor point confineTree anchorStack
      curvesInBox <- execStateT (traverseCTBox (collectIfCurveWithinBox getCurve box) box confineTree) []
      return (stack, curvesInBox)
