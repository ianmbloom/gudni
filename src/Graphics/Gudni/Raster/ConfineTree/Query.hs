{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.ConfineTree.Query
  ( queryPointWithInfo
  , queryBox
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Decorate
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ConfineTree.ItemStack
import Graphics.Gudni.Raster.ConfineTree.Traverse
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
import Control.Monad.State

collectIfOverlapsBox :: Monad m => Box s -> TaggedBezier s -> StateT [TaggedBezier s] m ()
collectIfOverlapsBox box curve =
  modify (curve:)

collectIfCrossed :: (Space s, Monad m) => Point2 s -> Point2 s -> TaggedBezier s -> StateT [TaggedBezier s] m ()
collectIfCrossed start end curve =
  when (crosses start end (curve ^. tBez)) $ modify (curve:)

modifyItemStackIfCrossed :: (Space s, Monad m) => Point2 s -> Point2 s -> TaggedBezier s -> StateT ItemStack m ()
modifyItemStackIfCrossed start end curve =
  when (crosses start end (curve ^. tBez)) $ modify (toggleItem (curve ^. tBezItem))

buildStack :: (Monad m) => ItemStack -> StateT (Point2 s, ItemStack) m ()
buildStack branchItemStack = modify (over _2 (combineItemStacks branchItemStack))

holdAnchor :: (Monad m) => Point2 s -> StateT (Point2 s, ItemStack) m ()
holdAnchor anchor = modify (set _1 anchor)

getAnchorStack :: (Space s) => Point2 s -> DecorateTree s -> (Point2 s, ItemStack)
getAnchorStack point decoTree = execState (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, [])

secondLeg :: (Space s) => Point2 s -> Point2 s -> ConfineTree s -> ItemStack -> ItemStack
secondLeg anchor point confineTree anchorStack = execState (traverseCTBetweenPoints (modifyItemStackIfCrossed anchor point) anchor point confineTree) anchorStack

queryPoint :: forall s . (Space s) => ConfineTree s -> DecorateTree s -> Point2 s -> ItemStack
queryPoint confineTree decoTree point =
    let (anchor, anchorStack) = getAnchorStack point decoTree
        stack = secondLeg anchor point confineTree anchorStack
    in  stack

queryPointWithInfo :: forall s . (Space s) => ConfineTree s -> DecorateTree s -> Point2 s -> (Point2 s, ItemStack, ItemStack, [TaggedBezier s])
queryPointWithInfo confineTree decoTree point =
    let (anchor, anchorStack) = execState (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, [])
        consideredCurves = execState (traverseCTBetweenPoints (collectIfOverlapsBox (boxAroundPoints anchor point)) anchor point confineTree) []
        stack = secondLeg anchor point confineTree anchorStack
    in  (anchor, anchorStack, stack, consideredCurves)

queryBox :: forall s . (Space s) => ConfineTree s -> DecorateTree s -> Box s -> (ItemStack, [TaggedBezier s])
queryBox confineTree decoTree box =
  let point = box ^. minBox
      (anchor, anchorStack) = getAnchorStack point decoTree
      stack = secondLeg anchor point confineTree anchorStack
      curvesInBox = execState (traverseCTBox (collectIfOverlapsBox box) box confineTree) []
  in  undefined
