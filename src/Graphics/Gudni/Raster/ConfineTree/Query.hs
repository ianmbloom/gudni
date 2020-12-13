{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.ConfineTree.Query
  ( queryConfinePoint
  , getAnchorStack
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.ConfineTree.Decorate
import Graphics.Gudni.Raster.ConfineTree.Traverse

import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Cross
import Graphics.Gudni.Raster.ConfineTree.Primitive.Stack
import Graphics.Gudni.Raster.ConfineTree.Primitive.WithinBox

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Util.Debug

import Foreign.Storable
import Control.Lens
import Control.Monad
import Control.Monad.State

modifyItemStackIfCrossed :: (TreeConstraints s m) => s -> Point2 s -> Point2 s -> PrimTagId -> StateT ShapeStack (TreeMonad s m) ()
modifyItemStackIfCrossed limit start end primTagId =
  do prim <- lift $ loadTreePrim primTagId
     let crosses = crossesPrim limit start end prim
     when crosses $ modify (toggleShapeActive (prim ^. primFabricTagId))

buildStack :: (TreeConstraints s m) => Slice FabricTagId -> StateT (Point2 s, ShapeStack) (TreeMonad s m) ()
buildStack slice = do newStack <- mapSliceM (lift . fromPileS (treeCrossingPile)) slice
                      modify (over _2 (combineShapeStacks newStack))

holdAnchor :: (Monad m) => Point2 s -> StateT (Point2 s, ShapeStack) m ()
holdAnchor anchor = modify (set _1 anchor)

getAnchorStack :: (TreeConstraints s m) => Point2 s -> ShapeStack -> DecoTagId s -> TreeMonad s m (Point2 s, ShapeStack)
getAnchorStack point initStack decoTree = execStateT (traverseDecorateTree buildStack holdAnchor point decoTree) (zeroPoint, initStack)

secondLeg :: (TreeConstraints s m) => s -> Point2 s -> Point2 s -> ConfineTagId s -> ShapeStack -> TreeMonad s m ShapeStack
secondLeg limit anchor point confineTagId anchorStack = execStateT (traverseCTagBetweenPoints (modifyItemStackIfCrossed limit anchor point) anchor point confineTagId) anchorStack

queryConfinePoint :: forall s m . (TreeConstraints s m) => s -> DecoTagId s -> ConfineTagId s -> ShapeStack -> Point2 s -> TreeMonad s m ShapeStack
queryConfinePoint limit decoId confineId initStack point =
    do  (anchor, anchorStack) <- getAnchorStack point initStack decoId
        --when (point == Point2 2.22 0.44) $ liftIO $ putStrLn $ "afterDecorate point " ++ show point ++ " anchor " ++ show anchor ++ " \n" ++ show anchorStack
        stack <- secondLeg limit anchor point confineId anchorStack
        --when (point == Point2 2.22 0.44) $ liftIO $ putStrLn $ "after SecondLeg " ++ " \n" ++ show stack
        return stack
