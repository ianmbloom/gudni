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

modifyItemStackIfCrossed :: Ord item => (TreeConstraints s m) => s -> (FabricTagId -> item) -> Point2 s -> Point2 s -> PrimTagId -> StateT [item] (TreeMonad s m) ()
modifyItemStackIfCrossed limit makeItem start end primTagId =
  do prim <- lift $ loadTreePrim primTagId
     let crosses = crossesPrim limit start end prim
     when crosses $ modify (toggleItemActive (makeItem $ prim ^. primFabricTagId))

buildStack :: (TreeConstraints s m, Ord item) => (FabricTagId -> item) -> Slice FabricTagId -> StateT (Point2 s, [item]) (TreeMonad s m) ()
buildStack makeItem slice = do newStack <- map makeItem <$> mapSliceM (lift . fromPileS (treeCrossingPile)) slice
                               modify (over _2 (combineItemStacks newStack))

holdAnchor :: (Monad m) => Point2 s -> StateT (Point2 s, [item]) m ()
holdAnchor anchor = modify (set _1 anchor)

getAnchorStack :: ( TreeConstraints s m
                  , Ord item
                  )
               => (FabricTagId -> item)
               -> Point2 s
               -> [item]
               -> DecoTagId s
               -> TreeMonad s m (Point2 s, [item])
getAnchorStack makeItem point initStack decoTree = execStateT (traverseDecorateTree (buildStack makeItem) holdAnchor point decoTree) (zeroPoint, initStack)

secondLeg :: ( TreeConstraints s m
             , Ord item
             )
          => s
          -> (FabricTagId -> item)
          -> Point2 s
          -> Point2 s
          -> ConfineTagId s
          -> [item]
          -> TreeMonad s m [item]
secondLeg limit makeItem anchor point confineTagId anchorStack = execStateT (traverseCTagBetweenPoints (modifyItemStackIfCrossed limit makeItem anchor point) anchor point confineTagId) anchorStack

queryConfinePoint :: forall s m item
                  . ( TreeConstraints s m
                    , Ord item
                    )
                  => s
                  -> (FabricTagId -> item)
                  -> DecoTagId s
                  -> ConfineTagId s
                  -> [item]
                  -> Point2 s
                  -> TreeMonad s m [item]
queryConfinePoint limit makeItem decoId confineId initStack point =
    do  (anchor, anchorStack) <- getAnchorStack makeItem point initStack decoId
        --when (point == Point2 2.22 0.44) $ liftIO $ putStrLn $ "afterDecorate point " ++ show point ++ " anchor " ++ show anchor ++ " \n" ++ show anchorStack
        stack <- secondLeg limit makeItem anchor point confineId anchorStack
        --when (point == Point2 2.22 0.44) $ liftIO $ putStrLn $ "after SecondLeg " ++ " \n" ++ show stack
        return stack
