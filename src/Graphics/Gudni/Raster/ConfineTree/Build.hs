{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}

module Graphics.Gudni.Raster.ConfineTree.Build
  ( addPileToConfineTree
  , buildConfineTree
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Add
import Graphics.Gudni.Raster.ConfineTree.Sweep
import Graphics.Gudni.Raster.ConfineTree.Storage

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Control.Loop

import Control.Monad.Random


addPileToConfineTree :: forall s m
                     .  ( TreeConstraints s m
                        )
                     => Bool
                     -> Slice PrimTagId
                     -> Pile PrimTagId
                     -> ConfineTagId s
                     -> TreeMonad s m (ConfineTagId s)
addPileToConfineTree enableShuffle slice pile treeId =
    flip evalRandT (mkStdGen 10000) $
        numLoopState 0 (size - 1) treeId go
        where
        start = sliceStart slice
        size = sliceLength slice
        store :: Reference PrimTagId -> PrimTagId -> RandT g (TreeMonad s m) ()
        store x a = liftIO $ toPile   pile (x + start) a
        load :: Reference PrimTagId -> RandT g (TreeMonad s m) PrimTagId
        load  x   = liftIO $ fromPile pile (x + start)
        go :: RandomGen g => ConfineTagId s -> Reference PrimTagId -> RandT g (TreeMonad s m) (ConfineTagId s)
        go treeId i =
            do  primTagId <- if enableShuffle
                             then do -- Swap the next tagId for a random on in the remaining slice.
                                     j <- Ref <$> getRandomR (unRef i, unRef size - 1)
                                     hold <- load i
                                     primTagId <- load j
                                     store j hold
                                     store i primTagId
                                     return primTagId
                             else load i
                -- Add the primitive to the tree.
                box <- lift $ boxOf <$> loadTreePrim primTagId
                lift $ addPrimToConfineTree box primTagId treeId

buildConfineTree :: forall s m
                 .  ( TreeConstraints s m
                    )
                 => s
                 -> Int
                 -> Slice PrimTagId
                 -> Pile PrimTagId
                 -> TreeMonad s m (DecoTagId s, ConfineTagId s)
buildConfineTree limit decorationLimit slice primTagIdPile =
  do confineTreeId <- addPileToConfineTree True slice primTagIdPile nullConfineTagId
     decoTreeId <- sweepConfineTree limit decorationLimit confineTreeId
     return (decoTreeId, confineTreeId)
