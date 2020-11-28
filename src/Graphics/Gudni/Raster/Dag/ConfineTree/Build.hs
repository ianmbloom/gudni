{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Build
  ( buildConfineTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.TagTypes

import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.ConfineTree.Add
import Graphics.Gudni.Raster.Dag.ConfineTree.Decorate
import Graphics.Gudni.Raster.Dag.ConfineTree.Sweep
import Graphics.Gudni.Raster.Dag.ConfineTree.SweepTrace
import Graphics.Gudni.Raster.Dag.ConfineTree.Depth

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Shuffle

import GHC.Exts
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Data.List.Lens
import Data.Kind

import Control.Loop
import Foreign.Storable

import qualified Data.Vector as V
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

import Control.Monad.ST
import Control.Monad.Random
import System.Random

addPileToConfineTree :: forall s m
                     .  ( Space s
                        , MonadIO m
                        )
                     => (PrimTagId -> m (Box s))
                     -> Bool
                     -> Slice PrimTagId
                     -> Pile PrimTagId
                     -> ConfineTree s
                     -> m (ConfineTree s)
addPileToConfineTree getBox enableShuffle slice pile mTree =
    flip evalRandT (mkStdGen 10000) $
        numLoopState 0 (size - 1) mTree go
        where
        size = sliceLength slice
        go :: RandomGen g => ConfineTree s -> Reference PrimTagId -> RandT g m (ConfineTree s)
        go mTree i =
            do  primTagId <- if enableShuffle
                             then do -- Swap the next tagId for a random on in the remaining slice.
                                     j <- Ref <$> getRandomR (unRef i, unRef size - 1)
                                     liftIO $ do (hold :: PrimTagId) <- fromPile pile i
                                                 primTagId <- fromPile pile j
                                                 toPile pile j hold
                                                 toPile pile i primTagId
                                                 return primTagId
                             else    liftIO $ fromPile pile i
                -- Add the primitive to the tree.
                box <- lift $ getBox primTagId
                return $ addBezierToConfineTree box primTagId mTree

buildConfineTree :: forall s m
                 .  ( Space s
                    , MonadIO m
                    )
                 => s
                 -> (PrimTagId -> m (Box s))
                 -> (PrimTagId -> m (Primitive s))
                 -> Bool
                 -> Int
                 -> Int
                 -> Slice PrimTagId
                 -> Pile PrimTagId
                 -> m (ConfineTree s, DecorateTree s, SweepTrace s)
buildConfineTree limit getBox getPrim decorateType traceLimit decorationLimit slice primTagIdPile =
  do   treeBare <- -- tcP "bareTree" .
                   -- trWith (show . confineTreeCountOverlaps) "confineTreeCountOverlaps" .
                   -- trWith (show . confineTreeDepth) "confineTreeDepth" .
                   -- trWith (show . logBase 2 . (fromIntegral :: Int -> Float) . confineTreeSize) "confineTreeSizeLog" .
                   trWith (show . confineTreeSize) "confineTreeSize" <$>
                   addPileToConfineTree getBox True slice primTagIdPile Nothing
       let numItems = confineTreeSize treeBare
       --liftIO $ putStrLn $ "decorateType " ++ show decorateType ++ " limit " ++ show decorationLimit
       (treeDecorated2, trace) <- runStateT (sweepConfineTree limit
                                                              (lift . getBox  )
                                                              (lift . getPrim )
                                                              crossStep
                                                              branchStep
                                                              (overhangOp   traceLimit)
                                                              (nothingOp    traceLimit)
                                                              (pushBypassOp traceLimit)
                                                              (popBypassOp  traceLimit)
                                                              (pushPath     traceLimit)
                                                              (popPath      traceLimit)
                                                              decorationLimit
                                                              treeBare
                                                            ) initialTrace

       -- liftIO $ putStrLn $ "sweepCrossSteps: " ++ show (trace ^. sweepCrossSteps)
       -- liftIO $ putStrLn $ "averageConsidered " ++ showFl' 4 (fromIntegral (trace ^. sweepCrossSteps) / fromIntegral numItems :: Double)
       -- liftIO $ putStrLn ""

       (treeDecorated, decorationCount) <- runStateT (buildDecorateTree limit (lift . getPrim) (modify (+1)) decorationLimit treeBare) 0
       --conConfineTree .= treeDecorated
       let total = decorationCount
       -- liftIO $ putStrLn $ "steps per node    " ++ showFl' 4 (fromIntegral decorationCount / fromIntegral numItems :: Double)
       -- liftIO $ putStrLn ""
       let treesEqual = treeDecorated == treeDecorated2
       --liftIO $ putStrLn $ "trees equal " ++ show treesEqual
       --if treesEqual then return () else error "trees not equal."
       let dTree = {-trP "treeDecorated\n" $ -}
                   if decorateType
                   then treeDecorated
                   else treeDecorated2
       return (treeBare, dTree, trace)
