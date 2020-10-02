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

module Graphics.Gudni.Raster.ConfineTree.Build
  ( buildConfineTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Raster.ItemInfo

import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ConfineTree.Add
import Graphics.Gudni.Raster.ConfineTree.Decorate
import Graphics.Gudni.Raster.ConfineTree.Sweep
import Graphics.Gudni.Raster.ConfineTree.SweepTrace
import Graphics.Gudni.Raster.ConfineTree.Depth

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
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

indexVector :: Bool -> Int -> IO (V.Vector Int)
indexVector doShuffle size =
  do let indices = V.generate size id
     if doShuffle
     then return $ evalRand (shuffleImmutable indices) (mkStdGen 10000)
     else return indices

addPileToConfineTree :: ( Space s
                        , Storable (ItemBezier s)
                        )
                     => Bool
                     -> Pile (ItemBezier s)
                     -> ConfineTree s
                     -> IO (ConfineTree s)
addPileToConfineTree doShuffle pile mTree =
  do  indices <- indexVector doShuffle size
      numLoopState 0 (size - 1) mTree (go indices)
  where
  size = fromIntegral . unBreadth . view pileBreadth $ pile
  go indices mTree i =
    do j <- V.indexM indices i
       (ItemBezier bez itemTagId) <- pileItem pile j
       return $ addBezierToConfineTree (TaggedBezier bez (CurveTag j) itemTagId) mTree

buildConfineTree :: forall s
                 .  ( Storable (ItemBezier s)
                    , Space s )
                 => Int
                 -> Int
                 -> Pile (ItemBezier s)
                 -> IO (ConfineTree s, DecorateTree s, SweepTrace s)
buildConfineTree traceLimit decorationLimit bezPile =
  do   treeBare <- -- tcP "bareTree" .
                   trWith (show . confineTreeCountOverlaps) "confineTreeCountOverlaps" .
                   trWith (show . confineTreeDepth) "confineTreeDepth" .
                   trWith (show . logBase 2 . (fromIntegral :: Int -> Float) . confineTreeSize) "confineTreeSizeLog" .
                   trWith (show . confineTreeSize) "confineTreeSize" <$>
                   addPileToConfineTree True bezPile Nothing
       -- treeDecorated <- liftIO $ decorateConfineTree bezPile treeBare
       -- (treeDecorated, trace) <- runStateT (sweepConfineTree crossStep
       --                                                       branchStep
       --                                                       (discardOp    traceLimit)
       --                                                       (keepOp       traceLimit)
       --                                                       (nothingOp    traceLimit)
       --                                                       (pushBypassOp traceLimit)
       --                                                       (popBypassOp  traceLimit)
       --                                                       (pushPath     traceLimit)
       --                                                       (popPath      traceLimit)
       --                                                       decorationLimit
       --                                                       treeBare
       --
       --                                                     ) initialTrace
       let trace = initialTrace
       let (treeDecorated, decorationCount) = runState (buildDecorateTree (modify (+1)) decorationLimit treeBare) 0
       --conConfineTree .= treeDecorated
       let total = decorationCount
           count = confineTreeSize treeBare
       putStrLn $ "steps per node    " ++ showFl' 12 (fromIntegral total/fromIntegral count :: Double)
       -- putStrLn ""
       -- putStrLn $ "sweepCrossSteps: " ++ show (trace ^. sweepCrossSteps)
       -- putStrLn $ "averageConsidered " ++ showFl' 12 (total/ fromIntegral count)
       -- putStrLn ""

       return (treeBare, {-trP "treeDecorated\n"-} treeDecorated, trace)
