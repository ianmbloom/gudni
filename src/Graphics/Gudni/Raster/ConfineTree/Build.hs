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
  , SweepTrace(..)
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

data SweepTrace s = SweepTrace
    { _sweepDiscarded :: [Bezier s]
    , _sweepContinue  :: [Bezier s]
    , _sweepSteps     :: Int
    } deriving (Show, Eq, Ord)
makeLenses ''SweepTrace


indexVector :: Bool -> Int -> IO (V.Vector Int)
indexVector doShuffle size =
  do let indices = V.generate size id
     if doShuffle
     then return $ evalRand (shuffleImmutable indices) (mkStdGen 10000)
     else return indices

addPileToConfineTree :: ( Space s
                        , Storable (TaggedBezier s)
                        )
                     => Bool
                     -> Pile (TaggedBezier s)
                     -> ConfineTree s
                     -> IO (ConfineTree s)
addPileToConfineTree doShuffle pile mTree =
  do  indices <- indexVector doShuffle size
      numLoopState 0 (size - 1) mTree (go indices)
  where
  size = fromIntegral . unBreadth . view pileBreadth $ pile
  go indices mTree i =
    do j <- V.indexM indices i
       (TaggedBezier bez itemTagId) <- pileItem pile j
       return $ addBezierToConfineTree itemTagId (CurveTag j) bez mTree

decorateConfineTree :: forall s
                    .  ( Space s
                       , Storable (TaggedBezier s)
                       )
                     => Pile (TaggedBezier s)
                     -> ConfineTree s
                     -> IO (ConfineTree s)
decorateConfineTree pile mTree =
    do  (mTreeDecorate, maxCount) <- numLoopState 0 (size - 1) (mTree, 0) goDecorate
        putStrLn $ "maxCount: " ++ show maxCount
        return mTreeDecorate
   where
   size = fromIntegral . unBreadth . view pileBreadth $ pile
   adder = modify (+1)
   goDecorate :: (ConfineTree s, Int) -> Int -> IO (ConfineTree s, Int)
   goDecorate (mTree, count) i =
     do (TaggedBezier bez itemTagId) <- pileItem pile i
        let (mTree', count') = runState (addCrossingToConfineTree adder itemTagId (CurveTag i) bez mTree) 0
        return (mTree', count + count')

buildConfineTree :: forall s
                 .  ( Storable (TaggedBezier s)
                    , Space s )
                 => Pile (TaggedBezier s)
                 -> IO (ConfineTree s)
buildConfineTree bezPile =
  do   treeBare <- --trP "bareTree" .
                   trWith (show . confineTreeCountOverlaps) "confineTreeCountOverlaps" .
                   trWith (show . confineTreeDepth) "confineTreeDepth" .
                   trWith (show . logBase 2 . (fromIntegral :: Int -> Float) . confineTreeSize) "confineTreeSizeLog" .
                   trWith (show . confineTreeSize) "confineTreeSize" <$>
                   addPileToConfineTree True bezPile Nothing
       -- treeDecorated <- liftIO $ decorateConfineTree bezPile treeBare
       let --tickStep = modify (over _1 (+1))
           --stub   _ = return ()
           tickStep = sweepSteps += 1
           discardOp :: [Bezier s] -> StateT (SweepTrace s) IO ()
           discardOp  list = do sweepDiscarded %= (list++)
           continueOp :: [Bezier s] -> StateT (SweepTrace s) IO ()
           continueOp list = sweepContinue .= list

       (treeDecorated, trace) <- runStateT (sweepConfineTree tickStep discardOp continueOp treeBare) (SweepTrace [] [] 0)
       putStrLn $ "trace: " ++ show trace
       --conConfineTree .= treeDecorated
       let total = confineTreeTotalConsidered treeDecorated
           count = confineTreeSize treeDecorated
       putStrLn $ "steps per node    " ++ showFl' 12 (fromIntegral (view sweepSteps trace)/fromIntegral count :: Double)
       putStrLn $ "averageConsidered " ++ showFl' 12 (total/ fromIntegral count)
       return treeDecorated
