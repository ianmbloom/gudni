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

module Graphics.Gudni.Raster.ConfineTree.SweepTrace
  ( SweepTrace(..)
  , sweepDiscarded
  , sweepContinue
  , sweepStores
  , sweepVisited
  , sweepBypasses
  , sweepPath
  , sweepBranchSteps
  , sweepCrossSteps

  , initialTrace
  , crossStep
  , branchStep
  , keepOp
  , discardOp
  , nothingOp
  , pushBypassOp
  , nullTail
  , popBypassOp
  , pushPath
  , popPath
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
import Graphics.Gudni.Util.Util

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

data SweepTrace s = SweepTrace
    { _sweepDiscarded   :: [TaggedBezier s]
    , _sweepContinue    :: [TaggedBezier s]
    , _sweepStores      :: [SweepStored s]
    , _sweepBypasses    :: [[TaggedBezier s]]
    , _sweepVisited     :: [Box s]
    , _sweepPath        :: [(Point2 s, Point2 s)]
    , _sweepBranchSteps :: Int
    , _sweepCrossSteps  :: Int
    } deriving (Show)
makeLenses ''SweepTrace

initialTrace :: SweepTrace s
initialTrace = SweepTrace [] [] [] [] [] [] 0 0

crossStep :: StateT (SweepTrace s) IO ()
crossStep = sweepCrossSteps += 1

branchStep :: StateT (SweepTrace s) IO ()
branchStep = sweepBranchSteps += 1

keepOp :: Int -> [TaggedBezier s] -> StateT (SweepTrace s) IO ()
keepOp limit list =
  do currentSteps <- use sweepBranchSteps
     when (currentSteps < limit) (sweepContinue .= list)

discardOp :: Int -> [TaggedBezier s] -> StateT (SweepTrace s) IO ()
discardOp limit list =
  do currentSteps <- use sweepBranchSteps
     when (currentSteps < limit) (sweepDiscarded %= (list++))

nothingOp :: Int -> Box s -> StateT (SweepTrace s ) IO ()
nothingOp limit boundary =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepVisited %= (boundary:))

pushBypassOp :: Int -> [TaggedBezier s] -> StateT (SweepTrace s ) IO ()
pushBypassOp limit bypasses =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepBypasses %= (bypasses:))

popBypassOp :: Int -> StateT (SweepTrace s ) IO ()
popBypassOp limit =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepBypasses %= nullTail)


pushPath :: Int -> (Point2 s, Point2 s) -> StateT (SweepTrace s ) IO ()
pushPath limit path =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepPath %= (path:))
      sweepBranchSteps += 1

popPath :: Int ->StateT (SweepTrace s ) IO ()
popPath limit =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepPath %= nullTail)
