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

module Graphics.Gudni.Raster.Dag.ConfineTree.SweepTrace
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
  , overhangOp
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

import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.ConfineTree.Add
import Graphics.Gudni.Raster.Dag.ConfineTree.Decorate
import Graphics.Gudni.Raster.Dag.ConfineTree.Sweep
import Graphics.Gudni.Raster.Dag.ConfineTree.Depth

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

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
    { _sweepDiscarded   :: [PrimTagId]
    , _sweepContinue    :: [PrimTagId]
    , _sweepStores      :: [SweepStored s]
    , _sweepBypasses    :: [[PrimTagId]]
    , _sweepVisited     :: [Box s]
    , _sweepPath        :: [(Point2 s, Point2 s)]
    , _sweepBranchSteps :: Int
    , _sweepCrossSteps  :: Int
    } deriving (Show)
makeLenses ''SweepTrace

initialTrace :: SweepTrace s
initialTrace = SweepTrace [] [] [] [] [] [] 0 0

crossStep :: (Monad m) => StateT (SweepTrace s) m ()
crossStep = sweepCrossSteps += 1

branchStep :: (Monad m) => StateT (SweepTrace s) m ()
branchStep = sweepBranchSteps += 1

overhangOp :: (Monad m) => Int -> [PrimTagId] -> [PrimTagId] -> StateT (SweepTrace s) m ()
overhangOp limit keep discard =
  do currentSteps <- use sweepBranchSteps
     when (currentSteps < limit) $ do sweepContinue .= keep
                                      sweepDiscarded %= (discard++)

nothingOp :: (Monad m) => Int -> Box s -> StateT (SweepTrace s ) m ()
nothingOp limit boundary =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepVisited %= (boundary:))

pushBypassOp :: (Monad m) => Int -> [PrimTagId] -> StateT (SweepTrace s ) m ()
pushBypassOp limit bypasses =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepBypasses %= (bypasses:))

popBypassOp :: (Monad m) => Int -> StateT (SweepTrace s ) m ()
popBypassOp limit =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepBypasses %= nullTail)

pushPath :: (Monad m) => Int -> (Point2 s, Point2 s) -> StateT (SweepTrace s ) m ()
pushPath limit path =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepPath %= (path:))
      sweepBranchSteps += 1

popPath :: (Monad m) => Int ->StateT (SweepTrace s ) m ()
popPath limit =
   do currentSteps <- use sweepBranchSteps
      when (currentSteps < limit) (sweepPath %= nullTail)
