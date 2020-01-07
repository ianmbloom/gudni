{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Job
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions dividing the serialized scene into jobs small enough to run a single
-- OpenCL kernel call.

module Graphics.Gudni.Raster.Job
  ( GeoReference(..)
  , BuildJobsMonad(..)
  , execBuildJobsMonad
  , RasterJob(..)
  , BuildState(..)
  , bsTileCount
  , bsCurrentJob
  , bsJobs
  , rJItemTagPile
  , rJTilePile
  , rJColumnAllocation
  , freeRasterJobs
  , accumulateRasterJobs
  , outputRasterJob
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.StorableM

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Tile
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.ReorderTable
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.ItemInfo

import Control.Monad
import Control.Monad.State
import Control.Monad.Memo.Class

import Control.Lens
import Control.DeepSeq
import Control.Concurrent
import Control.Loop

import Foreign.Storable
import Foreign.C.Types

import Data.List (intercalate)
import Data.Maybe
import qualified Data.Map as M

-- | A RasterJob stores the information needed to transfer data to OpenCL, to render a group of tiles∘
-- Each job corresponds to an individual rasterizer kernel call.
data RasterJob = RasterJob
  { _rJItemTagPile :: !(Pile ItemTag)
  , _rJTilePile  :: !(Pile (Tile (Slice ItemTag, Int)))
  , _rJColumnAllocation :: Int -- number of columns allocated for this job.
  } deriving (Show)
makeLenses ''RasterJob

-- | A BuildState allows a monad to pass over a data structure, such as a TileTree while building a stack
-- of RasterJobs
data BuildState = BuildState
  { _bsTileCount :: Int -- number of tiles added to the current job.
  , _bsCurrentJob :: RasterJob -- current job
  , _bsJobs  :: [RasterJob] -- accumulated jobs.
  }
makeLenses ''BuildState

-- | Monad for building RasterJobs
type BuildJobsMonad m = StateT BuildState m

-- | Run a RasterJobMonad and return the state.
execBuildJobsMonad :: MonadIO m => BuildJobsMonad m a -> m BuildState
execBuildJobsMonad code =
  do -- prime the stack of jobs
     initJob <- newRasterJob
     -- execute the monad (this is usually passing over a structure of tiles and adding each tile to the monad. )
     execStateT code (BuildState 0 initJob [])

-- | Create a new rasterJob with default allocation sizes.
newRasterJob :: MonadIO m => m RasterJob
newRasterJob = liftIO $
    do  initItemTagPile <- newPile :: IO (Pile ItemTag)
        initTilePile  <- newPile :: IO (Pile (Tile (Slice ItemTag, Int)))
        return RasterJob
            { _rJItemTagPile = initItemTagPile
            , _rJTilePile  = initTilePile
            , _rJColumnAllocation = 0
            }

-- | Free all memory allocated by the 'RasterJob'
freeRasterJob :: RasterJob -> IO ()
freeRasterJob job =
    do  freePile $ job ^. rJItemTagPile
        freePile $ job ^. rJTilePile

-- | Free a list of RasterJobs
freeRasterJobs :: MonadIO m => [RasterJob] -> m ()
freeRasterJobs jobs = liftIO $ mapM_ freeRasterJob jobs

-- | Add a tile to a RasterJob
addTileToRasterJob :: MonadIO m
                   => Tile TileEntry
                   -> StateT RasterJob m ()
addTileToRasterJob tile =
  do  -- get the list of new shapes from the tile entry
      let items = map (view itemEntryTag) $ tile ^. tileRep . tileItems
      -- add the stripped shapes to the raster job and get the range of the added shapes
      slice <- addListToPileState rJItemTagPile items
      -- strip the tile down so just the range of shapes is left
      columnAllocation <- use rJColumnAllocation
      let tileInfo = set tileRep (slice, columnAllocation) tile
      -- add it to the pile of tiles for the job.
      addToPileState rJTilePile tileInfo
      return ()

-- | Add a tile to the BuildState, adding a new RasterJob if we go over the maximum per tile.
accumulateRasterJobs :: MonadIO m
                     => Int
                     -> Int
                     -> Tile TileEntry
                     -> BuildJobsMonad m ()
accumulateRasterJobs maxTilesPerJob threadsPerTile tile =
  do  -- get the current stack of jobs
      jobs <- use bsJobs
      -- get the counter for the number of tiles in the job on top of the stack
      count <- use bsTileCount
      -- check if there is room for another tile in the job
      currentJob <- use bsCurrentJob
      if count >= maxTilesPerJob
      then do -- if not create a new job and add it to the stack
              bsJobs %= (currentJob:)
              newJob <- liftIO $ newRasterJob
              bsCurrentJob .= newJob
              -- reset the counter
              bsTileCount .= 0
              -- rerun the function with the new job on top of the state
              accumulateRasterJobs maxTilesPerJob threadsPerTile tile
      else do -- otherwise grab the job on the top of the stack
              -- add the new tile to the job
              (bsCurrentJob .=) =<<  execStateT (addTileToRasterJob tile) currentJob

              -- increment the counter
              bsTileCount += 1
              bsCurrentJob . rJColumnAllocation += threadsPerTile -- (fromIntegral . widthOf $ tile ^. tileBox)


-- | Output a RasterJob in IO
outputRasterJob :: RasterJob -> IO ()
outputRasterJob job =
  do
    putStrLn "---------------- rJShapePile ----------------------"
    print . view rJItemTagPile $ job
    putStrList =<< (pileToList . view rJItemTagPile $ job)
    putStrLn "---------------- rJTilePile -----------------------"
    putStrList =<< (pileToList . view rJTilePile $ job)

instance NFData RasterJob where
  rnf (RasterJob a b c) = a `deepseq` b `deepseq` c `deepseq` ()
