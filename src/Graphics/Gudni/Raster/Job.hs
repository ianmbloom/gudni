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
  , rJShapePile
  , rJTilePile
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

import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.StrandLookupTable
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.ShapeInfo

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
  { _rJShapePile :: !(Pile (Shape GeoReference))
  , _rJTilePile  :: !(Pile (Tile (Slice (Shape GeoReference))))
  } deriving (Show)
makeLenses ''RasterJob

-- | A BuildState allows a monad to pass over a data structure, such as a TileTree while building a stack
-- of RasterJobs
data BuildState = BuildState
  { _bsTileCount :: Int -- number of tiles added to the current job.
  , _bsJobs  :: [RasterJob] -- accumulated jobs.
  }
makeLenses ''BuildState

-- | Monad for building RasterJobs
type BuildJobsMonad m = StateT BuildState m

-- | Run a RasterJobMonad and return the state.
execBuildJobsMonad :: MonadIO m => BuildJobsMonad m a -> m [RasterJob]
execBuildJobsMonad code =
  do -- prime the stack of jobs
     initJob <- newRasterJob
     -- execute the monad (this is usually passing over a structure of tiles and adding each tile to the monad. )
     fmap (view bsJobs) $ execStateT code (BuildState 0 [initJob])

-- | Create a new rasterJob with default allocation sizes.
newRasterJob :: MonadIO m => m RasterJob
newRasterJob = liftIO $
    do  initShapePile <- newPile :: IO (Pile (Shape GeoReference))
        initTilePile  <- newPile :: IO (Pile (Tile (Slice (Shape GeoReference))))
        return RasterJob
            { _rJShapePile = initShapePile
            , _rJTilePile  = initTilePile
            }

-- | Free all memory allocated by the 'RasterJob'
freeRasterJob :: RasterJob -> IO ()
freeRasterJob job =
    do  freePile $ job ^. rJShapePile
        freePile $ job ^. rJTilePile

-- | Free a list of RasterJobs
freeRasterJobs :: MonadIO m => [RasterJob] -> m ()
freeRasterJobs jobs = liftIO $ mapM_ freeRasterJob jobs

-- Strip out the ShapeEntry data leaving only a Shape which refers to its geometric data.
referenceShape :: Shape ShapeEntry -> Shape GeoReference
referenceShape (Shape info shapeEntry) = Shape info (shapeEntry ^. shapeGeoRef)

-- | Add the shape to the pile of shapes in a RasterJob and return a reference to it.
appendShapes :: MonadIO m
            => [Shape GeoReference]
            -> StateT RasterJob m (Slice (Shape GeoReference))
appendShapes shapes =
       addListToPileState rJShapePile shapes

-- | Add a tile to a RasterJob
addTileToRasterJob :: MonadIO m
                   => Tile TileEntry
                   -> StateT RasterJob m ()
addTileToRasterJob tile =
  do  -- get the list of new shapes from the tile entry
      let shapeEntries = tile ^. tileRep . tileShapes
      --  strip the strandcount and bounding box out of the shape containers leaving only the
      --  reference to the geometric data.
          referenceEntries = map referenceShape shapeEntries
      -- add the stripped shapes to the raster job and get the range of the added shapes
      slice <- appendShapes referenceEntries
      -- strip the tile down so just the range of shapes is left
      let tileInfo = set tileRep slice tile
      -- add it to the pile of tiles for the job.
      addToPileState rJTilePile tileInfo
      return ()

-- | Add a tile to the BuildState, adding a new RasterJob if we go over the maximum per tile.
accumulateRasterJobs :: MonadIO m
                     => Int
                     -> Tile TileEntry
                     -> BuildJobsMonad m ()
accumulateRasterJobs maxTilesPerJob tile =
  do  -- get the current stack of jobs
      jobs <- use bsJobs
      -- get the counter for the number of tiles in the job on top of the stack
      count <- use bsTileCount
      -- check if there is room for another tile in the job
      if count >= maxTilesPerJob
      then do -- if not create a new job and add it to the stack
              newJob <- newRasterJob
              bsJobs .= newJob:jobs
              -- reset the counter
              bsTileCount .= 0
              -- rerun the function with the new job on top of the state
              accumulateRasterJobs maxTilesPerJob tile
      else do -- otherwise grab the job on the top of the stack
              let currentJob = head jobs
              -- add the new tile to the job
              job' <- execStateT (addTileToRasterJob tile) currentJob
              -- put it back on the stack
              bsJobs .= job':tail jobs
              -- increment the counter
              bsTileCount += 1

-- | Output a RasterJob in IO
outputRasterJob :: RasterJob -> IO ()
outputRasterJob job =
  do
    putStrLn "---------------- rJShapePile ----------------------"
    print . view rJShapePile $ job
    putStrList =<< (pileToList . view rJShapePile $ job)
    putStrLn "---------------- rJTilePile -----------------------"
    putStrList =<< (pileToList . view rJTilePile $ job)

instance NFData RasterJob where
  rnf (RasterJob a b) = a `deepseq` b `deepseq` ()
