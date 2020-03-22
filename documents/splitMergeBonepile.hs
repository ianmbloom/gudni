
buildRasterJobs :: (MonadIO m, Show token)
                => RasterParams token
                -> GeometryMonad m [RasterJob]
buildRasterJobs params =
  do  -- Get the tile tree from the geometryState
      tileTree <- use geoTileTree
      -- Determine the maximum number of tiles per RasterJob
      let maxThresholds = NumStrands $ fromIntegral $ params ^. rpRasterizer . rasterSpec . specMaxThresholds
          tilesPerCall = tr "tilesPerCall" $ fromIntegral $ params ^. rpRasterizer . rasterSpec . specMaxTilesPerJob
          threadsPerBlock = tr "threadsPerBlock" $ fromIntegral $ params ^. rpRasterizer . rasterSpec . specColumnsPerBlock
          splitTree = {-tr "splitTree" $-} splitTreeTiles maxThresholds tileTree

      -- Build all of the RasterJobs by traversing the TileTree.

      geoTileTree .= splitTree
      (numberedTileTree, finalState) <- runBuildJobsMonad (traverseTileTree (accumulateRasterJobs tilesPerCall threadsPerBlock) splitTree)
      let jobs = finalState ^. bsCurrentJob : finalState ^. bsJobs
          pointTileQueries = tr "pointTileQueries" $ map (\(queryId, loc) -> (queryId, loc, locatePointInTileTree numberedTileTree loc)) (params ^. rpPointQueries)
          jobsWithQueries = foldl addPointQueryToRasterJobs jobs pointTileQueries
      return $ trWith (show . length) "num jobs" $ jobsWithQueries

-- | Generate an call the rasterizer kernel. Polymorphic over the DrawTarget type.
generateCall  :: forall a b token .
                 (  KernelArgs
                   'KernelSync
                   'NoWorkGroups
                   'UnknownWorkItems
                   'Z
                   (a
                   -> NumWorkItems
                   -> WorkGroup
                   -> CL ())
                 , Show a, Show token
                 )
              => RasterParams token
              -> BuffersInCommon
              -> RasterJob
              -> Point2 CInt
              -> CInt
              -> CInt
              -> a
              -> CL [(PointQueryId,SubstanceTagId)]
generateCall params bic job bitmapSize frameCount jobIndex target =
  do

    -> BuffersInCommon
    -> CInt
    -> RasterJob
    -> CInt
    -> CL [(PointQueryId, SubstanceTagId)]

    return (tr "queryResults" $ zip (map pqQueryId queries) queryResults)
    -- Run the rasterizer over each rasterJob inside a CLMonad.
    queryResults <- concat <$> zipWithM (raster params bic frameCount) jobs [0..]

, BuildJobsMonad(..)
, runBuildJobsMonad
, RasterJob(..)

, bsTileCount
, bsCurrentJob
, bsJobs

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , BuildState(..)

  , rJItemTagIdPile
  , rJTilePile
  , rJThreadAllocation
  , rJPointQueries
  , PointQueryId(..)
  , PointQuery(..)
  , PointQuerySequence(..)
  , freeRasterJobs
  , accumulateRasterJobs
  , addPointQueryToRasterJobs
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
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.ReorderTable
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.ItemInfo

import Graphics.Gudni.Interface.Query

import Control.Monad
import Control.Monad.State
import Control.Monad.Memo.Class

import Control.Lens
import Control.DeepSeq
import Control.Concurrent
import Control.Loop

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import Data.List (intercalate)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Foldable
import Data.List.Lens


newtype JobId = JobId {unJobId :: Int} deriving (Show, Eq, Ord, Num)

-- | A RasterJob stores the information needed to transfer data to OpenCL, to render a group of tiles∘
-- Each job corresponds to an individual rasterizer kernel call.
data RasterJob = RasterJob
  { _rJItemTagIdPile :: !(Pile ItemTagId)
  , _rJTilePile  :: !(Pile (Tile (Slice ItemTagId, Int)))
  , _rJThreadAllocation :: Int -- total number of threads allocated for this job.
  , _rJPointQueries :: PointQuerySequence
  } deriving (Show)
makeLenses ''RasterJob

-- | A BuildState allows a monad to pass over a data structure, such as a TileTree while building a stack
-- of RasterJobs
data BuildState = BuildState
  { _bsTileCount :: TileId -- number of tiles added to the current job.
  , _bsCurrentJob :: RasterJob -- current job
  , _bsJobCount :: JobId
  , _bsJobs  :: [RasterJob] -- accumulated jobs.
  }
makeLenses ''BuildState

-- | Monad for building RasterJobs
type BuildJobsMonad m = StateT BuildState m

-- | Run a RasterJobMonad and return the state.
runBuildJobsMonad :: MonadIO m => BuildJobsMonad m a -> m (a, BuildState)
runBuildJobsMonad code =
  do -- prime the stack of jobs
     initJob <- newRasterJob
     -- execute the monad (this is usually passing over a structure of tiles and adding each tile to the monad. )
     runStateT code (BuildState 0 initJob 0 [])

-- | Create a new rasterJob with default allocation sizes.
newRasterJob :: MonadIO m => m RasterJob
newRasterJob = liftIO $
    do  initItemTagIdPile <- newPile :: IO (Pile ItemTagId)
        initTilePile  <- newPile :: IO (Pile (Tile (Slice ItemTagId, Int)))
        return RasterJob
            { _rJItemTagIdPile = initItemTagIdPile
            , _rJTilePile  = initTilePile
            , _rJThreadAllocation = 0
            , _rJPointQueries = S.empty
            }

-- | Free all memory allocated by the 'RasterJob'
freeRasterJob :: RasterJob -> IO ()
freeRasterJob job =
    do  freePile $ job ^. rJItemTagIdPile
        freePile $ job ^. rJTilePile

-- | Free a list of RasterJobs
freeRasterJobs :: MonadIO m => [RasterJob] -> m ()
freeRasterJobs jobs = liftIO $ mapM_ freeRasterJob jobs

-- | Add a tile to a RasterJob
addTileToRasterJob :: MonadIO m
                   => Tile (S.Seq ItemEntry)
                   -> StateT RasterJob m ()
addTileToRasterJob tile =
  do  -- get the list of new shapes from the tile entry
      let items = toList . fmap (view itemEntryTagId) $ tile ^. tileRep
      -- add the stripped shapes to the raster job and get the range of the added shapes
      slice <- addListToPileState rJItemTagIdPile items
      -- strip the tile down so just the range of shapes is left
      threadAllocation <- use rJThreadAllocation
      let tileInfo = set tileRep (slice, threadAllocation) tile
      -- add it to the pile of tiles for the job.
      addToPileState rJTilePile tileInfo
      return ()

-- | Add a tile to the BuildState, adding a new RasterJob if we go over the maximum per tile.
accumulateRasterJobs :: MonadIO m
                     => TileId
                     -> Int
                     -> Tile (S.Seq ItemEntry)
                     -> BuildJobsMonad m (Tile (JobId, TileId))
accumulateRasterJobs maxTilesPerJob threadsPerBlock tile =
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
              bsJobCount += 1
              bsTileCount .= 0
              -- rerun the function with the new job on top of the state
              accumulateRasterJobs maxTilesPerJob threadsPerBlock tile
      else do -- otherwise grab the job on the top of the stack
              -- add the new tile to the job
              currentTileId <- use bsTileCount
              currentJobId  <- use bsJobCount
              (bsCurrentJob .=) =<<  execStateT (addTileToRasterJob tile) currentJob
              -- increment the counter
              bsTileCount += 1
              bsCurrentJob . rJThreadAllocation += threadsPerBlock -- (fromIntegral . widthOf $ tile ^. tileBox)
              return (set tileRep (currentJobId, currentTileId) tile)

-- | Add a point query to a raster job
addPointQueryToRasterJobs :: [RasterJob] -> (PointQueryId, Point2 SubSpace, (JobId, TileId)) -> [RasterJob]
addPointQueryToRasterJobs jobs (queryId, location, (JobId jobId, tileId)) =
   over (ix jobId . rJPointQueries) (|>(PointQuery tileId queryId location)) jobs

-- | Output a RasterJob in IO
outputRasterJob :: RasterJob -> IO ()
outputRasterJob job =
  do putStrLn "---------------- rJShapePile ----------------------"
     print . view rJItemTagIdPile $ job
     putStrList =<< (pileToList . view rJItemTagIdPile $ job)
     putStrLn "---------------- rJTilePile -----------------------"
     putStrList =<< (pileToList . view rJTilePile $ job)

instance NFData RasterJob where
    rnf (RasterJob a b c d ) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()


    columnAllocation :: CInt <- peekM
    slice  <- peekM


    runCheckSplitKernel :: RasterParams token
                        -> BlockSection
                        -> S.Seq Tile
                        -> S.Seq (Slice BlockId)
                        -> S.Seq BlockId
                        -> CL (S.Seq Int)
    runCheckSplitKernel params thresholdBuffers tiles checkBlockSlices checkBlocks =
        let context        = clContext (params ^. rpRasterizer . rasterClState)
            threadsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
            maxJobSize     = params ^. rpRasterizer . rasterDeviceSpec . specMaxCheckSplitJobSize
            checkJobs      = divideIntoJobs maxJobSize checkBlockSlices
        in
        do
        tileBuffer            <- liftIO . vectorToBuffer context . fromSequence $ tiles
        checkBlockSliceBuffer <- liftIO . vectorToBuffer context . fromSequence $ checkBlockSlices
        checkBlockBuffer      <- liftIO . vectorToBuffer context . fromSequence $ checkBlocks
        maximumsBuffer        <- newBuffer (S.length tiles) :: CL (CLBuffer CInt)
        S.traverseWithIndex (
               \jobIndex (jobSize, jobOffset) ->
               do liftIO $ putStrLn ("checkSplitKernel " ++ show jobIndex ++ "           XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
                  runKernel  (params ^. rpRasterizer . rasterCheckSplitKernel)
                             (thresholdBuffers ^. tbTileBuffer      )
                             (thresholdBuffers ^. tbQueueSliceBuffer)

                             checkBlockSliceBuffer
                             checkBlockBuffer
                             (toCInt  $  params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth)
                             (toCInt <$> params ^. rpFrameSpec  . specBitmapSize)
                             (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                             (toCInt jobIndex)
                             (toCInt jobOffset)
                             maximumsBuffer
                             (Work2D jobSize threadsPerBlock)
                             (WorkGroup [1, threadsPerBlock]) :: CL ()
               ) checkJobs
        maximumThresholds <- fmap fromCInt . S.fromList . VS.toList <$> readBuffer maximumsBuffer
        liftIO . clReleaseMemObject . bufferObject $ checkBlockSliceBuffer
        liftIO . clReleaseMemObject . bufferObject $ maximumsBuffer
        return maximumThresholds


data SplitState = SplitState
   { _splitShoulds :: S.Seq Bool
   , _splitTiles   :: S.Seq Tile
   , _splitBlocks  :: S.Seq BlockId
   , _splitNextBlocKId :: BlockId
   }
makeLenses ''SplitState

addSplitTile top bottom topBlock bottomBlock =
  do splitTiles  %= \tiles  -> tiles |> top |> bottom
     splitBlocks %= \blocks -> blocks |> topBlock |> bottomBlock

popShould :: State SplitState Bool
popShould =
   do shouldSplits <- use splitShoulds
      let (should, rest) = case S.viewl shouldSplits of
                             (S.:<) should rest -> (should, rest)
                             S.EmptyL -> error "shouldSplits is empty"
      splitShoulds .= rest
      return should

makeSplits :: S.Seq (Tile, S.Seq BlockId) -> State SplitState (S.Seq (Tile, S.Seq BlockId))
makeSplits ss = join <$> mapM go ss
  where go :: (Tile, S.Seq BlockId) -> State SplitState (S.Seq (Tile, S.Seq BlockId))
        go (tile, blocks) =
            do nextBlockId <- use splitNextBlocKId
               should <- popShould
               if should
               then let (top, bottom) = verticalSplitTile tile
                        len = S.length blocks
                        newBlocks = S.iterateN len (+1) nextBlockId
                    in  do traverse (uncurry (addSplitTile top bottom)) $ S.zip blocks newBlocks
                           splitNextBlocKId += BlockId len
                           return . S.fromList  $ [(top, blocks),(bottom,newBlocks)]
                else  return . S.singleton $ (tile,blocks)

shouldSplitTile :: RasterParams token -> Tile -> Int -> Bool
shouldSplitTile params tile maxThresholdCount =
  let maxThresholds = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
  in  heightOf (tile ^. tileBox) > 1 && maxThresholdCount > maxThresholds


splitTileLoop :: RasterParams token
              -> BlockSection
              -> TileTree (S.Seq (Tile, S.Seq BlockId))
              -> BlockId
              -> BlockId
              -> CL (TileTree (S.Seq (Tile, S.Seq BlockId)))
splitTileLoop params thresholdBuffers tileTree allocation availableAllocation =
    -- Collect all of the tiles that need to be split.
    do let (checkTiles, checkBlockSlices, checkBlocks, _) = execState (traverseTileTree collectTileBlocks tileTree) (S.empty, S.empty, S.empty, 0)
       -- maximums <- tr "maximums" <$> runCheckSplitKernel params thresholdBuffers checkTiles checkBlockSlices checkBlocks
       -- let shouldSplits = S.zipWith (shouldSplitTile params) checkTiles maximums
       -- if or shouldSplits
       -- then do let (splitTileTree, (SplitState _ splitTiles splitBlocks newAllocation)) = runState (traverseTileTree makeSplits tileTree) (SplitState shouldSplits S.empty S.empty allocation)
       --         adjustedBlockSection <- if newAllocation <= availableAllocation
       --                                     then return thresholdBuffers
       --                                     else error "out of blocks"
       --         runSplitKernel params adjustedBlockSection splitTiles splitBlocks
       --         splitTileLoop params thresholdBuffers splitTileTree newAllocation availableAllocation
       -- else return tileTree
       return tileTree

iterateJobSequence :: forall m a
                   . Monad m
                   => (a -> Int)
                   -> S.Seq a
                   -> (Int -> Int -> a -> m ())
                   -> m ()
iterateJobSequence getLength ssss f = go 0 0 ssss
   where go :: Monad m => Int -> Int -> S.Seq a -> m ()
         go count offset sss =
            case S.viewl sss of
              S.EmptyL -> return ()
              (S.:<) s ss  -> do let size = getLength s
                                 f count offset s
                                 go (count + 1) (offset + size) ss
