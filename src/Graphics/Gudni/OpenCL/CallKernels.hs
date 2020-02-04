{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Graphics.Gudni.OpenCL.CallKernels
  ( runRaster
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.PrepareBuffers
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.SubstanceInfo
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Enclosure(NumStrands(..))
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM

import CLUtil
import CLUtil.KernelArgs
import CLUtil.VectorBuffers

import qualified Data.Map      as M
import qualified Data.Sequence as S
import qualified Data.Vector   as V
import qualified Data.Vector.Storable as VS
import Data.Traversable
import Data.Foldable

import Control.Monad.Identity
import Control.Monad.State
import Control.Lens
import Control.Applicative

import Foreign.Storable
import Foreign.Ptr

fromSequence :: Storable a => S.Seq a -> VS.Vector a
fromSequence = VS.fromList . toList

cInt :: Int -> CInt
cInt = fromIntegral

-- A word about terms used below
-- A Block is trio fixed size blocks of memory used to store lists or thresholds, headers (threshold metadata), and queueSlices (information about the list of thresholds)
--   for each thread of a given kernel call.
-- A Tile is just the description of a given area of the screen. A tile can have multiple blocks but each block only has one tile.
-- A Job is a group of blocks, after it's been broken into chunks that an individual kernel call can handle (without timing out).

runGenerateThresholdsKernel :: RasterParams token
                            -> ThresholdBuffers
                            -> BuffersInCommon
                            -> S.Seq Tile
                            -> S.Seq GeneratorBlock
                            -> CL ()
runGenerateThresholdsKernel params thresholdBuffers buffersInCommon tiles generators =
  let threadsPerTile = cInt $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
      maxJobSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxGenerateJobSize
      generatorJobs = S.chunksOf maxJobSize generators
  in
  do
  S.traverseWithIndex (
         \jobIndex generatorJob ->
         do liftIO $ putStrLn ("rasterGenerateThresholdsKernel " ++ show jobIndex ++ " XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
            runKernel (params ^. rpRasterizer . rasterGenerateThresholdsKernel)
                      -- input job buffer
                      (fromSequence tiles )
                      (fromSequence generatorJob)
                      -- constant data buffers
                      (buffersInCommon ^. bicGeoBuffer    )
                      (buffersInCommon ^. bicGeoRefBuffer )
                      (buffersInCommon ^. bicPictFacets   )
                      (buffersInCommon ^. bicItemTagBuffer)
                      (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                      (cInt  $  params ^. rpFrameSpec . specFrameCount)
                      (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                      (cInt jobIndex)
                      -- return buffers
                      (thresholdBuffers ^. tbThresholdBuffer          )
                      (thresholdBuffers ^. tbHeaderBuffer             )
                      (thresholdBuffers ^. tbQueueSliceBuffer)
                      (Work2D (S.length generatorJob) (fromIntegral threadsPerTile))
                      (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
          ) generatorJobs
  return ()

runCheckSplitKernel :: RasterParams token
                    -> ThresholdBuffers
                    -> S.Seq Tile
                    -> S.Seq MergeBlock
                    -> CL ()
runCheckSplitKernel params thresholdBuffers tiles mergeBlocks =
    let threadsPerTile = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxCheckSplitJobSize
        mergeJobs = S.chunksOf maxJobSize mergeBlocks
    in
    do
    S.traverseWithIndex (
           \jobIndex mergeJob ->
           do liftIO $ putStrLn ("sortThresholdsKernel " ++ show jobIndex ++ "           XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterCheckSplitKernel)
                        (thresholdBuffers ^. tbThresholdBuffer          )
                        (thresholdBuffers ^. tbHeaderBuffer             )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence tiles)
                        (fromSequence mergeJob)
                        (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                        (cInt  $  params ^. rpFrameSpec . specFrameCount)
                        (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt jobIndex)
                        (Work2D (S.length mergeJob) (threadsPerTile))
                        (WorkGroup [1, threadsPerTile]) :: CL ()
           ) mergeJobs
    return ()

data SplitBlock = SplitBlock BlockId BlockId

runSplitTileKernel :: RasterParams token
                   -> ThresholdBuffers
                   -> S.Seq Tile
                   -> S.Seq SplitBlock
                   -> CL ()
runSplitTileKernel params thresholdBuffers tiles splitBlocks =
    let threadsPerTile = fromIntegral $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize     =                params ^. rpRasterizer . rasterDeviceSpec . specMaxSplitJobSize
        splitJobs = S.chunksOf maxJobSize splitBlocks
    in
    do
    S.traverseWithIndex (
           \jobIndex splitJob ->
           do liftIO $ putStrLn ("rasterSplitTileKernel " ++ show jobIndex ++ "         XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterSplitTileKernel)
                        (thresholdBuffers ^. tbThresholdBuffer )
                        (thresholdBuffers ^. tbHeaderBuffer    )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence tiles)
                        (fromSequence splitJob)
                        (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                        (cInt  $  params ^. rpFrameSpec  . specFrameCount)
                        (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt jobIndex)
                        (Work2D (S.length splitJob) (fromIntegral threadsPerTile))
                        (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
           ) splitJobs
    return ()

data MergeBlock = MergeBlock BlockId BlockId

runMergeTileKernel :: RasterParams token
                   -> ThresholdBuffers
                   -> S.Seq Tile
                   -> S.Seq MergeBlock
                   -> CL ()
runMergeTileKernel params thresholdBuffers tiles mergeBlocks =
    let threadsPerTile = fromIntegral $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize     =                params ^. rpRasterizer . rasterDeviceSpec . specMaxMergeJobSize
        mergeJobs = S.chunksOf maxJobSize mergeBlocks
    in
    do
    S.traverseWithIndex (
           \jobIndex job ->
           do liftIO $ putStrLn ("mergeTileKernel " ++ show jobIndex ++ "               XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterMergeTileKernel)
                        (thresholdBuffers ^. tbThresholdBuffer )
                        (thresholdBuffers ^. tbHeaderBuffer    )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence tiles)
                        (fromSequence job)
                        (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                        (cInt  $  params ^. rpFrameSpec . specFrameCount)
                        (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt jobIndex)
                        (Work2D (S.length job) (fromIntegral threadsPerTile))
                        (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
           ) mergeJobs
    return ()


runSortTileKernel :: RasterParams token
                  -> ThresholdBuffers
                  -> S.Seq BlockId
                  -> CL ()
runSortTileKernel params thresholdBuffers sortTiles =
    let threadsPerTile = fromIntegral $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxSortJobSize
        jobs = S.chunksOf maxJobSize sortTiles
    in
    do
    S.traverseWithIndex (
           \jobIndex job ->
           do liftIO $ putStrLn ("sortThresholdsKernel " ++ show jobIndex ++ "          XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterSortThresholdsKernel)
                        (thresholdBuffers ^. tbThresholdBuffer )
                        (thresholdBuffers ^. tbHeaderBuffer    )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence job)
                        (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                        (cInt $ params ^. rpFrameSpec . specFrameCount)
                        (cInt $ params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt jobIndex)
                        (Work2D (S.length job) (fromIntegral threadsPerTile))
                        (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
           ) jobs
    return ()

runRenderTileKernel :: (  KernelArgs
                         'KernelSync
                         'NoWorkGroups
                         'UnknownWorkItems
                         'Z
                         (target -> NumWorkItems -> WorkGroup -> CL ())
                       )
                    => RasterParams token
                    -> ThresholdBuffers
                    -> BuffersInCommon
                    -> S.Seq Tile
                    -> S.Seq BlockId
                    -> target
                    -> CL ()
runRenderTileKernel params thresholdBuffers buffersInCommon tiles renderBlocks target =
    let threadsPerTile = fromIntegral $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxRenderJobSize
        renderJobs = S.chunksOf maxJobSize renderBlocks
    in
    do
    S.traverseWithIndex (
           \jobIndex renderJob ->
           do liftIO $ putStrLn ("rasterRenderThresholdsKernel " ++ show jobIndex ++ "   XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterRenderThresholdsKernel)
                        (thresholdBuffers ^. tbThresholdBuffer )
                        (thresholdBuffers ^. tbHeaderBuffer    )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence tiles)
                        (fromSequence renderJob)
                        (buffersInCommon ^. bicItemTagBuffer)
                        (buffersInCommon ^. bicSubTagBuffer )
                        (buffersInCommon ^. bicPictFacets   )
                        (buffersInCommon ^. bicPictBuffer   ) -- (params ^. rpPictData)
                        (buffersInCommon ^. bicPictMemBuffer)
                        (buffersInCommon ^. bicSolidColors  )
                        (buffersInCommon ^. bicRandoms      ) -- (params ^. rpGeometryState  . geoRandomField)
                        (params ^. rpSubstanceState . suBackgroundColor)
                        (cInt <$> params ^. rpFrameSpec ^. specBitmapSize)
                        (cInt $ params ^. rpFrameSpec ^. specFrameCount)
                        (cInt $ params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt jobIndex)
                        target
                        (Work2D (S.length renderJob) (fromIntegral threadsPerTile))
                        (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
          ) renderJobs
    return ()

runPointQueryKernel :: RasterParams token
                    -> ThresholdBuffers
                    -> BuffersInCommon
                    -> S.Seq BlockId
                    -> CL [PointQueryResult SubstanceTagId]
runPointQueryKernel params thresholdBuffers buffersInCommon pointQueries =
    do
       --liftIO $ putStrLn ("rasterPointQueryKernel " ++ show jobIndex ++ "         XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
       -- queryResults <- let numPointQueries = length $ tr "pointQueries" $ job ^. rJPointQueries in
       --                 if  numPointQueries <= 0
       --                 then return []
       --                 else (toList :: CLUtil.Vector SubstanceTagId -> [SubstanceTagId]) <$>
       --                      runKernel (params ^. rpRasterizer . rasterQueryKernel)
       --                                (thresholdBuffers ^. tbThresholdBuffer )
       --                                (thresholdBuffers ^. tbHeaderBuffer    )
       --                                (thresholdBuffers ^. tbQueueSliceBuffer)
       --                                (buffersInCommon  ^. bicPictFacets)
       --                                (job    ^. rJTilePile)
       --                                (params ^. rpFrameSpec ^. specBitmapSize)
       --                                (params ^. rpFrameSpec ^. specFrameCount)
       --                                (params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)

       --                                jobIndex
       --                                (fromIntegral numPointQueries :: CInt)
       --                                (VS.fromList queries)
       --                                (Out numPointQueries)
       --                                (Work2D numTiles (fromIntegral threadsPerTile))
       --                                (WorkGroup [1, fromIntegral threadsPerTile])
       let queryResults = []
       return queryResults


-- | Divide entrySequence based on number of items and total number of strands
divideEntrySequences :: RasterParams token
                     -> (Tile, S.Seq ItemEntry)
                     -> Identity (Tile, (S.Seq (S.Seq ItemEntry)))
divideEntrySequences params (tile, rep) = return $ (tile, go rep)
  where
  go ss = let len = S.length ss
              maxLayers     = params ^. rpRasterizer . rasterDeviceSpec . specMaxLayers
              maxThresholds = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
          in
          if (len > 1) && (len > maxLayers || (sum (fmap (unNumStrands . view itemStrandCount) ss) * 3) > fromIntegral maxThresholds)
          then let (left, right) = S.splitAt (len `div` 2) ss
               in go left <|> go right
          else S.singleton ss

newtype BlockId = BlockId {unBlockId :: Int} deriving (Eq, Ord, Show, Num)

allocateTilePortion :: Tile
                    -> Slice ItemTagId
                    -> State ( S.Seq Tile
                             , S.Seq GeneratorBlock
                             , BlockId) BlockId
allocateTilePortion tile slice =
  do (tiles, generatorJobs, blockId) <- get
     put (tiles |> tile, generatorJobs |> GeneratorBlock slice, blockId + 1)
     return blockId


data GeneratorBlock = GeneratorBlock (Slice ItemTagId)

allocateGeneration :: (Tile, S.Seq (Slice ItemTagId))
                   -> State ( S.Seq Tile
                            , S.Seq GeneratorBlock
                            , BlockId) (S.Seq (Tile,S.Seq BlockId))
allocateGeneration (tile, slices) =
    do blockIds <- mapM (allocateTilePortion tile) slices
       return (S.singleton (tile, blockIds))

--mustSplitTile :: S.Seq (Tile (S.Seq (S.Seq ItemEntry))) -> State (S.S.Seq (Tile (S.S
checkSplitTile = undefined
  -- if heightOf tile > 1 && sums of columns of thresholds cannot be split.
  -- if the height is <= 1 then we can just take the top thresholds

splitTileLoop :: RasterParams token
              -> ThresholdBuffers
              -> TileTree (Tile, S.Seq BlockId)
              -> BlockId
              -> CL (TileTree (Tile, S.Seq BlockId), BlockId)
splitTileLoop params thresholdBuffers tileTree allocation =
    -- Collect all of the tiles that need to be split.
    let (splitTileSequence,(splitPairs, newAllocation)) = runState (traverseTileTree checkSplitTile tileTree) (S.empty, allocation)
    in
    if length splitPairs > 0
    then -- Check allocation newAllocations and reallocate buffers if necessary
      do (dataBlocks, queueBlocks) <- if (undefined {-newAllocaion-} > allocation || undefined {- available allocation-})
                                      then undefined
                                        -- make new buffers
                                        -- copy old buffers to new
                                        -- get rid of old buffers
                                      else undefined
                                        -- use current buffers
         -- Divide all of the split tiles into chunks we know can execute in one kernel.
         let
         -- Execute all of the split jobs.
         runSplitTileKernel params thresholdBuffers undefined undefined -- tiles splitBlocks
         --
         splitTileLoop params thresholdBuffers splitTileSequence newAllocation
    else return (splitTileSequence, allocation)


addPortionToPile :: S.Seq ItemEntry -> StateT (Pile ItemTagId) CL (Slice ItemTagId)
addPortionToPile portion =
    do pile <- get
       (pile', slice) <- liftIO $ addSequenceToPile pile (fmap (view itemEntryTagId) portion)
       put pile'
       return slice

makeItemEntrySlice :: (Tile, S.Seq (S.Seq ItemEntry)) -> StateT (Pile ItemTagId) CL (Tile, S.Seq (Slice ItemTagId))
makeItemEntrySlice (tile, portions) = do slices <- mapM addPortionToPile portions
                                         return (tile, slices)

splitAndMergeTileTree :: RasterParams token
                      -> BuffersInCommon
                      -> TileTree (Tile, S.Seq ItemEntry)
                      -> a
                      -> CL (S.Seq (PointQueryId, SubstanceTagId))
splitAndMergeTileTree params
                      buffersInCommon
                      tree
                      output =
  do -- Start by dividing all of the items into sub seqeunces that can definitely be generated given the memory restraints of the generation kernel.
     let dividedTileTree = tr "dividedTileTree" $ runIdentity (traverseTileTree (divideEntrySequences params) tree)
     (sliceTree, itemPile) <- runStateT (traverseTileTree makeItemEntrySlice dividedTileTree) =<< (liftIO newPile)
     -- Allocate space for each generation kernel.
     let (blockTree, (tiles, blocks, totalBlocks)) = runState (traverseTileTree allocateGeneration sliceTree) (S.empty, S.empty, 0)
     thresholdBuffers <- createThresholdBuffers params (unBlockId totalBlocks)
     -- Generate all of the thresholds from the items
     runGenerateThresholdsKernel params thresholdBuffers buffersInCommon tiles blocks
     {-
     -- Given the size of each generated threshold queue continuously split tiles until the can all the divided sequences can be properly merged
     splitTileTree <- splitTileLoop runSplitTileKernel maxSplitJobSize runCheckSplitKernel maxCheckSplitJobSize allocatedTileTree

     -- Now extract the tiles that need to be merged.
     let (mergedTileTree, tilesToMerge) = runState (mapM collectTilesToMerge splitTileTree) (S.empty,0)
     -- Execute each merge job.
     runMergeTileKernel params thresholdBuffers bufferInCommon tilesToMerge

     let tileSequence = runState (traverseTileTree (\tile -> modify (|> tile)) mergedTileTree) S.empty

     runSortTileKernal params thresholdBuffers bufferInCommon tileSequence

     runRenderTileKernel params thresholdBuffers bufferInCommon tileSequence

     let queryDataBlockPairs = map (locatePointInTileTree mergedTileTree) queries
     let queryJobs = S.chunksOf maxQueryJobSize queryDataBlockPairs
     queryResults <- join <$> mapM_ runQueryKernel queryJobs
     releaseThresholdBuffers thresholdBuffers
     putStrLn ("rasterKernels Done             XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
     return queryResults
     -}
     return S.empty

makeTokenQuery :: M.Map SubstanceTagId token
               -> (PointQueryId, SubstanceTagId)
               -> (PointQueryId, Maybe token)
makeTokenQuery mapping (queryId, substanceId) =
  (queryId,
  if substanceId == noSubstanceTagId
  then Nothing
  else Just $ (M.!) mapping substanceId)

-- | Rasterize a rasterJob inside the CLMonad
runRaster :: Show token
          => RasterParams token
          -> IO (S.Seq (PointQueryResult token))
runRaster params =
    do  let tileTree = params ^. rpGeometryState . geoTileTree
        -- Get the OpenCL state from the Library structure.
        let state = params ^. rpRasterizer . rasterClState
        -- total number of 32 bit words in the output buffer.
        -- liftIO $ outputGeometryState (params ^. rpGeometryState)
        -- liftIO $ outputSubstanceState(params ^. rpSubstanceState)
        runCL state $
            do buffersInCommon <- createBuffersInCommon params (clContext state)
               -- | Create the buffers in common, which are the read only buffers that the rasterization kernel will use
               -- to generate thresholds and render images
               queryResults <- case params ^. rpFrameSpec . specDrawTarget . targetBuffer of
                                   HostBitmapTarget outputPtr ->
                                       let outputSize   = fromIntegral $ pointArea (params ^. rpFrameSpec . specBitmapSize)
                                       in  -- In this case the resulting bitmap will be stored in memory at outputPtr.
                                           splitAndMergeTileTree params buffersInCommon tileTree (OutPtr outputPtr outputSize)
                                   GLTextureTarget textureName ->
                                       -- In this case an identifier for a Texture object that stays on the GPU would be stored∘
                                       -- But currently this isn't working, so throw an error.
                                       error "GLTextureTarget not implemented"
               return $ fmap (makeTokenQuery (params ^. rpSubstanceState . suTokenMap)) queryResults

instance Storable BlockId where
    sizeOf (BlockId a) = sizeOf a
    alignment (BlockId a) = alignment a
    peek i = BlockId <$> peek (castPtr i)
    poke i (BlockId a) = poke (castPtr i) a

instance StorableM GeneratorBlock where
  sizeOfM _ = do sizeOfM (undefined :: Slice ItemTagId)
  alignmentM _ = do alignmentM (undefined :: Slice ItemTagId)
  peekM = do slice   <- peekM
             return (GeneratorBlock slice)
  pokeM (GeneratorBlock slice) =
          do pokeM slice

instance Storable GeneratorBlock where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance StorableM SplitBlock where
  sizeOfM _ = do sizeOfM (undefined :: BlockId)
                 sizeOfM (undefined :: BlockId)
  alignmentM _ = do alignmentM (undefined :: BlockId)
                    alignmentM (undefined :: BlockId)
  peekM = do blockIdA <- peekM
             blockIdB <- peekM
             return (SplitBlock blockIdA blockIdB)
  pokeM (SplitBlock blockIdA blockIdB) =
          do pokeM blockIdA
             pokeM blockIdB

instance Storable SplitBlock where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance StorableM MergeBlock where
  sizeOfM _ = do sizeOfM (undefined :: BlockId)
                 sizeOfM (undefined :: BlockId)

  alignmentM _ = do alignmentM (undefined :: BlockId)
                    alignmentM (undefined :: BlockId)
  peekM = do blockIdA <- peekM
             blockIdB <- peekM
             return (MergeBlock blockIdA blockIdB)
  pokeM (MergeBlock blockIdA blockIdB) =
          do pokeM blockIdA
             pokeM blockIdB

instance Storable MergeBlock where
  sizeOf    = sizeOfV
  alignment = alignmentV
  peek      = peekV
  poke      = pokeV
