{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Graphics.Gudni.OpenCL.CallKernels
  (
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

import CLUtil
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

fromSequence :: Storable a => S.Seq a -> VS.Vector a
fromSequence = VS.fromList . toList

cInt :: Int -> CInt
cInt = fromIntegral

runGenerateThresholdsKernel :: RasterParams token
                            -> ThresholdBuffers
                            -> BuffersInCommon
                            -> S.Seq (Tile, (DataBlockId, Slice ItemEntry))
                            -> CL ()
runGenerateThresholdsKernel params thresholdBuffers buffersInCommon generateSequence =
  let threadsPerTile = cInt $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
      maxJobSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxGenerateJobSize
      jobs = S.chunksOf maxJobSize generateSequence
  in
  do
  S.traverseWithIndex (
         \jobIndex job ->
         do liftIO $ putStrLn ("rasterGenerateThresholdsKernel " ++ show jobIndex ++ " XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
            let (tiles,blocks) = S.unzip job
            runKernel (params ^. rpRasterizer . rasterGenerateThresholdsKernel)
                      -- input job buffer
                      (fromSequence tiles)
                      (fromSequence blocks)
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
                      (Work2D (S.length job) (fromIntegral threadsPerTile))
                      (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
          ) jobs
  return ()

runCheckSplitKernel :: RasterParams token
                    -> ThresholdBuffers
                    -> S.Seq DataBlockId
                    -> CL ()
runCheckSplitKernel params thresholdBuffers mergeTiles =
    let threadsPerTile = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxCheckSplitJobSize
        jobs = S.chunksOf maxJobSize mergeTiles
    in
    do
    S.traverseWithIndex (
           \jobIndex job ->
           do liftIO $ putStrLn ("sortThresholdsKernel " ++ show jobIndex ++ "           XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterCheckSplitKernel)
                        (thresholdBuffers ^. tbThresholdBuffer          )
                        (thresholdBuffers ^. tbHeaderBuffer             )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence job)
                        (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                        (cInt  $  params ^. rpFrameSpec . specFrameCount)
                        (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt jobIndex)
                        (Work2D (S.length job) (threadsPerTile))
                        (WorkGroup [1, threadsPerTile]) :: CL ()
           ) jobs
    return ()

runSplitTileKernel :: RasterParams token
                   -> ThresholdBuffers
                   -> S.Seq (DataBlockId, DataBlockId)
                   -> CL ()
runSplitTileKernel params thresholdBuffers splitTiles =
    let threadsPerTile = fromIntegral $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize     =                params ^. rpRasterizer . rasterDeviceSpec . specMaxSortJobSize
        jobs = S.chunksOf maxJobSize splitTiles
    in
    do
    S.traverseWithIndex (
           \jobIndex job ->
           do liftIO $ putStrLn ("rasterSplitTileKernel " ++ show jobIndex ++ "         XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterSplitTileKernel)
                        (thresholdBuffers ^. tbThresholdBuffer )
                        (thresholdBuffers ^. tbHeaderBuffer    )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence job)
                        (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                        (cInt  $  params ^. rpFrameSpec  . specFrameCount)
                        (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt jobIndex)
                        (Work2D (S.length job) (fromIntegral threadsPerTile))
                        (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
           ) jobs
    return ()

runMergeTileKernel :: RasterParams token
                   -> ThresholdBuffers
                   -> S.Seq (DataBlockId, DataBlockId)
                   -> CL ()
runMergeTileKernel params thresholdBuffers mergeTiles =
    let threadsPerTile = fromIntegral $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize     =                params ^. rpRasterizer . rasterDeviceSpec . specMaxMergeJobSize
        jobs = S.chunksOf maxJobSize mergeTiles
    in
    do
    S.traverseWithIndex (
           \jobIndex job ->
           do liftIO $ putStrLn ("mergeTileKernel " ++ show jobIndex ++ "               XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterMergeTileKernel)
                        (thresholdBuffers ^. tbThresholdBuffer )
                        (thresholdBuffers ^. tbHeaderBuffer    )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence job)
                        (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                        (cInt  $  params ^. rpFrameSpec . specFrameCount)
                        (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt jobIndex)
                        (Work2D (S.length job) (fromIntegral threadsPerTile))
                        (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
           ) jobs
    return ()


runSortTileKernel :: RasterParams token
                  -> ThresholdBuffers
                  -> S.Seq DataBlockId
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

runRenderTileKernel :: RasterParams token
                    -> ThresholdBuffers
                    -> BuffersInCommon
                    -> target
                    -> S.Seq DataBlockId
                    -> CL ()
runRenderTileKernel params thresholdBuffers buffersInCommon target renderTiles =
    let threadsPerTile = fromIntegral $ params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerTile
        maxJobSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxRenderJobSize
        jobs = S.chunksOf maxJobSize renderTiles
    in
    S.foldMapWithIndex (
           \jobIndex job ->
           do liftIO $ putStrLn ("rasterRenderThresholdsKernel " ++ show jobIndex ++ "   XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterRenderThresholdsKernel)
                        (thresholdBuffers ^. tbThresholdBuffer )
                        (thresholdBuffers ^. tbHeaderBuffer    )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (fromSequence job)
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
                        (Work2D (S.length job) (fromIntegral threadsPerTile))
                        (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
          ) jobs

runPointQueryKernel :: RasterParams token
                    -> ThresholdBuffers
                    -> BuffersInCommon
                    -> S.Seq DataBlockId
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
                     -> Tile
                     -> (S.Seq ItemEntry)
                     -> Identity (Tile, (S.Seq (S.Seq ItemEntry)))
divideEntrySequences params tile rep = return $ (tile, go rep)
  where
  go ss = let len = S.length ss
              maxLayers     = params ^. rpRasterizer . rasterDeviceSpec . specMaxLayers
              maxThresholds = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
          in
          if (len > 1) && (len > maxLayers || (sum (fmap (unNumStrands . view itemStrandCount) ss) * 3) > maxThresholds)
          then let (left, right) = S.splitAt (len `div` 2) ss
               in go left <|> go right
          else S.singleton ss

newtype DataBlockId = DataBlockId {unDataBlockId :: Int} deriving (Eq, Ord, Show, Num)

allocateTilePortion :: S.Seq ItemEntry
                    -> State (S.Seq (DataBlockId, S.Seq ItemEntry), DataBlockId) DataBlockId
allocateTilePortion portion =
   do (generateSequence, allocation) <- get
      put (generateSequence |> (allocation, portion), allocation + 1)
      return allocation

allocateTiles :: Tile
              -> S.Seq (S.Seq ItemEntry)
              -> State (S.Seq (DataBlockId, S.Seq ItemEntry), DataBlockId) (Tile, S.Seq DataBlockId)
allocateTiles tile rep =
   do let portions = rep
      portionAllocations <- mapM (allocateTilePortion) portions
      return (tile, portionAllocations)

--mustSplitTile :: S.Seq (Tile (S.Seq (S.Seq ItemEntry))) -> State (S.S.Seq (Tile (S.S
mustSplitTile = undefined
  -- if heightOf tile > 1 && sums of columns of thresholds cannot be split.
  -- if the height is <= 1 then we can just take the top thresholds

splitTileLoop :: RasterParams token
              -> S.Seq (Tile, S.Seq DataBlockId)
              -> DataBlockId
              -> CL (S.Seq (Tile, S.Seq DataBlockId), DataBlockId)
splitTileLoop params tileSequence allocation =
    -- Collect all of the tiles that need to be split.
    let (splitTileSequence,(splitPairs, newAllocation)) = runState (mapM mustSplitTile tileSequence) (S.empty, allocation)
    in
    if length splitPairs > 0
    then -- Check allocation newAllocations and reallocate buffers if necessary
      do (dataBlocks, queueBlocks) <- if (newAllocaion > allocation || undefined {- available allocation-})
                                      then undefined
                                        -- make new buffers
                                        -- copy old buffers to new
                                        -- get rid of old buffers
                                      else undefined
                                        -- use current buffers
         -- Divide all of the split tiles into chunks we know can execute in one kernel.
         let maxSplitJobSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxSplitJobSize
             splitJobs       = S.chunksOf maxSplitJobSize splitPairs
         -- Execute all of the split jobs.
         mapM_ runSplitTileKernel splitJobs
         --
         splitTileLoop splitTileSequence newAllocation
    else (tileSequence, allocation)

splitAndMergeTileTree :: RasterParams token
                      -> BuffersInCommon
                      -> TileTree (S.Seq ItemEntry)
                      -> a
                      -> CL (S.Seq (PointQueryResult SubstanceTagId))
splitAndMergeTileTree params
                      buffersInCommon
                      tree
                      output =
  do -- Start by dividing all of the items into sub seqeunces that can definitely be generated given the memory restraints of the generation kernel.
     let dividedTileTree = tr "dividedTileTree" $ runIdentity (traverseTileTree (divideEntrySequences params) tree)
         -- Allocate space for each generation kernel.
         (allocatedTileTree, (generateTiles, generateSlices, totalBlocks)) = runState (traverseTileTree allocateTiles dividedSequenceTree) (S.empty,0)
     thresholdBuffers <- createThresholdBuffers params totalBlocks
     -- Generate all of the thresholds from the items
     runGenerateThresholdsKernel params thresholdBuffers buffersInCommon generateSequence
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
     return ()

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
          -> TileTree (S.Seq ItemEntry)
          -> IO (S.Seq (PointQueryResult token))
runRaster params tileTree =
    do  -- Get the OpenCL state from the Library structure.
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

instance Storable DataBlockId where
    sizeOf (DataBlockId a) = sizeOf a
    alignment (DataBlockId a) = alignment a
    peek i = DataBlockId <$> peek (castPtr i)
    poke i (DataBlockId a) = poke (castPtr i) a
