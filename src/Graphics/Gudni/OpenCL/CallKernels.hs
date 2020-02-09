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
import qualified Data.Sequence ((<|),(|>))
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

toInt :: CInt -> Int
toInt = fromIntegral

-- A word about terms used below
-- A Block is trio fixed size blocks of memory used to store lists or thresholds, headers (threshold metadata), and queueSlices (information about the list of thresholds)
--   for each thread of a given kernel call.
-- A Tile is just the description of a given area of the screen. A tile can have multiple blocks but each block only has one tile.
-- A Job is a group of blocks, after it's been broken into chunks that an individual kernel call can handle (without timing out).

{- It's important to understand that in the beggining of the process one or more blocks are associated with each tile in the output image.
   The first stage of the process is to generate all of the thresholds contained by each block. We then determine if there is enough room to
   Merge every generated block into a single block for each tile. If it is we do that, if it's not we vertically split the tile in half and
   split each associated block in half by redistributing every threshold to the appropriate tiles.
   So for example if a tile has three blocks that are too large to be merged, we will split that into 2 tiles and 6 blocks (3 for each new tile).
   We split tiles continually until all of their blocks can be merged, or the height of the tile is one pixel. In the latter, extreme case when the tile is merged
   the bottom most threshold information will be ignored, the assumption being that information that is below MAXLAYERS of other color information in one pixel, can safely be ignored.
-}

divideIntoJobs :: Int -> S.Seq a -> S.Seq (Int, Int)
divideIntoJobs maxJobSize workItems =
  let jobSizes   = fmap S.length . S.chunksOf maxJobSize $ workItems
      jobOffsets = S.scanl (+) 0 jobSizes
  in  S.zip jobSizes jobOffsets

runGenerateThresholdsKernel :: RasterParams token
                            -> ThresholdBuffers
                            -> BuffersInCommon
                            -> Pile ItemTagId
                            -> S.Seq Tile
                            -> S.Seq GeneratorBlock
                            -> CL ()
runGenerateThresholdsKernel params thresholdBuffers buffersInCommon itemTagIdPile tiles itemSlices =
  let context         = clContext (params ^. rpRasterizer . rasterClState)
      threadsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerBlock
      maxJobSize      = params ^. rpRasterizer . rasterDeviceSpec . specMaxGenerateJobSize
      generatorJobs   = divideIntoJobs maxJobSize tiles
  in
  do
  tileBuffer      <- liftIO . vectorToBuffer context . fromSequence $ trWith (show . S.length) "tiles" tiles
  itemSliceBuffer <- liftIO . vectorToBuffer context . fromSequence $ trWith (show . S.length) "itemSlices" itemSlices
  S.traverseWithIndex (
         \jobIndex (jobSize, jobOffset) ->
         do liftIO $ putStrLn ("rasterGenerateThresholdsKernel " ++ show jobIndex ++ " XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
            runKernel (params ^. rpRasterizer . rasterGenerateThresholdsKernel)
                      -- constant data buffers
                      (buffersInCommon ^. bicGeoBuffer      )
                      (buffersInCommon ^. bicGeoRefBuffer   )
                      (buffersInCommon ^. bicPictFacets     )
                      (buffersInCommon ^. bicItemTagBuffer  )
                      itemTagIdPile
                      (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                      (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                      (cInt  $  params ^. rpFrameSpec . specFrameCount)
                      -- input job buffer
                      tileBuffer
                      itemSliceBuffer
                      (cInt jobIndex)
                      (cInt jobOffset)
                      -- return buffers
                      (thresholdBuffers ^. tbThresholdBuffer          )
                      (thresholdBuffers ^. tbHeaderBuffer             )
                      (thresholdBuffers ^. tbQueueSliceBuffer)
                      (Work2D jobSize threadsPerBlock)
                      (WorkGroup [1, threadsPerBlock]) :: CL ()
          ) generatorJobs
  liftIO . clReleaseMemObject . bufferObject $ tileBuffer
  liftIO . clReleaseMemObject . bufferObject $ itemSliceBuffer
  return ()

runCheckSplitKernel :: RasterParams token
                    -> ThresholdBuffers
                    -> S.Seq Tile
                    -> S.Seq (Slice BlockId)
                    -> S.Seq BlockId
                    -> CL (S.Seq Int)
runCheckSplitKernel params thresholdBuffers tiles checkBlockSlices checkBlocks =
    let context        = clContext (params ^. rpRasterizer . rasterClState)
        threadsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerBlock
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
                         (thresholdBuffers ^. tbQueueSliceBuffer)
                         tileBuffer
                         checkBlockSliceBuffer
                         checkBlockBuffer
                         (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                         (cInt <$> params ^. rpFrameSpec  . specBitmapSize)
                         (cInt  $  params ^. rpFrameSpec  . specFrameCount)
                         (cInt jobIndex)
                         (cInt jobOffset)
                         maximumsBuffer
                         (Work2D jobSize threadsPerBlock)
                         (WorkGroup [1, threadsPerBlock]) :: CL ()
           ) checkJobs
    maximumThresholds <- fmap toInt . S.fromList . VS.toList <$> readBuffer maximumsBuffer
    liftIO . clReleaseMemObject . bufferObject $ tileBuffer
    liftIO . clReleaseMemObject . bufferObject $ checkBlockSliceBuffer
    liftIO . clReleaseMemObject . bufferObject $ maximumsBuffer
    return maximumThresholds

runSplitKernel :: RasterParams token
               -> ThresholdBuffers
               -> S.Seq Tile
               -> S.Seq BlockId
               -> CL ()
runSplitKernel params thresholdBuffers tiles splitPairs =
    let context        = clContext (params ^. rpRasterizer . rasterClState)
        threadsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerBlock
        maxJobSize     = params ^. rpRasterizer . rasterDeviceSpec . specMaxSplitJobSize
        splitJobs      = divideIntoJobs maxJobSize splitPairs
    in
    do
    tileBuffer      <- liftIO . vectorToBuffer context . fromSequence $ tiles
    splitPairBuffer <- liftIO . vectorToBuffer context . fromSequence $ splitPairs
    S.traverseWithIndex (
           \jobIndex (jobSize, jobOffset)->
           do liftIO $ putStrLn ("splitTileKernel " ++ show jobIndex ++ "         XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
              runKernel (params ^. rpRasterizer . rasterSplitTileKernel)
                        (thresholdBuffers ^. tbThresholdBuffer )
                        (thresholdBuffers ^. tbHeaderBuffer    )
                        (thresholdBuffers ^. tbQueueSliceBuffer)
                        (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                        (cInt <$> params ^. rpFrameSpec  . specBitmapSize)
                        (cInt  $  params ^. rpFrameSpec  . specFrameCount)
                        tileBuffer
                        splitPairBuffer
                        (cInt jobIndex)
                        (cInt jobOffset)
                        (Work2D jobSize threadsPerBlock)
                        (WorkGroup [1, threadsPerBlock]) :: CL ()
           ) splitJobs
    liftIO . clReleaseMemObject . bufferObject $ tileBuffer
    liftIO . clReleaseMemObject . bufferObject $ splitPairBuffer
    return ()

data MergeBlock = MergeBlock BlockId BlockId

runMergeKernel :: RasterParams token
               -> ThresholdBuffers
               -> S.Seq Tile
               -> S.Seq (Slice BlockId)
               -> S.Seq BlockId
               -> CL ()
runMergeKernel params thresholdBuffers tiles mergeBlockSlices mergeBlocks =
  let context        = clContext (params ^. rpRasterizer . rasterClState)
      threadsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerBlock
      maxJobSize     = params ^. rpRasterizer . rasterDeviceSpec . specMaxMergeJobSize
      mergeJobs      = divideIntoJobs maxJobSize mergeBlocks
  in
  do  tileBuffer            <- liftIO . vectorToBuffer context . fromSequence $ tiles
      mergeBlockSliceBuffer <- liftIO . vectorToBuffer context . fromSequence $ mergeBlockSlices
      mergeBlockBuffer      <- liftIO . vectorToBuffer context . fromSequence $ mergeBlocks
      S.traverseWithIndex (
             \jobIndex (jobSize, jobOffset) ->
             do liftIO $ putStrLn ("mergeTileKernel " ++ show jobIndex ++ "               XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
                runKernel (params ^. rpRasterizer . rasterMergeTileKernel)
                          (thresholdBuffers ^. tbThresholdBuffer )
                          (thresholdBuffers ^. tbHeaderBuffer    )
                          (thresholdBuffers ^. tbQueueSliceBuffer)
                          tileBuffer
                          mergeBlockSliceBuffer
                          mergeBlockBuffer
                          (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                          (cInt <$> params ^. rpFrameSpec . specBitmapSize)
                          (cInt  $  params ^. rpFrameSpec . specFrameCount)
                          (cInt jobIndex)
                          (cInt jobOffset)
                          (Work2D jobSize threadsPerBlock)
                          (WorkGroup [1, threadsPerBlock]) :: CL ()
             ) mergeJobs
      liftIO . clReleaseMemObject . bufferObject $ tileBuffer
      liftIO . clReleaseMemObject . bufferObject $ mergeBlockSliceBuffer
      liftIO . clReleaseMemObject . bufferObject $ mergeBlockBuffer
      return ()

runSortKernel :: RasterParams token
              -> ThresholdBuffers
              -> S.Seq Tile
              -> S.Seq BlockId
              -> CL ()
runSortKernel params thresholdBuffers tiles sortBlocks =
    let context        = clContext (params ^. rpRasterizer . rasterClState)
        threadsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerBlock
        maxJobSize     = params ^. rpRasterizer . rasterDeviceSpec . specMaxSortJobSize
        sortJobs       = divideIntoJobs maxJobSize sortBlocks
    in
    do  tileBuffer      <- liftIO . vectorToBuffer context . fromSequence $ tiles
        sortBlockBuffer <- liftIO . vectorToBuffer context . fromSequence $ sortBlocks
        S.traverseWithIndex (
               \jobIndex (jobSize, jobOffset) ->
               do liftIO $ putStrLn ("sortThresholdsKernel " ++ show jobIndex ++ "          XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
                  runKernel (params ^. rpRasterizer . rasterSortThresholdsKernel)
                            (thresholdBuffers ^. tbThresholdBuffer )
                            (thresholdBuffers ^. tbHeaderBuffer    )
                            (thresholdBuffers ^. tbQueueSliceBuffer)
                            tileBuffer
                            sortBlockBuffer
                            (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                            (cInt <$> params ^. rpFrameSpec  . specBitmapSize)
                            (cInt  $  params ^. rpFrameSpec  . specFrameCount)
                            (cInt jobIndex)
                            (cInt jobOffset)
                            (Work2D jobSize threadsPerBlock)
                            (WorkGroup [1, threadsPerBlock]) :: CL ()
               ) sortJobs
        liftIO . clReleaseMemObject . bufferObject $ tileBuffer
        liftIO . clReleaseMemObject . bufferObject $ sortBlockBuffer
        return ()

runRenderKernel :: (  KernelArgs
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
runRenderKernel params thresholdBuffers buffersInCommon tiles renderBlocks target =
    let context        = clContext (params ^. rpRasterizer . rasterClState)
        threadsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerBlock
        maxJobSize     = params ^. rpRasterizer . rasterDeviceSpec . specMaxRenderJobSize
        renderJobs     = divideIntoJobs maxJobSize renderBlocks
    in
    do  tileBuffer        <- liftIO . vectorToBuffer context . fromSequence $ tiles
        renderBlockBuffer <- liftIO . vectorToBuffer context . fromSequence $ renderBlocks
        S.traverseWithIndex (
               \jobIndex (jobSize, jobOffset) ->
               do liftIO $ putStrLn ("rasterRenderThresholdsKernel " ++ show jobIndex ++ "   XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
                  runKernel (params ^. rpRasterizer . rasterRenderThresholdsKernel)
                            (thresholdBuffers ^. tbThresholdBuffer )
                            (thresholdBuffers ^. tbHeaderBuffer    )
                            (thresholdBuffers ^. tbQueueSliceBuffer)
                            (buffersInCommon ^. bicItemTagBuffer)
                            (buffersInCommon ^. bicSubTagBuffer )
                            (buffersInCommon ^. bicPictFacets   )
                            (buffersInCommon ^. bicPictBuffer   ) -- (params ^. rpPictData)
                            (buffersInCommon ^. bicPictMemBuffer)
                            (buffersInCommon ^. bicSolidColors  )
                            (buffersInCommon ^. bicRandoms      ) -- (params ^. rpGeometryState  . geoRandomField)
                            (params ^. rpSubstanceState . suBackgroundColor)
                            (cInt  $  params ^. rpRasterizer . rasterDeviceSpec . specComputeDepth)
                            (cInt <$> params ^. rpFrameSpec  . specBitmapSize)
                            (cInt  $  params ^. rpFrameSpec  . specFrameCount)
                            tileBuffer
                            renderBlockBuffer
                            (cInt jobIndex)
                            (cInt jobOffset)
                            target
                            (Work2D jobSize threadsPerBlock)
                            (WorkGroup [1, threadsPerBlock]) :: CL ()
              ) renderJobs
        liftIO . clReleaseMemObject . bufferObject $ tileBuffer
        liftIO . clReleaseMemObject . bufferObject $ renderBlockBuffer
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
       --                                (Work2D numTiles (fromIntegral threadsPerBlock))
       --                                (WorkGroup [1, fromIntegral threadsPerBlock])
       let queryResults = []
       return queryResults


-- | Divide entrySequence based on number of items and total number of strands
divideEntrySequences :: RasterParams token
                     -> (Tile, S.Seq ItemEntry)
                     -> Identity (Tile, (S.Seq (S.Seq ItemEntry)))
divideEntrySequences params (tile, rep) = return $ (tile, go rep)
  where
  go ss = let len = tr "len" $ S.length ss
              maxLayers     = params ^. rpRasterizer . rasterDeviceSpec . specMaxLayers
              maxThresholds = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
              totalStrands  = sum (fmap (unNumStrands . view itemStrandCount) ss) * 2
              tooManyShapes  = len > maxLayers
              tooManyStrands = totalStrands > fromIntegral maxThresholds
          in
          if (len > 1) && (tooManyShapes || tooManyStrands)
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

mkSlice start len = Slice (Ref (fromIntegral start)) (Breadth (fromIntegral len))

collectTileBlocks :: S.Seq (Tile, S.Seq BlockId) -> State (S.Seq Tile, S.Seq (Slice BlockId), S.Seq BlockId, Int) ()
collectTileBlocks = mapM_ go
    where go (newTile, newBlocks) =
             do (tiles, blockSlices, blocks, current) <- get
                let len = S.length newBlocks
                put (tiles |> newTile, blockSlices |> mkSlice current len, blocks <|> newBlocks, current + len)

mergeTileBlocks :: S.Seq (Tile, S.Seq BlockId) -> State (S.Seq Tile, S.Seq (Slice BlockId), S.Seq BlockId, Int) (S.Seq (Tile, BlockId))
mergeTileBlocks ss = mapM go ss
    where go :: (Tile, S.Seq BlockId) -> State (S.Seq Tile, S.Seq (Slice BlockId), S.Seq BlockId, Int) (Tile, BlockId)
          go (newTile, newBlocks) =
             do (tiles, blockSlices, blocks, current) <- get
                let len = S.length newBlocks
                put (tiles |> newTile, blockSlices |> mkSlice current len, blocks <|> newBlocks, current + len)
                case S.viewl newBlocks of
                  S.EmptyL -> error "empty blockId sequence."
                  (S.:<) topBlockId _ -> return (newTile, topBlockId)

verticalSplitTile :: Tile -> (Tile, Tile)
verticalSplitTile tile =
  let cut = (tile ^. tileBox . topSide + tile ^. tileBox . bottomSide) `div` 2
      topTile    = Tile (set bottomSide cut (tile ^. tileBox)) (tile ^. tileHDepth) (tile ^. tileVDepth - 1)
      bottomTile = Tile (set topSide    cut (tile ^. tileBox)) (tile ^. tileHDepth) (tile ^. tileVDepth - 1)
  in  (topTile, bottomTile)

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
              -> ThresholdBuffers
              -> TileTree (S.Seq (Tile, S.Seq BlockId))
              -> BlockId
              -> BlockId
              -> CL (TileTree (S.Seq (Tile, S.Seq BlockId)))
splitTileLoop params thresholdBuffers tileTree allocation availableAllocation =
    -- Collect all of the tiles that need to be split.
    do let (checkTiles, checkBlockSlices, checkBlocks, _) = execState (traverseTileTree collectTileBlocks tileTree) (S.empty, S.empty, S.empty, 0)
       maximums <- tr "maximums" <$> runCheckSplitKernel params thresholdBuffers checkTiles checkBlockSlices checkBlocks
       let shouldSplits = S.zipWith (shouldSplitTile params) checkTiles maximums
       if or shouldSplits
       then do let (splitTileTree, (SplitState _ splitTiles splitBlocks newAllocation)) = runState (traverseTileTree makeSplits tileTree) (SplitState shouldSplits S.empty S.empty allocation)
               adjustedThresholdBuffers <- if newAllocation <= availableAllocation
                                           then return thresholdBuffers
                                           else error "out of blocks"
               runSplitKernel params adjustedThresholdBuffers splitTiles splitBlocks
               splitTileLoop params thresholdBuffers splitTileTree newAllocation availableAllocation
       else return tileTree

addPortionToPile :: S.Seq ItemEntry -> StateT (Pile ItemTagId) CL (Slice ItemTagId)
addPortionToPile portion =
    do pile <- get
       (pile', slice) <- liftIO $ addSequenceToPile pile (fmap (view itemEntryTagId) portion)
       put pile'
       return slice

makeItemEntrySlice :: (Tile, S.Seq (S.Seq ItemEntry)) -> StateT (Pile ItemTagId) CL (Tile, S.Seq (Slice ItemTagId))
makeItemEntrySlice (tile, portions) = do slices <- mapM addPortionToPile portions
                                         return (tile, slices)

{-
generateAndMergeBlocks :: RasterParams token
                       -> ThresholdBuffers
                       -> BuffersInCommon
                       -> S.Seq BlockId
                       -> S.Seq (Tile, Slice ItemTagId)
                       -> CL (S.Seq BlockId, S.Seq (Tile, BlockId))
generateAndMergeBlocks params
                       thresholdBuffers
                       buffersInCommon
                       availableBlocks
                       tileSlices =
    do


  runGenerateThresholdsKernel :: RasterParams token
                              -> ThresholdBuffers
                              -> BuffersInCommon
                              -> Pile ItemTagId
                              -> S.Seq Tile
                              -> S.Seq GeneratorBlock
                              -> CL ()


-}
splitAndMergeTileTree :: (  KernelArgs
                           'KernelSync
                           'NoWorkGroups
                           'UnknownWorkItems
                           'Z
                           (target -> NumWorkItems -> WorkGroup -> CL ())
                          )
                      => RasterParams token
                      -> BuffersInCommon
                      -> TileTree (Tile, S.Seq ItemEntry)
                      -> target
                      -> CL (S.Seq (PointQueryId, SubstanceTagId))
splitAndMergeTileTree params
                      buffersInCommon
                      tree
                      target =
  do -- Start by dividing all of the items into sub seqeunces that can definitely be generated given the memory restraints of the generation kernel.
     let dividedTileTree = tr "dividedTileTree" $ runIdentity (traverseTileTree (divideEntrySequences params) tree)
     (sliceTree, itemTagIdPile) <- runStateT (traverseTileTree makeItemEntrySlice dividedTileTree) =<< (liftIO newPile)
     -- Allocate space for each generation kernel.
     let (blockTree, (tiles, blocks, totalBlocks)) = runState (traverseTileTree allocateGeneration sliceTree) (S.empty, S.empty, 0)
         availableBlocks = params ^. rpRasterizer . rasterDeviceSpec . specAvailableBlocks
         --tileChunks  = S.chunksOf (availableBlocks `div` 4) tiles
         --blockChunks = S.chunksOf (availableBlocks `div` 4) blocks
         --jobs = S.zipWith tileChunks blockChunks
     thresholdBuffers <- createThresholdBuffers params availableBlocks
     -- Generate all of the thresholds from the items

     runGenerateThresholdsKernel params thresholdBuffers buffersInCommon itemTagIdPile tiles blocks
     -- Given the size of each generated threshold queue continuously split tiles until the can all the divided sequences can be properly merged
     splitTileTree <- splitTileLoop params thresholdBuffers blockTree totalBlocks (BlockId availableBlocks)
     -- Now extract the tiles that need to be merged.
     let (mergedTileTree, (mergeTiles, mergeBlockSlices, mergeBlocks, _)) = runState (traverseTileTree mergeTileBlocks splitTileTree) (S.empty, S.empty, S.empty, 0)
      -- Execute each merge job.
     runMergeKernel params thresholdBuffers mergeTiles mergeBlockSlices mergeBlocks

     let (finalTiles, finalBlocks) = execState (traverseTileTree (mapM (\(tile, blockId) -> modify (\(tiles, blocks) -> (tiles |> tile, blocks |> blockId)))) mergedTileTree) (S.empty, S.empty)

     runSortKernel params thresholdBuffers finalTiles finalBlocks

     runRenderKernel params thresholdBuffers buffersInCommon finalTiles finalBlocks target
     {-
     queryResults <- runPointQueryKernel params thresholdBuffers buffersInCommon tiles (params ^. rasterQueries)
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

instance StorableM MergeBlock where
  sizeOfM    _ = do sizeOfM (undefined :: BlockId)
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
