{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

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
import Graphics.Gudni.Util.CTypeConversion
import Graphics.Gudni.Util.Util

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
import Data.Bits
import Data.Maybe
import Control.Lens.Indexed
import Linear.V4


import Control.Monad.Identity
import Control.Monad.State
import Control.Lens
import Control.Applicative
import Control.Loop

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

fromSequence :: Storable a => S.Seq a -> VS.Vector a
fromSequence = VS.fromList . toList

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

announceKernel name jobStep jobOffset jobSize =
     liftIO $ putStrLn (name ++ " step:" ++ show jobStep ++ " offset:" ++ show jobOffset ++ " size:" ++ show jobSize ++ "    XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");

runGenerateThresholdsKernel :: RasterParams token
                            -> BuffersInCommon
                            -> Pile ItemTagId
                            -> S.Seq (Tile, Slice ItemTagId)
                            -> CL BlockSection
runGenerateThresholdsKernel params buffersInCommon itemTagIdPile tileSlices =
  let context         = clContext (params ^. rpRasterizer . rasterClState)
      columnsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
      maxJobSize      = params ^. rpRasterizer . rasterDeviceSpec . specGenerateJobSize
      (tiles, itemSlices) = S.unzip tileSlices
  in
  do itemSliceBuffer <- liftIO . vectorToBuffer context . fromSequence $ itemSlices
     blockSection <- createBlockSection params
     writeBuffer (blockSection ^. sectTileBuffer) . fromSequence $ tiles
     let len = S.length tiles
     iterateJobs len maxJobSize $
         \jobStep jobOffset jobSize ->
             do announceKernel "rasterGenerateThresholdsKernel" jobStep jobOffset jobSize
                dumpBufferPart ("before generate inUseBuffer " ++ show jobStep) (blockSection ^. sectInUseBuffer)
                runKernel (params ^. rpRasterizer . rasterGenerateThresholdsKernel)
                          -- constant data buffers
                          (buffersInCommon ^. bicGeoBuffer      )
                          (buffersInCommon ^. bicGeoRefBuffer   )
                          (buffersInCommon ^. bicFacetBuffer    )
                          (buffersInCommon ^. bicItemTagBuffer  )
                          itemTagIdPile
                          itemSliceBuffer
                          (toCInt  $  params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth)
                          (toCInt <$> params ^. rpFrameSpec . specBitmapSize)
                          (toCInt  $  params ^. rpFrameSpec . specFrameCount)
                          -- input job buffer
                          (toCInt jobStep    )
                          (toCInt jobOffset  )
                          (toCInt jobSize    )
                          -- return buffers
                          (blockSection ^. sectTileBuffer       )
                          (blockSection ^. sectThresholdBuffer  )
                          (blockSection ^. sectHeaderBuffer     )
                          (blockSection ^. sectQueueSliceBuffer )
                          (blockSection ^. sectBlockIdBuffer    )
                          (blockSection ^. sectInUseBuffer      )
                          (Work2D jobSize columnsPerBlock)
                          (WorkGroup [1, columnsPerBlock]) :: CL ()
                dumpBufferPart ("after generate inUseBuffer " ++ show jobStep) (blockSection ^. sectInUseBuffer)
                dumpBufferPart ("after generate blockIdBuffer " ++ show jobStep) (blockSection ^. sectBlockIdBuffer)

     return . set sectInUseLength len $ blockSection


runSplitKernel :: RasterParams token
               -> BlockSection
               -> BlockSection
               -> CL (BlockSection, BlockSection)
runSplitKernel params blockSection newBlockSection =
    let context         = clContext (params ^. rpRasterizer . rasterClState)
        columnsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
        maxJobSize      = params ^. rpRasterizer . rasterDeviceSpec . specSplitJobSize
        splitLength     = blockSection ^. sectInUseLength - blockSection ^. sectRenderLength
    in
    do
    iterateJobs splitLength maxJobSize $
           \jobStep jobOffset jobSize ->
           do announceKernel "rasterSplitTileKernel" jobStep jobOffset jobSize
              runKernel (params ^. rpRasterizer . rasterSplitTileKernel)
                        (blockSection ^. sectTileBuffer      )
                        (blockSection ^. sectThresholdBuffer )
                        (blockSection ^. sectHeaderBuffer    )
                        (blockSection ^. sectQueueSliceBuffer)
                        (blockSection ^. sectBlockIdBuffer   )
                        (0 :: CInt)
                        (newBlockSection ^. sectTileBuffer      )
                        (newBlockSection ^. sectThresholdBuffer )
                        (newBlockSection ^. sectHeaderBuffer    )
                        (newBlockSection ^. sectQueueSliceBuffer)
                        (newBlockSection ^. sectBlockIdBuffer   )
                        (0 :: CInt)
                        (toCInt  $  params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth)
                        (toCInt <$> params ^. rpFrameSpec  . specBitmapSize)
                        (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                        (toCInt jobStep)
                        (toCInt jobOffset)
                        (Work2D jobSize columnsPerBlock)
                        (WorkGroup [1, columnsPerBlock]) :: CL ()
    return (blockSection, newBlockSection)

iterateJobs :: Monad m => Int -> Int -> (Int -> Int -> Int -> m ()) -> m ()
iterateJobs total chunk f = go 0 0
  where go step offset = let size = min (total - offset) chunk
                         in  if offset < total
                             then do f step offset size
                                     go (step + 1) (offset + chunk)
                             else return ()


runMergeKernel :: RasterParams token
               -> BlockSection
               -> CL ()
runMergeKernel params blockSection =
  let context          = clContext (params ^. rpRasterizer . rasterClState)
      columnsPerBlock  = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
      columnDepth      = params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth
      blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
      maxJobSize       = params ^. rpRasterizer . rasterDeviceSpec . specMergeJobSize
      jobDepth         = tr "jobDepth" $ params ^. rpRasterizer . rasterDeviceSpec . specMergeJobDepth

  in
  do  liftIO $ putStrLn "runMergeKernel Start ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
      iterateJobs (tr "sectInUseLength" $ blockSection ^. sectInUseLength) (tr "maxMergeJobSize" maxJobSize) $
          \jobStep jobOffset jobSize ->
             do  dumpBufferPartTiles ("tiles " ++ show jobStep) (blockSection ^. sectTileBuffer)
                 dumpBufferPart ("inUseBuffer " ++ show jobStep) (blockSection ^. sectInUseBuffer)
                 forM_ [0,(-1)] $ \ strideOffset ->
                    do   forLoop 0 (< jobDepth - 1) (+ 1) $ \ strideExp ->
                            do announceKernel ("mergeAdjacent strideExp:" ++ show strideExp) jobStep jobOffset jobSize
                               runKernel (params ^. rpRasterizer . rasterMergeTileKernel)
                                         (blockSection ^. sectTileBuffer      )
                                         (blockSection ^. sectThresholdBuffer )
                                         (blockSection ^. sectHeaderBuffer    )
                                         (blockSection ^. sectQueueSliceBuffer)
                                         (blockSection ^. sectBlockIdBuffer   )
                                         (blockSection ^. sectInUseBuffer     )
                                         (toCInt jobDepth   )
                                         (toCInt columnDepth)
                                         (toCInt <$> params ^. rpFrameSpec . specBitmapSize)
                                         (toCInt  $  params ^. rpFrameSpec . specFrameCount)
                                         (toCInt jobStep      )
                                         (toCInt jobOffset    )
                                         (toCInt jobSize      )
                                         (toCInt strideExp    )
                                         (toCInt strideOffset )
                                         (Local columnsPerBlock :: LocalMem CInt)
                                         (Work2D jobSize columnsPerBlock)
                                         (WorkGroup [1, columnsPerBlock]) :: CL ()
                               dumpBufferPart ("afterMerge strideExp " ++ show strideExp ++ "inUseBuffer")   (blockSection ^. sectInUseBuffer)
                               dumpBufferPart ("afterMerge strideExp " ++ show strideExp ++ "blockIdBuffer") (blockSection ^. sectBlockIdBuffer)
      liftIO $ putStrLn "runMergeKernel Done ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"


runCollectMergedKernel :: RasterParams token
                       -> BlockSection
                       -> CL BlockSection
runCollectMergedKernel params blockSection =
    let blocksPerSection  = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
    in
    do  announceKernel "collectMergedKernel" 0 0 0
        outputInUseLength <-
            runKernel (params ^. rpRasterizer . rasterCollectMergedBlocksKernel)
                      (blockSection ^. sectTileBuffer     )
                      (blockSection ^. sectBlockIdBuffer  )
                      (blockSection ^. sectInUseBuffer    )
                      (toCInt  $  params ^. rpRasterizer . rasterDeviceSpec . specBlockSectionDepth)
                      (toCInt  $  blocksPerSection)
                      (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                      (Out 1)
                      (Local blocksPerSection :: LocalMem CInt)
                      (Work1D blocksPerSection)
                      (WorkGroup [blocksPerSection]) :: CL (VS.Vector CInt)
        let inUseLength = tr "inUnseLength" $ fromCInt . VS.head $ outputInUseLength
        dumpBufferPart ("afterCollectMerged inUseBuffer")   (blockSection ^. sectInUseBuffer)
        dumpBufferPart ("afterCollectMerged blockIdBuffer") (blockSection ^. sectBlockIdBuffer)
        return . set sectInUseLength inUseLength $ blockSection

runCollectRenderKernel :: RasterParams token
                       -> BlockSection
                       -> CL BlockSection
runCollectRenderKernel params blockSection =
    let blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
    in
    do  announceKernel "collectRenderKernel" 0 0 0
        outputRenderLength <-
            runKernel (params ^. rpRasterizer . rasterCollectRenderBlocksKernel)
                      (blockSection ^. sectTileBuffer)
                      (blockSection ^. sectBlockIdBuffer  )
                      (blockSection ^. sectInUseBuffer    )
                      (toCInt  $  blockSection ^. sectInUseLength)
                      (blockSection ^. sectRenderBuffer   )
                      (toCInt  $  params ^. rpRasterizer . rasterDeviceSpec . specBlockSectionDepth)
                      (toCInt  $  blocksPerSection)
                      (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                      (Out 1)
                      (Local blocksPerSection :: LocalMem CInt)
                      (Work1D blocksPerSection)
                      (WorkGroup [blocksPerSection]) :: CL (VS.Vector CInt)
        let renderLength = fromCInt . VS.head $ outputRenderLength
            inUseLength' = blockSection ^. sectInUseLength - renderLength
        dumpBufferPart ("afterCollectRender blockIdBuffer") (blockSection ^. sectRenderBuffer)
            -- splitLength  = fromCInt . VS.head $ outputSplitLength
        return . set sectRenderLength renderLength
               . set sectInUseLength inUseLength' $ blockSection


runSortKernel :: RasterParams token
              -> BlockSection
              -> CL ()
runSortKernel params blockSection =
    let context         = clContext (params ^. rpRasterizer . rasterClState)
        columnsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
        maxJobSize      = params ^. rpRasterizer . rasterDeviceSpec . specMaxSortJobSize
        columnDepth     = params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth
    in
    do  iterateJobs (blockSection ^. sectRenderLength) maxJobSize $
               \jobStep jobOffset jobSize ->
               do announceKernel "sortKernel" jobStep jobOffset jobSize
                  runKernel (params ^. rpRasterizer . rasterSortThresholdsKernel)
                            (blockSection ^. sectTileBuffer      )
                            (blockSection ^. sectThresholdBuffer )
                            (blockSection ^. sectHeaderBuffer    )
                            (blockSection ^. sectQueueSliceBuffer)
                            (blockSection ^. sectRenderBuffer    )
                            (toCInt columnDepth)
                            (toCInt <$> params ^. rpFrameSpec  . specBitmapSize)
                            (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                            (toCInt jobStep)
                            (toCInt jobOffset)
                            (Work2D jobSize columnsPerBlock)
                            (WorkGroup [1, columnsPerBlock]) :: CL ()
        return ()

runRenderKernel :: (  KernelArgs
                     'KernelSync
                     'NoWorkGroups
                     'UnknownWorkItems
                     'Z
                     (target -> NumWorkItems -> WorkGroup -> CL ())
                   )
                => RasterParams token
                -> BuffersInCommon
                -> BlockSection
                -> target
                -> CL ()
runRenderKernel params buffersInCommon blockSection target =
    let context         = clContext (params ^. rpRasterizer . rasterClState)
        columnsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
        maxJobSize      = params ^. rpRasterizer . rasterDeviceSpec . specMaxRenderJobSize
    in
    do
        iterateJobs (blockSection ^. sectRenderLength) maxJobSize $
               \jobStep jobOffset jobSize ->
               do liftIO $ putStrLn ("rasterRenderThresholdsKernel index:" ++ show jobStep ++ " size:" ++ show jobSize ++ "   XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
                  runKernel (params ^. rpRasterizer . rasterRenderThresholdsKernel)
                            (blockSection ^. sectTileBuffer      )
                            (blockSection ^. sectThresholdBuffer )
                            (blockSection ^. sectHeaderBuffer    )
                            (blockSection ^. sectQueueSliceBuffer)
                            (blockSection ^. sectRenderBuffer    )
                            (buffersInCommon ^. bicItemTagBuffer)
                            (buffersInCommon ^. bicSubTagBuffer )
                            (buffersInCommon ^. bicFacetBuffer   )
                            (buffersInCommon ^. bicPictBuffer   ) -- (params ^. rpPictData)
                            (buffersInCommon ^. bicPictMemBuffer)
                            (buffersInCommon ^. bicSolidColors  )
                            (buffersInCommon ^. bicRandoms      ) -- (params ^. rpGeometryState  . geoRandomField)
                            (params ^. rpSubstanceState . suBackgroundColor)
                            (toCInt  $  params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth)
                            (toCInt <$> params ^. rpFrameSpec  . specBitmapSize)
                            (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                            (toCInt jobStep)
                            (toCInt jobOffset)
                            target
                            (Work2D jobSize columnsPerBlock)
                            (WorkGroup [1, columnsPerBlock]) :: CL ()
        return ()

runPointQueryKernel :: RasterParams token
                    -> BlockSection
                    -> BuffersInCommon
                    -> S.Seq BlockId
                    -> CL [PointQueryResult SubstanceTagId]
runPointQueryKernel params blockSection buffersInCommon pointQueries =
    do
       --liftIO $ putStrLn ("rasterPointQueryKernel " ++ show jobStep ++ "         XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
       -- queryResults <- let numPointQueries = length $ tr "pointQueries" $ job ^. rJPointQueries in
       --                 if  numPointQueries <= 0
       --                 then return []
       --                 else (toList :: CLUtil.Vector SubstanceTagId -> [SubstanceTagId]) <$>
       --                      runKernel (params ^. rpRasterizer . rasterQueryKernel)
       --                                (blockSection ^. sectThresholdBuffer )
       --                                (blockSection ^. sectHeaderBuffer    )
       --                                (blockSection ^. sectQueueSliceBuffer)
       --                                (buffersInCommon  ^. bicFacetBuffer)
       --                                (job    ^. rJTilePile)
       --                                (params ^. rpFrameSpec ^. specBitmapSize)
       --                                (params ^. rpFrameSpec ^. specFrameCount)
       --                                (params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth)

       --                                jobStep
       --                                (fromIntegral numPointQueries :: CInt)
       --                                (VS.fromList queries)
       --                                (Out numPointQueries)
       --                                (Work2D numTiles (fromIntegral columnsPerBlock))
       --                                (WorkGroup [1, fromIntegral columnsPerBlock])
       let queryResults = []
       return queryResults


-- | Divide entrySequence based on number of items and total number of strands
divideEntrySequences :: RasterParams token
                     -> (Tile, S.Seq ItemEntry)
                     -> Identity (Tile, (S.Seq (S.Seq ItemEntry)))
divideEntrySequences params (tile, rep) = return $ (tile, go rep)
  where
  go ss = let len = S.length ss
              maxLayers      = params ^. rpRasterizer . rasterDeviceSpec . specMaxLayers
              maxThresholds  = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
              totalStrands   = sum (fmap (unNumStrands . view itemStrandCount) ss) * 2
              tooManyShapes  = len > maxLayers
              tooManyStrands = totalStrands > fromIntegral maxThresholds
          in
          if (len > 1) && (tooManyShapes || tooManyStrands)
          then let (left, right) = S.splitAt (len `div` 2) ss
               in go left <|> go right
          else S.singleton ss

allocateTilePortion :: Tile
                    -> Slice ItemTagId
                    -> State ( S.Seq (Tile, Slice ItemTagId) ) ()
allocateTilePortion tile slice =
  do generatorJobs <- get
     put (generatorJobs |> (tile, slice))

allocateGeneration :: (Tile, S.Seq (Slice ItemTagId))
                   -> State ( S.Seq (Tile, Slice ItemTagId) ) ()
allocateGeneration (tile, slices) =
    mapM_ (allocateTilePortion tile) slices

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
      topTile    = Tile (set bottomSide cut (tile ^. tileBox))
      bottomTile = Tile (set topSide    cut (tile ^. tileBox))
  in  (topTile, bottomTile)

dumpBufferPart message buffer =
  do liftIO . putStrLn $ "----------------------------------- " ++ message ++ " -----------------------------------"
     hs <-readBuffer buffer
     liftIO . putStrLn . show $ hs

dumpBufferPartTiles :: String -> CLBuffer Tile -> CL ()
dumpBufferPartTiles message buffer =
  do liftIO . putStrLn $ "----------------------------------- " ++ message ++ " -----------------------------------"
     hs <- VS.take 32 <$> readBuffer buffer
     liftIO . putStrLn . concat . map (lpad 5 . show . view (tileBox . leftSide  )) . VS.toList $ hs
     liftIO . putStrLn . concat . map (lpad 5 . show . view (tileBox . topSide   )) . VS.toList $ hs
     liftIO . putStrLn . concat . map (lpad 5 . show . view (tileBox . rightSide )) . VS.toList $ hs
     liftIO . putStrLn . concat . map (lpad 5 . show . view (tileBox . bottomSide)) . VS.toList $ hs

withBeforeAndAfter :: Monad m => S.Seq a ->(Maybe a -> Maybe a -> a -> m b) -> m (S.Seq b)
withBeforeAndAfter ss f = iforM ss go
   where
   len = S.length ss
   go i this =
     let before = if i > 0 then Just $ S.index ss (i-1) else Nothing
         after  = if i < len - 1 then Just $ S.index ss  (i+1)else Nothing
     in  f before after this

checkBlockSection section = if section ^. sectInUseLength == 0
                            then do releaseBlockSection section
                                    return Nothing
                            else return . Just $ section

condenseStack blockSectionStack = fmap fromJust . S.filter isJust <$> mapM checkBlockSection blockSectionStack

processBufferStack :: (  KernelArgs
                        'KernelSync
                        'NoWorkGroups
                        'UnknownWorkItems
                        'Z
                        (target -> NumWorkItems -> WorkGroup -> CL ())
                      )
                   => RasterParams token
                   -> BuffersInCommon
                   -> S.Seq BlockSection
                   -> target
                   -> CL (S.Seq BlockSection)
processBufferStack params buffersInCommon blockSectionStack target =
    do -- iforM blockSectionStack (\i buffers -> dumpBufferPart ("generated inUse part " ++ show i) (buffers ^. sectInUseBuffer))
       -- iforM blockSectionStack (\i buffers -> dumpBufferPart ("generated bufferId part " ++ show i) (buffers ^. sectBlockIdBuffer))
       liftIO $ putStrLn $ "|||||||||||||||||||||||| Merge Section Start Length " ++ show (S.length blockSectionStack) ++ "||||||||||||"
       mergedStack <- iforM blockSectionStack $
                                  \ i blockSection ->
                                       do liftIO $ putStrLn $ "|||||||||||||||||||||||| Merge Section " ++ show i ++ " |||||||||||||||||||||||| "
                                          runMergeKernel params blockSection
                                          runCollectMergedKernel params blockSection
       liftIO $ putStrLn $ "||||||||||||||||||||||||||||||||||||  Merge Section Done ||||||||||||||||||||||||"
       splitStack <- withBeforeAndAfter mergedStack $
                   \ before after blockSection ->
                      do toRenderSection <- runCollectRenderKernel params blockSection
                         --dumpBufferPart ("render length " ++ show (toRenderSection ^. sectRenderLength)) (toRenderSection ^. sectRenderBuffer)
                         --dumpBufferPart "renderCollected bufferId part " (toRenderSection ^. sectBlockIdBuffer)
                         if toRenderSection ^. sectRenderLength > 0
                         then do runSortKernel params toRenderSection
                                 runRenderKernel params buffersInCommon toRenderSection target
                         else return ()
                         if tr "**inUseLength" (toRenderSection ^. sectInUseLength) <= 0
                         then return $ S.singleton toRenderSection
                         else do newBlockSection <- createBlockSection params
                                 runSplitKernel params toRenderSection newBlockSection
                                 return $ S.singleton toRenderSection <|> S.singleton newBlockSection
       let stack = trWith (show . fmap (view sectInUseLength)) "stack lengths" $ S.filter ((>0) . view sectInUseLength) $ join splitStack
       iforM stack (\i buffers -> dumpBufferPart ("stack inuse  " ++ show i) (buffers ^. sectInUseBuffer))
       iforM stack (\i buffers -> dumpBufferPart ("stack render " ++ show i) (buffers ^. sectBlockIdBuffer))
       condensed <- condenseStack stack
       if   not (S.null condensed)
       then processBufferStack params buffersInCommon condensed target
       else {-
            queryResults <- runPointQueryKernel params blockSection buffersInCommon tiles (params ^. rasterQueries)
            releaseBlockSection blockSection
            putStrLn ("rasterKernels Done             XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
            return queryResults
            -}
            return S.empty


addPortionToPile :: S.Seq ItemEntry -> StateT (Pile ItemTagId) CL (Slice ItemTagId)
addPortionToPile portion =
    do pile <- get
       (pile', slice) <- liftIO $ addSequenceToPile pile (fmap (view itemEntryTagId) portion)
       put pile'
       return slice

makeItemEntrySlice :: (Tile, S.Seq (S.Seq ItemEntry)) -> StateT (Pile ItemTagId) CL (Tile, S.Seq (Slice ItemTagId))
makeItemEntrySlice (tile, portions) = do slices <- mapM addPortionToPile portions
                                         return (tile, slices)

splitAndMergeTileTree :: ( KernelArgs
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
     let dividedTileTree :: TileTree (Tile, S.Seq (S.Seq ItemEntry))
         dividedTileTree = runIdentity (traverseTileTree (divideEntrySequences params) tree)
     (sliceTree, itemTagIdPile) <- runStateT (traverseTileTree makeItemEntrySlice dividedTileTree) =<< (liftIO newPile)
     -- Allocate space for each generation kernel.
     let tileBlocks :: S.Seq (Tile, Slice ItemTagId)
         tileBlocks = execState (traverseTileTree allocateGeneration sliceTree) S.empty
         blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
         sections :: S.Seq (S.Seq (Tile, Slice ItemTagId))
         sections = trWith (show . S.length) "numSections"  $ S.chunksOf (tr "blocksPerSection" blocksPerSection) tileBlocks
     -- Generate all of the thresholds from the items
     blockSectionStack <- forM sections $ runGenerateThresholdsKernel params buffersInCommon itemTagIdPile
     processBufferStack params buffersInCommon blockSectionStack target
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
