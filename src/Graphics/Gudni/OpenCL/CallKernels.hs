{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Graphics.Gudni.OpenCL.CallKernels
  ( dumpBufferPart
  , dumpBufferPartTiles
  , iterateJobs
  , runGenerateThresholdsKernel
  , runMergeKernel
  , runCollectMergedKernel
  , runCollectRenderKernel
  , runSortKernel
  , runRenderKernel
  , runSplitKernel
  , runCombineSectionKernel
  , runPointQueryKernel
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

dumpBufferPart message buffer =
  do liftIO . putStrLn $ "----------------------------------- " ++ message ++ " -----------------------------------"
     hs <- readBuffer buffer
     liftIO . putStrLn . show $ hs

dumpBufferPartTiles :: String -> CLBuffer Tile -> CL ()
dumpBufferPartTiles message buffer =
  do liftIO . putStrLn $ "----------------------------------- " ++ message ++ " -----------------------------------"
     hs <- VS.take 32 <$> readBuffer buffer
     liftIO . putStrLn . concat . map (lpad 5 . show . view (tileBox . leftSide  )) . VS.toList $ hs
     liftIO . putStrLn . concat . map (lpad 5 . show . view (tileBox . topSide   )) . VS.toList $ hs
     liftIO . putStrLn . concat . map (lpad 5 . show . view (tileBox . rightSide )) . VS.toList $ hs
     liftIO . putStrLn . concat . map (lpad 5 . show . view (tileBox . bottomSide)) . VS.toList $ hs


iterateJobs :: Monad m => Int -> Int -> (Int -> Int -> Int -> m ()) -> m ()
iterateJobs total chunk f = go 0 0
  where go step offset = let size = min (total - offset) chunk
                         in  if offset < total
                             then do f step offset size
                                     go (step + 1) (offset + chunk)
                             else return ()

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
     blockSection    <- createBlockSection params
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
     -- TODO: Is itemSliceBuffer being freed
     return . set sectInUseLength len $ blockSection

runMergeKernel :: RasterParams token
               -> BlockSection
               -> CL ()
runMergeKernel params blockSection =
  let columnsPerBlock  = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
      columnDepth      = params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth
      blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
      maxJobSize       = params ^. rpRasterizer . rasterDeviceSpec . specMergeJobSize
      jobDepth         = params ^. rpRasterizer . rasterDeviceSpec . specMergeJobDepth
  in
  do  iterateJobs (blockSection ^. sectInUseLength) maxJobSize $
          \jobStep jobOffset jobSize ->
             do  dumpBufferPartTiles ("tiles " ++ show jobStep) (blockSection ^. sectTileBuffer)
                 dumpBufferPart ("inUseBuffer " ++ show jobStep) (blockSection ^. sectInUseBuffer)
                 -- right now there are two passes looking for blocks that can be merged
                 -- the stride offset determines the offset looking for these pairs.
                 forM_ [0,(-1)] $ \ strideOffset -> -- TODO: this is probably unneccessary
                    -- now loop through each exponent up to the job depth
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

runCollectMergedKernel :: RasterParams token
                       -> BlockSection
                       -> CL BlockSection
runCollectMergedKernel params blockSection =
    let blockDepth       = params ^. rpRasterizer . rasterDeviceSpec . specBlockSectionDepth
        blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
    in
    do  announceKernel "collectMergedKernel" 0 0 0
        (outputInUseLength, sideTiles) <-
            runKernel (params ^. rpRasterizer . rasterCollectMergedBlocksKernel)
                      (blockSection ^. sectTileBuffer     )
                      (blockSection ^. sectBlockIdBuffer  )
                      (blockSection ^. sectInUseBuffer    )
                      (toCInt  $  blockDepth      )
                      (toCInt  $  blocksPerSection)
                      (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                      (Out 1)
                      (Out 2)
                      (Local blocksPerSection :: LocalMem CInt)
                      (Work1D blocksPerSection)
                      (WorkGroup [blocksPerSection]) :: CL (VS.Vector CInt, VS.Vector Tile)
        let inUseLength = fromCInt . VS.head $ outputInUseLength
            firstTile   = (VS.!) sideTiles 0
            lastTile    = (VS.!) sideTiles 1
        dumpBufferPart ("afterCollectMerged inUseBuffer")   (blockSection ^. sectInUseBuffer)
        dumpBufferPart ("afterCollectMerged blockIdBuffer") (blockSection ^. sectBlockIdBuffer)
        dumpBufferPartTiles ("after Collect Merged Tiles ") (blockSection ^. sectTileBuffer)
        return . set sectInUseLength inUseLength
               . set sectFirstTile firstTile
               . set sectLastTile  lastTile  $ blockSection

runCollectRenderKernel :: RasterParams token
                       -> BlockSection
                       -> Tile
                       -> Tile
                       -> CL BlockSection
runCollectRenderKernel params blockSection prevTile nextTile =
    let blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
    in
    do  announceKernel "collectRenderKernel" 0 0 0
        (outputSplitLength, outputRenderLength) <-
            runKernel (params ^. rpRasterizer . rasterCollectRenderBlocksKernel)
                      (blockSection ^. sectTileBuffer)
                      (blockSection ^. sectBlockIdBuffer  )
                      (blockSection ^. sectInUseBuffer    )
                      (toCInt  $  blockSection ^. sectInUseLength)
                      (VS.fromList [prevTile, nextTile])
                      (toCInt  $  params ^. rpRasterizer . rasterDeviceSpec . specBlockSectionDepth)
                      (toCInt  $  blocksPerSection)
                      (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                      (Out 1)
                      (Out 1)
                      (Local blocksPerSection :: LocalMem CInt)
                      (Work1D blocksPerSection)
                      (WorkGroup [blocksPerSection]) :: CL (VS.Vector CInt, VS.Vector CInt)
        let splitLength  = fromCInt . VS.head $ outputSplitLength
            renderLength = fromCInt . VS.head $ outputRenderLength
        liftIO $ putStrLn $ "splitLength " ++ show splitLength ++ " renderLength " ++ show renderLength
        dumpBufferPart ("afterCollectRender inUseBuffer")   (blockSection ^. sectInUseBuffer)
        dumpBufferPart ("afterCollectRender blockIdBuffer") (blockSection ^. sectBlockIdBuffer)
        dumpBufferPartTiles ("after Collect Render Tiles ") (blockSection ^. sectTileBuffer)

        return . set sectRenderLength renderLength
               . set sectInUseLength splitLength $ blockSection


runSortKernel :: RasterParams token
              -> BlockSection
              -> CL ()
runSortKernel params blockSection =
    let columnsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
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
                            (blockSection ^. sectBlockIdBuffer   )
                            (toCInt $ blockSection ^. sectInUseLength )
                            (toCInt $ blockSection ^. sectRenderLength)
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
    let columnsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
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
                            (blockSection ^. sectBlockIdBuffer   )
                            (toCInt $ blockSection ^. sectInUseLength )
                            (toCInt $ blockSection ^. sectRenderLength)
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

runSplitKernel :: RasterParams token
               -> BlockSection
               -> BlockSection
               -> CL (BlockSection, BlockSection)
runSplitKernel params blockSection newBlockSection =
    let columnsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
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

runCombineSectionKernel :: RasterParams token
                        -> BlockSection
                        -> BlockSection
                        -> CL (BlockSection, BlockSection)
runCombineSectionKernel params blockSectionDst blockSectionSrc =
   let columnsPerBlock  = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
       blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
   in
   do  announceKernel "rasterCombineSectionKernel" 0 0 0
       (outputInUseLengthDst, outputInUseLengthSrc) <-
           runKernel (params ^. rpRasterizer . rasterSplitTileKernel)
                     (blockSectionDst ^. sectTileBuffer      )
                     (blockSectionDst ^. sectThresholdBuffer )
                     (blockSectionDst ^. sectHeaderBuffer    )
                     (blockSectionDst ^. sectQueueSliceBuffer)
                     (blockSectionDst ^. sectBlockIdBuffer   )
                     (blockSectionDst ^. sectInUseBuffer     )
                     (toCInt $ blockSectionDst ^. sectInUseLength     )

                     (blockSectionSrc ^. sectTileBuffer      )
                     (blockSectionSrc ^. sectThresholdBuffer )
                     (blockSectionSrc ^. sectHeaderBuffer    )
                     (blockSectionSrc ^. sectQueueSliceBuffer)
                     (blockSectionSrc ^. sectBlockIdBuffer   )
                     (blockSectionSrc ^. sectInUseBuffer     )
                     (toCInt $ blockSectionSrc ^. sectInUseLength     )
                     (toCInt blocksPerSection)

                     (toCInt  $  params ^. rpRasterizer . rasterDeviceSpec . specColumnDepth)
                     (toCInt <$> params ^. rpFrameSpec  . specBitmapSize)
                     (toCInt  $  params ^. rpFrameSpec  . specFrameCount)
                     (Out 1)
                     (Out 1)
                     (Work2D blocksPerSection columnsPerBlock)
                     (WorkGroup [1, columnsPerBlock]) :: CL (VS.Vector CInt, VS.Vector CInt)
       let inUseLengthDst = fromCInt . VS.head $ outputInUseLengthDst
           inUseLengthSrc = fromCInt . VS.head $ outputInUseLengthSrc
       return ( set sectInUseLength inUseLengthDst blockSectionDst
              , set sectInUseLength inUseLengthSrc blockSectionSrc
              )


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
