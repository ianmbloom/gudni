{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Graphics.Gudni.OpenCL.ProcessBuffers
  ( divideEntrySequences
  , allocateTilePortion
  , allocateGeneration
  , collectTileBlocks
  , mergeTileBlocks
  , verticalSplitTile
  , dumpBufferPart
  , dumpBufferPartTiles
  , withBeforeAndAfter
  , checkBlockSection
  , condenseStack
  , processBufferStack
  , addPortionToPile
  , makeItemEntrySlice
  , splitAndMergeTileTree
  , makeTokenQuery
  , runRaster
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface.Query
import Graphics.Gudni.Interface.DrawTarget
import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.PrepareBuffers
import Graphics.Gudni.OpenCL.CallKernels
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

condenseStack blockSectionStack =
   fmap fromJust . S.filter isJust <$> mapM checkBlockSection blockSectionStack

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
       renderedStack <- withBeforeAndAfter mergedStack $
                   \ before after blockSection ->
                      do let prevTile = maybe nullTile (view sectLastTile)  before
                             nextTile = maybe nullTile (view sectFirstTile) after
                         toRenderSection <- runCollectRenderKernel params blockSection prevTile nextTile
                         --dumpBufferPart ("render length " ++ show (toRenderSection ^. sectRenderLength)) (toRenderSection ^. sectRenderBuffer)
                         --dumpBufferPart "renderCollected bufferId part " (toRenderSection ^. sectBlockIdBuffer)
                         when (toRenderSection ^. sectRenderLength > 0) $
                            do runSortKernel params toRenderSection
                               runRenderKernel params buffersInCommon toRenderSection target
                         return toRenderSection
       condensed <- condenseStack renderedStack
       splitStack <- forM condensed $
                   \ blockSection ->
                         if tr "**inUseLength" (blockSection ^. sectInUseLength) <= 0
                         then return $ S.singleton blockSection
                         else do newBlockSection <- createBlockSection params
                                 runSplitKernel params blockSection newBlockSection
                                 return $ S.singleton blockSection <|> S.singleton newBlockSection
       let joinStack = join splitStack
       iforM joinStack (\i buffers -> dumpBufferPart ("stack inuse  " ++ show i) (buffers ^. sectInUseBuffer))
       iforM joinStack (\i buffers -> dumpBufferPart ("stack render " ++ show i) (buffers ^. sectBlockIdBuffer))
       if   not (S.null joinStack)
       then processBufferStack params buffersInCommon joinStack target
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
