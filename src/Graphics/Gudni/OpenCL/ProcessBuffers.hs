{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Graphics.Gudni.OpenCL.ProcessBuffers
  ( collectTileBlocks
  , mergeTileBlocks
  , verticalSplitTile
  , dumpBufferPart
  , dumpBufferPartTiles
  , withBeforeAndAfterIndex
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
import Control.Concurrent

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

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


withBeforeAndAfterIndex :: Monad m => S.Seq a ->(Int -> Maybe a -> Maybe a -> a -> m b) -> m (S.Seq b)
withBeforeAndAfterIndex ss f = iforM ss go
   where
   len = S.length ss
   go i this =
     let before = if i > 0 then Just $ S.index ss (i-1) else Nothing
         after  = if i < len - 1 then Just $ S.index ss  (i+1)else Nothing
     in  f i before after this

checkBlockSection section = if section ^. sectNumActive == 0
                            then do releaseBlockSection section
                                    return Nothing
                            else return . Just $ section

cleanStack blockSectionStack =
   fmap fromJust . S.filter isJust <$> mapM checkBlockSection blockSectionStack

combineSections :: RasterParams token
                -> BlockSection
                -> BlockSection
                -> CL (Bool, BlockSection, BlockSection)
combineSections params dst src =
   let blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
       available = min (blocksPerSection - dst ^. sectNumActive) (src ^. sectNumActive)
   in
   if available > 0 &&
      dst ^. sectNumActive > 0 &&
      src ^. sectNumActive > 0 &&
      dst ^. sectLastTile == src ^. sectFirstTile
   then do (dst', src') <- runCombineSectionKernel params dst src
           return (True, dst', src')
   else return (False, dst, src)

condenseStack :: RasterParams token
              -> S.Seq BlockSection
              -> CL (Bool, S.Seq BlockSection)
condenseStack params ssss =
    do -- liftIO $ putStrLn $ "condenseStack " ++ show (S.length ssss)
       let blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
       case S.viewl ssss of
         S.EmptyL -> return (False, S.empty)
         (S.:<) s0 sss -> case S.viewl sss of
                            S.EmptyL -> return (False, ssss)
                            (S.:<) s1 ss ->
                               do (combined0, c0, c1) <- combineSections params s0 s1
                                  let c0L = c0 ^. sectNumActive
                                      before0 = if not combined0 && c0L > 0 then S.singleton c0 else S.empty
                                      mid0    = if combined0     && c0L > 0 then S.singleton c0 else S.empty
                                      after0  = if c0L <= 0                 then S.singleton c0 else S.empty
                                      c1L = c1 ^. sectNumActive
                                      before1 = if not combined0 && c1L > 0 then S.singleton c1 else S.empty
                                      mid1    = if combined0     && c1L > 0 then S.singleton c1 else S.empty
                                      after1  = if c1L <= 0                 then S.singleton c1 else S.empty
                                  (combined1, middle) <- condenseStack params (mid0 <> mid1 <> ss)
                                  let combined = combined0 || combined1
                                  return (combined, before0 <> before1 <> middle <> after0 <> after1)

sectionStackIsClear :: S.Seq BlockSection -> Bool
sectionStackIsClear stack =
  sum (fmap (view sectNumActive) stack) <= 0

releaseStack :: S.Seq BlockSection -> CL ()
releaseStack = mapM_ (\s -> when (s ^. sectNumActive <= 0) $ releaseBlockSection s)

data GenerateState a = GenState
  { _genData     :: a
  , _genProgress :: Int
  }
makeLenses ''GenerateState

type GenerateMonad s a = StateT (GenerateState s) CL a

mergeAndCollect :: RasterParams token -> Int -> BlockSection -> CL BlockSection
mergeAndCollect params strideExp section =
   do let startLength = section ^. sectNumActive
      section' <- foldM (runMergeKernel params 0) section [0,(-1)]
      collectedSection <- runCollectMergedKernel params section'
      if collectedSection ^. sectNumActive < startLength
      then mergeAndCollect params strideExp collectedSection
      else return collectedSection

caseLast :: S.Seq a -> b -> (S.Seq a -> a -> b) -> b
caseLast ss empty full =
      case S.viewr ss of
              S.EmptyR -> empty
              (S.:>) sss s -> full sss s

generateLoop :: (  KernelArgs
                  'KernelSync
                  'NoWorkGroups
                  'UnknownWorkItems
                  'Z
                  (target -> NumWorkItems -> WorkGroup -> CL ())
                )
             => RasterParams token
             -> BuffersInCommon
             -> target
             -> Pile ItemTagId
             -> S.Seq (Tile, Slice ItemTagId)
             -> Int
             -> CL ()
generateLoop params buffersInCommon target itemTagIdPile tileSlices batchSize =
    do    let  blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
               maxThresholds    = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
               processTile :: (Tile, Slice ItemTagId) -> GenerateMonad (S.Seq BlockSection) ()
               processTile (tile, slice) =
                   do  liftIO $ putStrLn $ "~~~~~~~ Tile Start" ++ show tile
                       genProgress .= 0
                       sectionLoop tile slice
                       stack <- use genData
                       lift $ showSections params "sectionLoopStack" stack
                       stack' <- lift $ processStack stack 0
                       genData .= stack'
                       liftIO $ putStrLn $ "~~~~~~~ Tile Complete " ++ show tile
               sectionLoop :: Tile -> Slice ItemTagId -> GenerateMonad (S.Seq BlockSection) ()
               sectionLoop tile slice =
                 do -- attempt to fill a block and see if the task is completed afterward.
                    moreToGenerate <- withSection (blockLoop tile slice)
                    if moreToGenerate
                    then do -- if there are more items to generate
                            -- first attempt to merge the existing blocks
                            withSection mergePass
                            len <- topNumActive
                            if len < blocksPerSection
                            then -- if space becomes available in the section loop again
                                 sectionLoop tile slice
                            else -- otherwise allocate a new section and generate into it
                                 holdSection (sectionLoop tile slice)
                    else return ()
               blockLoop :: Tile -> Slice ItemTagId -> GenerateMonad BlockSection Bool
               blockLoop tile slice  =
                 do -- use new blocks as required to generate thresholds for all the items in the tile.
                    -- initialize a block from the available blocks in the section
                    blockId <- initBlock tile
                    -- loop through each batch until no new items can be safely added to the block.
                    moreToGenerate <- batchLoop slice blockId
                    len <- use (genData . sectNumActive)
                    if moreToGenerate && len < blocksPerSection
                    then blockLoop tile slice
                    else return moreToGenerate
               batchLoop :: Slice ItemTagId -> BlockId -> GenerateMonad BlockSection Bool
               batchLoop slice blockId =
                 do let numItems = fromIntegral $ unBreadth $ sliceLength slice
                    progressBefore <- tr "progressBefore" <$> use genProgress
                    maxQueue <- generateBatch blockId (sliceStart slice) (tr "size" $ min (numItems - progressBefore) batchSize)
                    progress <- tr "progressAfter" <$> use genProgress
                    let spaceAvailableForBatch = tr "spaceAvailable" $ (tr "maxQueue" maxQueue) + batchSize < maxThresholds
                        moreToGenerate = tr "moreToGenerate" $ progress < (fromIntegral . unBreadth) (sliceLength slice)
                    if  spaceAvailableForBatch && moreToGenerate
                    then batchLoop slice blockId
                    else return moreToGenerate
               generateBatch blockId itemStart size =
                 do section <- use genData
                    progress <- use genProgress
                    (maxQueue, section') <- lift $ runGenerateThresholdsKernel params buffersInCommon itemTagIdPile itemStart section blockId progress size
                    genData .= section'
                    genProgress += size
                    return maxQueue
               initBlock tile =
                 do section <- use genData
                    let blockId = BlockId $ section ^. sectNumActive
                    section' <- lift $ runInitializeBlockKernel params section blockId tile
                    genData .= over sectNumActive (+1) section'
                    return blockId
               newSection  = lift $ runInitializeSectionKernel params
               popSection :: GenerateMonad (S.Seq BlockSection) BlockSection
               popSection =
                   do stack <- use genData
                      case S.viewl stack of
                              S.EmptyL -> do n <- newSection
                                             lift $ showSection params "popSection new" 0 n
                                             return n
                              (S.:<) s ss -> do genData .= ss
                                                lift $ showSection params "popSection" 0 s
                                                return s
               topNumActive :: GenerateMonad (S.Seq BlockSection) Int
               topNumActive =
                 do section <- popSection
                    pushSection section
                    return (section ^. sectNumActive)
               pushSection :: BlockSection -> GenerateMonad (S.Seq BlockSection) ()
               pushSection section = genData %= (section <|)
               withStack :: GenerateMonad (S.Seq BlockSection) () -> S.Seq BlockSection -> CL (S.Seq BlockSection)
               withStack code stack = do (GenState stack' progress') <- execStateT code (GenState stack 0)
                                         return stack'
               withSection :: GenerateMonad BlockSection a -> GenerateMonad (S.Seq BlockSection) a
               withSection code = do progress <- use genProgress
                                     section <- popSection
                                     (a, GenState section' progress') <- lift $ runStateT code (GenState section progress)
                                     pushSection section'
                                     genProgress .= progress'
                                     return a
               holdSection :: GenerateMonad (S.Seq BlockSection) a -> GenerateMonad (S.Seq BlockSection) a
               holdSection code = do -- hold the current section and execute code with the rest
                                     hold <- popSection
                                     a <- code
                                     pushSection hold
                                     return a
               processStack stack pass = processBufferStack params buffersInCommon target stack pass
               mergePass :: GenerateMonad BlockSection ()
               mergePass = do section <- use genData
                              section' <- lift $ mergeAndCollect params 0 section
                              genData .= section'
          liftIO $ putStrLn $ "tileSlices " ++ show tileSlices


          finishedStack <- withStack (mapM_ processTile tileSlices) S.empty -- loop through each tile slice while reusing the stack
          releaseStack finishedStack -- deallocate all blockSections

          return ()

processBufferStack :: (  KernelArgs
                        'KernelSync
                        'NoWorkGroups
                        'UnknownWorkItems
                        'Z
                        (target -> NumWorkItems -> WorkGroup -> CL ())
                      )
                   => RasterParams token
                   -> BuffersInCommon
                   -> target
                   -> S.Seq BlockSection
                   -> Int
                   -> CL (S.Seq BlockSection)
processBufferStack params buffersInCommon target blockSectionStack pass =
    do --liftIO $ putStrLn $ "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
       --liftIO $ putStrLn $ "||||||||||||||||||||||||       " ++ show pass ++ "       |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
       --liftIO $ putStrLn $ "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
       --liftIO $ putStrLn $ "|||||||||||||||||||||||| ProcessSection Section Start Length " ++ show (S.length blockSectionStack) ++ "||||||||||||"
       -- ORDER SHOULD BE
       --   render
       --   split
       --   merge

       mergedStack <- iforM blockSectionStack $ \ i blockSection ->
                            do --liftIO $ putStrLn $ "|||||||||||||||||||||||| Merge Section Start " ++ show i ++ " of " ++ show (S.length blockSectionStack) ++ "  |||||||||||||||||||||||| "
                               --showSection params "before" i blockSection
                               -- right now there are two passes looking for blocks that can be merged
                               -- the stride offset determines the offset looking for these pairs.
                               -- now loop through each exponent up to the job depth
                               let jobDepth = params ^. rpRasterizer . rasterDeviceSpec . specMergeJobDepth
                               blockSection' <- mergeAndCollect params 0 blockSection
                               showSection params "afterMerge" i blockSection'
                               --liftIO $ putStrLn $ "|||||||||||||||||||||||| Merge Section End   " ++ show i ++ " of " ++ show (S.length blockSectionStack) ++ "  |||||||||||||||||||||||| "
                               return blockSection'
       processedStack <- withBeforeAndAfterIndex mergedStack $
                   \ i before after blockSection ->
                      do let prevTile = maybe nullTile (view sectLastTile)  before
                             nextTile = maybe nullTile (view sectFirstTile) after
                         --liftIO $ putStrLn $ "|||||||||||||||||||||||| Process Section Start " ++ show i ++ " of " ++ show (S.length blockSectionStack) ++ "  |||||||||||||||||||||||| "
                         section' <- do toRenderSection <- runCollectRenderKernel params blockSection prevTile nextTile
                                        when ((tr "renderLength" $ toRenderSection ^. sectRenderLength) > 0) $
                                           do runSortKernel params toRenderSection
                                              runRenderKernel params buffersInCommon toRenderSection target
                                        return toRenderSection
                         --liftIO $ putStrLn $ "|||||||||||||||||||||||| Process Section End   " ++ show i ++ " of " ++ show (S.length blockSectionStack) ++ "  |||||||||||||||||||||||| "
                         return section'
       showSections params "rendered" processedStack
       (hasCombined, condensed) <- condenseStack params processedStack
       showSections params ("condensed " ++ show hasCombined) condensed
       splitStack <-
           if hasCombined
           then return condensed
           else uncurry (<|>) . S.unzip <$> (forM condensed $
                    \ blockSection ->
                          do newBlockSection <- createBlockSection params
                             --liftIO $ threadDelay 1000
                             runSplitKernel params blockSection newBlockSection
                             return $ (blockSection, set sectNumActive (blockSection ^. sectNumActive) newBlockSection))
       showSections params "splitStack" splitStack
       if   not (sectionStackIsClear splitStack) -- && pass < 5
       then processBufferStack params buffersInCommon target splitStack (pass + 1)
       else {-
            queryResults <- runPointQueryKernel params blockSection buffersInCommon tiles (params ^. rasterQueries)
            releaseBlockSection blockSection
            putStrLn ("rasterKernels Done             XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
            return queryResults
            -}
            return S.empty


addPortionToPile :: S.Seq ItemTagId -> StateT (Pile ItemTagId) CL (Slice ItemTagId)
addPortionToPile portion =
    do pile <- get
       (pile', slice) <- liftIO $ addSequenceToPile pile portion
       put pile'
       return slice

makeItemEntrySlice :: (Tile, S.Seq ItemTagId) -> StateT (Pile ItemTagId) CL (Tile, Slice ItemTagId)
makeItemEntrySlice (tile, portion) = do slice <- addPortionToPile portion
                                        return (tile, slice)

splitAndMergeTileTree :: ( KernelArgs
                           'KernelSync
                           'NoWorkGroups
                           'UnknownWorkItems
                           'Z
                           (target -> NumWorkItems -> WorkGroup -> CL ())
                         )
                      => RasterParams token
                      -> BuffersInCommon
                      -> TileTree (Tile, S.Seq ItemTagId)
                      -> target
                      -> CL (S.Seq (PointQueryId, SubstanceTag))
splitAndMergeTileTree params
                      buffersInCommon
                      tree
                      target =
  do -- Start by dividing all of the items into sub seqeunces that can definitely be generated given the memory restraints of the generation kernel.
     (sliceTree, itemTagIdPile) <- runStateT (traverseTileTree makeItemEntrySlice tree) =<< (liftIO newPile)
     -- Allocate space for each generation kernel.
     let tileSlices :: S.Seq (Tile, Slice ItemTagId)
         tileSlices = execState (traverseTileTree (\t -> modify ( |> t)) sliceTree) S.empty
         batchSize = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds `div` 2
     -- Generate all of the thresholds from the items
     generateLoop params buffersInCommon target itemTagIdPile tileSlices batchSize
     return S.empty


makeTokenQuery :: M.Map SubstanceTag token
               -> (PointQueryId, SubstanceTag)
               -> (PointQueryId, Maybe token)
makeTokenQuery mapping (queryId, substanceTag) =
  (queryId,
  if substanceTag == noSubstanceTag
  then Nothing
  else Just $ (M.!) mapping substanceTag)

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
