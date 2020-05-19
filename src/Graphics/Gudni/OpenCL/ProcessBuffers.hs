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
  , addPortionToPile
  , makeItemEntrySlice
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
import Graphics.Gudni.Raster.Params
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
import Data.Maybe hiding (catMaybes)
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
import Data.Filtrable

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
releaseStack = mapM_ (\s -> when (True {-s ^. sectNumActive <= 0-}) $ releaseBlockSection s)

data GenerateState a = GenState
  { _genData     :: a
  , _genProgress :: Int
  }
makeLenses ''GenerateState

type GenerateMonad s a = StateT (GenerateState s) CL a

mergeAndCollect :: RasterParams token -> Int -> BlockSection -> CL BlockSection
mergeAndCollect params strideExp section =
   do let startLength = section ^. sectNumActive
      section' <- foldM (runMergeBlockKernel params 0) section [0,(-1)]
      collectedSection <- runCollectMergedBlockKernel params section'
      if collectedSection ^. sectNumActive < startLength
      then mergeAndCollect params strideExp collectedSection
      else return collectedSection

caseLast :: S.Seq a -> b -> (S.Seq a -> a -> b) -> b
caseLast ss empty full =
      case S.viewr ss of
              S.EmptyR -> empty
              (S.:>) sss s -> full sss s

unzipAndConcat :: S.Seq (Maybe a, Maybe a) -> S.Seq a
unzipAndConcat = fmap fromJust . S.filter isJust . uncurry (<>) . S.unzip

announceStack = announcePart (show . S.length)
announceSection = announcePart (show . view sectNumActive)

announcePart :: (b -> String) -> String -> GenerateMonad b a -> GenerateMonad b a
announcePart g message f =
  do testProgress <- use genProgress
     item <- use genData
     liftIO $ putStrLn $ "~~~~~~~ START " ++ message ++ " length " ++ g item ++ " progress " ++ show testProgress
     result <- f
     testProgress <- use genProgress
     item <- use genData
     liftIO $ putStrLn $ "~~~~~~~   END " ++ message ++ " length " ++ g item ++ " progress " ++ show testProgress
     return result

{-
     queryResults <- runPointQueryKernel params blockSection buffersInCommon tiles (params ^. rasterQueries)
     releaseBlockSection blockSection
     putStrLn ("rasterKernels Done             XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
     return queryResults
-}

generateLoop :: ( KernelArgs
                  'KernelSync
                  'NoWorkGroups
                  'UnknownWorkItems
                  'Z
                  (target -> NumWorkItems -> WorkGroup -> CL ())
                )
             => RasterParams token
             -> BuffersInCommon
             -> TileTree (Tile, Slice ItemTagId)
             -> target
             -> CL (S.Seq (PointQueryId, SubstanceTag))
generateLoop params buffersInCommon sliceTree target =
    do  let  blocksPerSection = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
             maxThresholds    = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
             batchSize        = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds `div` 2
             tileSlices :: S.Seq (Tile, Slice ItemTagId)
             tileSlices = execState (traverseTileTree (\t -> modify ( |> t)) sliceTree) S.empty
             processTile :: (Tile, Slice ItemTagId) -> GenerateMonad (S.Seq BlockSection) ()
             processTile (tile, slice) = announceStack ("tile: " ++ show tile) $
                 do  genProgress .= 0
                     sectionLoop tile slice
                     stack <- use genData
                     --lift $ showSections params "generated" stack
                     mergeAll
                     processStack 0
             sectionLoop :: Tile -> Slice ItemTagId -> GenerateMonad (S.Seq BlockSection) ()
             sectionLoop  tile slice = -- announceStack "section" $
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
             blockLoop tile slice  = --announceSection "block" $
               do -- use new blocks as required to generate thresholds for all the items in the tile.
                  -- initialize a block from the available blocks in the section
                  blockPtr <- initBlock tile
                  -- loop through each batch until no new items can be safely added to the block.
                  moreToGenerate <- batchLoop slice blockPtr
                  len <- use (genData . sectNumActive)
                  if moreToGenerate && len < blocksPerSection
                  then blockLoop tile slice
                  else return moreToGenerate
             batchLoop :: Slice ItemTagId -> BlockPtr -> GenerateMonad BlockSection Bool
             batchLoop slice blockPtr = --announceSection "batch" $
               do let numItems = fromIntegral $ unBreadth $ sliceLength slice
                  progressBefore <- use genProgress
                  maxQueue <- generateBatch blockPtr (sliceStart slice) (min (numItems - progressBefore) batchSize)
                  progress <- use genProgress
                  let spaceAvailableForBatch = maxQueue + batchSize < maxThresholds
                      moreToGenerate = progress < (fromIntegral . unBreadth) (sliceLength slice)
                  if  spaceAvailableForBatch && moreToGenerate
                  then batchLoop slice blockPtr
                  else return moreToGenerate
             generateBatch :: BlockPtr -> Reference ItemTagId -> Int -> GenerateMonad BlockSection Int
             generateBatch blockPtr itemStart size = --announceSection "generateBatch" $
               do section <- use genData
                  progress <- use genProgress
                  (maxQueue, section') <- lift $ runGenerateThresholdsKernel params buffersInCommon itemStart section blockPtr progress size
                  genData .= section'
                  genProgress += size
                  return maxQueue
             initBlock :: Tile -> GenerateMonad BlockSection BlockPtr
             initBlock tile =
                 do section <- use genData
                    let blockPtr = BlockPtr $ section ^. sectNumActive
                    section' <- lift $ runInitializeBlockKernel params section blockPtr tile
                    genData .= over sectNumActive (+1) section'
                    return blockPtr
             newSection :: GenerateMonad (S.Seq BlockSection) BlockSection
             newSection = -- announceStack "newSection" $
                 lift $ do blockSection <- createBlockSection params
                           runInitializeSectionKernel params blockSection
             popSection :: GenerateMonad (S.Seq BlockSection) BlockSection
             popSection = -- announceStack "popSection" $
                 do stack <- use genData
                    case S.viewl stack of
                            S.EmptyL -> do n <- newSection
                                           --lift $ showSection params "popSection new" 0 n
                                           return n
                            (S.:<) s ss -> do genData .= ss
                                              --lift $ showSection params "popSection" 0 s
                                              return s

             topNumActive :: GenerateMonad (S.Seq BlockSection) Int
             topNumActive = -- announceStack "topNumActive" $
               do section <- popSection
                  pushSection section
                  return (section ^. sectNumActive)
             pushSection :: BlockSection -> GenerateMonad (S.Seq BlockSection) ()
             pushSection section = -- announceStack "pushSection" $
                  do genData %= (section <|)
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
             processStack :: Int -> GenerateMonad (S.Seq BlockSection) ()
             processStack pass = -- announceStack "processStack" $
               do collectAndRender
                  condenseAndSplit
                  mergeAll
                  mergedStack <- use genData
                  if   not (sectionStackIsClear mergedStack) && pass < 3
                  then processStack (pass + 1)
                  else return ()
             collectAndRender :: GenerateMonad (S.Seq BlockSection) ()
             collectAndRender = -- announceStack "collectAndRender" $
                 do stack <- use genData
                    renderedStack <- withBeforeAndAfterIndex stack $
                                \ i before after blockSection ->
                                   do let prevTile = maybe nullTile (view sectLastTile)  before
                                          nextTile = maybe nullTile (view sectFirstTile) after
                                      section' <- lift $ do toRenderSection <- runCollectRenderKernel params blockSection prevTile nextTile
                                                            when (toRenderSection ^. sectNumToRender > 0) $
                                                               do runSortThresholdsKernel params toRenderSection
                                                                  runRenderThresholdsKernel params buffersInCommon toRenderSection target
                                                            return toRenderSection
                                      return section'
                    --lift $ showSections params "rendered" renderedStack
                    genData .= renderedStack
             condenseAndSplit :: GenerateMonad (S.Seq BlockSection) ()
             condenseAndSplit = -- announceStack "condenseAndSplit" $
               do stack <- use genData
                  (hasCombined, condensed) <- lift $ condenseStack params stack
                  --lift $ showSections params ("condensed " ++ show hasCombined) condensed
                  splitStack <-
                      if hasCombined
                      then return condensed
                      else unzipAndConcat <$> (
                               iforM condensed $
                               \ i blockSection -> --announceStack ("split " ++ show i) $
                                     if (blockSection ^. sectNumActive > 0)
                                     then if (blockSection ^. sectNumActive * 2 > blocksPerSection)
                                          then  lift $ do newBlockSection <- createBlockSection params
                                                          --liftIO $ putStrLn $ "blockSection to split " ++ show (blockSection ^. sectNumActive)
                                                          runSplitBlocksKernel params blockSection 0 newBlockSection 0
                                                          return (Just blockSection, Just $ set sectNumActive (blockSection ^. sectNumActive) newBlockSection)
                                          else lift $ do runSplitBlocksKernel params blockSection 0 blockSection (blockSection ^. sectNumActive)
                                                         return (Just (over sectNumActive (*2) blockSection), Nothing)
                                     else lift $ return (Just blockSection, Nothing)
                              )
                  --lift $ showSections params ("split " ++ show hasCombined) splitStack
                  genData .= splitStack
             mergeAll :: GenerateMonad (S.Seq BlockSection) ()
             mergeAll = --announceStack "mergeAll" $
               do stack <- use genData
                  mergedStack <- iforM stack $ \ i blockSection ->
                                       lift $ mergeAndCollect params 0 blockSection
                  --lift $ showSections params ("mergedAll ") mergedStack
                  genData .= mergedStack
             mergePass :: GenerateMonad BlockSection ()
             mergePass = do section <- use genData
                            section' <- lift $ mergeAndCollect params 0 section
                            genData .= section'
        --liftIO $ putStrLn $ "tileSlices " ++ show tileSlices
        finishedStack <- withStack (mapM_ processTile tileSlices) S.empty -- loop through each tile slice while reusing the stack
        releaseStack finishedStack -- deallocate all blockSections
        return S.empty

{-
addPortionToPile :: S.Seq ItemTagId -> StateT (Pile ItemTagId) CL (Slice ItemTagId)
addPortionToPile portion =
    do pile <- get
       (pile', slice) <- liftIO $ addSequenceToPile pile portion
       put pile'
       return slice

makeItemEntrySlice :: (Tile, S.Seq ItemTagId) -> StateT (Pile ItemTagId) CL (Tile, Slice ItemTagId)
makeItemEntrySlice (tile, portion) = do slice <- addPortionToPile portion
                                        return (tile, slice)
-}

addPortionToPile :: Pile ItemTagId -> StateT (Pile ItemTagId) CL (Slice ItemTagId)
addPortionToPile portion =
    do pile <- get
       (pile', slice) <- liftIO $ addPileToPile pile portion
       put pile'
       return slice

makeItemEntrySlice :: (Tile, Pile ItemTagId) -> StateT (Pile ItemTagId) CL (Tile, Slice ItemTagId)
makeItemEntrySlice (tile, portion) = do slice <- addPortionToPile portion
                                        return (tile, slice)

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
    do  let tileTree = params ^. rpSerialState . serTileTree
        -- Get the OpenCL state from the Library structure.
            state = params ^. rpRasterizer . rasterClState
        -- total number of 32 bit words in the output buffer.
        -- liftIO $ outputGeometryState (params ^. rpGeometryState)
        -- liftIO $ outputSerialState(params ^. rpSerialState)
        runCL state $
            do (sliceTree, itemTagIdPile) <- runStateT (traverseTileTree makeItemEntrySlice tileTree) =<< (liftIO newPile)
               liftIO $ putStrLn "tileTreeTraversal complete"
               queryResults <- withBuffersInCommon params itemTagIdPile $
                     \ buffersInCommon ->
                     -- | Create the buffers in common, which are the read only buffers that the rasterization kernel will use
                     -- to generate thresholds and render images
                     case params ^. rpDrawTarget . targetBuffer of
                           HostBitmapTarget outputPtr ->
                               let outputSize   = fromIntegral $ pointArea (params ^. rpBitmapSize)
                               in  -- In this case the resulting bitmap will be stored in memory at outputPtr.
                                   generateLoop params buffersInCommon sliceTree (OutPtr outputPtr outputSize)
                           GLTextureTarget textureName ->
                               -- In this case an identifier for a Texture object that stays on the GPU would be stored∘
                               -- But currently this isn't working, so throw an error.
                               error "GLTextureTarget not implemented"
               liftIO $ freePile itemTagIdPile
               return $ fmap (makeTokenQuery (params ^. rpSerialState . serTokenMap)) queryResults
