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


withBeforeAndAfterIndex :: Monad m => S.Seq a ->(Int -> Maybe a -> Maybe a -> a -> m b) -> m (S.Seq b)
withBeforeAndAfterIndex ss f = iforM ss go
   where
   len = S.length ss
   go i this =
     let before = if i > 0 then Just $ S.index ss (i-1) else Nothing
         after  = if i < len - 1 then Just $ S.index ss  (i+1)else Nothing
     in  f i before after this

checkBlockSection section = if section ^. sectInUseLength == 0
                            then do releaseBlockSection section
                                    return Nothing
                            else return . Just $ section

cleanStack blockSectionStack =
   fmap fromJust . S.filter isJust <$> mapM checkBlockSection blockSectionStack

byPairs :: Monad m => (a -> a -> m (Bool, Maybe a, Maybe a)) -> S.Seq a -> m (Bool, S.Seq a)
byPairs f ssss =
   case S.viewl ssss of
     S.EmptyL -> return (tr "ssss" False, ssss)
     (S.:<) s0 sss -> case S.viewl sss of
                        S.EmptyL -> return (tr "ssss2" False, ssss)
                        (S.:<) s1 ss ->
                           do (combined0, mA0, mA1) <- f s0 s1
                              (combined1, r) <- case (mA0, mA1) of
                                                    (Just a0, Just a1) -> do (combined1, r) <- byPairs f (a1 <| ss)
                                                                             return (combined1, a0 <| r)
                                                    (Just a0, Nothing) -> byPairs f (a0 <| ss)
                                                    (Nothing, Just a1) -> byPairs f (a1 <| ss)
                                                    (Nothing, Nothing) -> byPairs f ss
                              let combined = combined0 || combined1
                              return (combined, r)

combineSections :: RasterParams token
                -> BlockSection
                -> BlockSection
                -> CL (Bool, Maybe BlockSection, Maybe BlockSection)
combineSections params a b =
  do (combined, a', b') <- if (tr "a ^. sectInUseLength" $ a ^. sectInUseLength) > 0 &&
                              (tr "b ^. sectInUseLength" $ b ^. sectInUseLength) > 0
                           then do (a', b') <- runCombineSectionKernel params a b
                                   return (True, a', b')
                           else return (False, a, b)
     mA <- if a' ^. sectInUseLength <= 0
           then do releaseBlockSection a'
                   return Nothing
           else return $ Just a'
     mB <- if b' ^. sectInUseLength <= 0
           then do releaseBlockSection b'
                   return Nothing
           else return $ Just b'
     return (combined, mA, mB)

condenseStack :: RasterParams token -> S.Seq BlockSection -> CL (Bool, S.Seq BlockSection)
condenseStack params ss = byPairs (combineSections params) ss

showSections title stack =
    do liftIO $ putStrLn $ "{{{{{{{{{{{{{{{{{{{{{{{ " ++ title ++ " }}}}}}}}}}}}}}}}}}}}}}"
       iforM stack (\i buffers -> do dumpBufferPart (title ++ " inuse  " ++ show i) (buffers ^. sectInUseBuffer)
                                     dumpBufferPart (title ++ " render " ++ show i) (buffers ^. sectBlockIdBuffer)
                                     dumpBufferPartTiles (title ++ " tiles " ++ show i) (buffers ^. sectTileBuffer)
                        )
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
                   -> Int
                   -> CL (S.Seq BlockSection)
processBufferStack params buffersInCommon blockSectionStack target pass =
    do liftIO $ putStrLn $ "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
       liftIO $ putStrLn $ "||||||||||||||||||||||||       " ++ show pass ++ "       |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
       liftIO $ putStrLn $ "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"
       showSections "before" blockSectionStack
       liftIO $ putStrLn $ "|||||||||||||||||||||||| ProcessSection Section Start Length " ++ show (S.length blockSectionStack) ++ "||||||||||||"
       mergedStack <- forM blockSectionStack $ \ blockSection ->
                            do runMergeKernel params blockSection
                               runCollectMergedKernel params blockSection
       processedStack <- withBeforeAndAfterIndex mergedStack $
                   \ i before after blockSection ->
                      do let prevTile = maybe nullTile (view sectLastTile)  before
                             nextTile = maybe nullTile (view sectFirstTile) after
                         liftIO $ putStrLn $ "|||||||||||||||||||||||| Process Section Start " ++ show i ++ " of " ++ show (S.length blockSectionStack) ++ "  |||||||||||||||||||||||| "
                         section' <- do toRenderSection <- runCollectRenderKernel params blockSection prevTile nextTile
                                        --dumpBufferPart ("render length " ++ show (toRenderSection ^. sectRenderLength)) (toRenderSection ^. sectRenderBuffer)
                                        --dumpBufferPart "renderCollected bufferId part " (toRenderSection ^. sectBlockIdBuffer)
                                        when (toRenderSection ^. sectRenderLength > 0) $
                                           do runSortKernel params toRenderSection
                                              runRenderKernel params buffersInCommon toRenderSection target
                                        return toRenderSection
                         liftIO $ putStrLn $ "|||||||||||||||||||||||| Process Section End   " ++ show i ++ " of " ++ show (S.length blockSectionStack) ++ "  |||||||||||||||||||||||| "
                         return section'
       showSections "processed" processedStack
       (hasCombined, condensed) <- condenseStack params processedStack
       showSections ("condensed " ++ show hasCombined) condensed
       splitStack <-
           if tr "hasCombined" hasCombined
           then return condensed
           else uncurry (<|>) . S.unzip <$> (forM condensed $
                    \ blockSection ->
                          do newBlockSection <- createBlockSection params
                             dumpBufferPartTiles ("blockSection tiles ") (blockSection ^. sectTileBuffer)
                             dumpBufferPartTiles ("newBlockSection tiles ") (newBlockSection ^. sectTileBuffer)
                             runSplitKernel params blockSection newBlockSection

                             return $ (blockSection, set sectInUseLength (blockSection ^. sectInUseLength) newBlockSection))
       showSections "splitStack" splitStack
       if   not (S.null splitStack) && pass < 5
       then processBufferStack params buffersInCommon splitStack target (pass + 1)
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
     processBufferStack params buffersInCommon blockSectionStack target 0
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
