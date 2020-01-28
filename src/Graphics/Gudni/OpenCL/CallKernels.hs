{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for preparing buffers and calling the rasterizer kernel∘

module Graphics.Gudni.OpenCL.CallKernels
  ( raster
  , RasterParams(..)
  , queueRasterJobs
  , buildRasterJobs
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Interface

import Control.Concurrent

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.SubstanceInfo
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Job

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.RandomField

import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.OpenCL.Instances
import Graphics.Gudni.Interface.GLInterop

import Control.Monad
import Control.Monad.State
import Control.Lens

import qualified Data.Vector.Storable as VS

import Foreign.C.Types(CUInt, CChar)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Linear
import Linear.Affine

import CLUtil.KernelArgs
import CLUtil.VectorBuffers
import CLUtil

import Control.Concurrent.ParallelIO.Global

import Control.Monad.Morph

import Data.Word
import qualified Data.Map as M
import GHC.Exts

data RasterParams token = RasterParams
  { _rpDevice          :: Rasterizer
  , _rpTarget          :: DrawTarget
  , _rpGeometryState   :: GeometryState
  , _rpSubstanceState  :: SubstanceState token SubSpace
  , _rpPictDataPile    :: Pile Word8
  , _rpPointQueries    :: [(PointQueryId, Point2 SubSpace)]
  }
makeLenses ''RasterParams

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
              -> CL [(PointQueryId,SubstanceId)]
generateCall params bic job bitmapSize frameCount jobIndex target =
  do  let numTiles     = job ^. rJTilePile . pileSize
          threadsToAlloc = fromIntegral $ job ^. rJThreadAllocation
          -- ideal number of threads per tile
          threadsPerTile = fromIntegral $ params ^. rpDevice . rasterSpec . specThreadsPerTile
          maxThresholds  = fromIntegral $ params ^. rpDevice . rasterSpec . specMaxThresholds
          -- adjusted log2 of the number of threads
          computeDepth = adjustedLog threadsPerTile :: CInt
          queries = toList $ job ^. rJPointQueries
      --liftIO $ outputGeometryState (params ^. rpGeometryState)
      --liftIO $ outputSubstanceState(params ^. rpSubstanceState)
      thresholdBuffer <- (allocBuffer [CL_MEM_READ_WRITE] (tr "allocSize thresholdBuffer " $ threadsToAlloc * maxThresholds) :: CL (CLBuffer THRESHOLDTYPE))
      headerBuffer    <- (allocBuffer [CL_MEM_READ_WRITE] (tr "allocSize headerBuffer    " $ threadsToAlloc * maxThresholds) :: CL (CLBuffer HEADERTYPE   ))
      shapeStateBuffer<- (allocBuffer [CL_MEM_READ_WRITE] (tr "allocSize shapeStateBuffer" $ threadsToAlloc * tr "sIZEoFsHAPEsTATE" sIZEoFsHAPEsTATE) :: CL (CLBuffer CChar))
      thresholdQueueSliceBuffer <- (allocBuffer [CL_MEM_READ_WRITE] (tr "allocSize thresholdQueueBuffer" $ threadsToAlloc) :: CL (CLBuffer (Slice Int)))
      liftIO $ putStrLn ("rasterGenerateThresholdsKernel XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
      runKernel (params ^. rpDevice . rasterGenerateThresholdsKernel)
                (bicGeoBuffer    bic)
                (bicGeoRefBuffer bic)
                (bicPictFacets bic)
                (job    ^. rJItemTagPile)
                (bicSubTagBuffer bic)
                (job    ^. rJTilePile)
                bitmapSize
                computeDepth
                frameCount
                jobIndex
                thresholdBuffer
                headerBuffer
                shapeStateBuffer
                thresholdQueueSliceBuffer
                (Work2D numTiles (fromIntegral threadsPerTile))
                (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
      liftIO $ putStrLn ("sortThresholdsKernel           XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
      runKernel (params ^. rpDevice . rasterSortThresholdsKernel)
                thresholdBuffer
                headerBuffer
                thresholdQueueSliceBuffer
                (job     ^. rJTilePile)
                bitmapSize
                computeDepth
                frameCount
                jobIndex
                (Work2D numTiles (fromIntegral threadsPerTile))
                (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
      liftIO $ putStrLn ("rasterRenderThresholdsKernel   XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
      runKernel (params ^. rpDevice . rasterRenderThresholdsKernel)
                thresholdBuffer
                headerBuffer
                shapeStateBuffer
                thresholdQueueSliceBuffer
                (bicPictFacets    bic)
                (bicPictBuffer bic) -- (params ^. rpPictData)
                (bicPictMemBuffer bic)
                (bicSolidColors bic)
                (bicRandoms    bic) -- (params ^. rpGeometryState  . geoRandomField)
                (job    ^. rJTilePile)
                (params ^. rpSubstanceState . suBackgroundColor)
                bitmapSize
                computeDepth
                frameCount
                jobIndex
                target
                (Work2D numTiles (fromIntegral threadsPerTile))
                (WorkGroup [1, fromIntegral threadsPerTile]) :: CL ()
      liftIO $ putStrLn ("rasterQueryKernel              XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
<<<<<<< HEAD
      -- queryResults <- let numPointQueries = length $ tr "pointQueries" $ job ^. rJPointQueries in
      --                 if  numPointQueries <= 0
      --                 then return []
      --                 else (toList :: CLUtil.Vector SubstanceId -> [SubstanceId]) <$>
      --                      runKernel (params ^. rpDevice . rasterQueryKernel)
      --                                thresholdBuffer
      --                                headerBuffer
      --                                shapeStateBuffer
      --                                thresholdQueueSliceBuffer
      --                                (bicPictFacets    bic)
      --                                (job    ^. rJTilePile)
      --                                bitmapSize
      --                                computeDepth
      --                                frameCount
      --                                jobIndex
      --                                (fromIntegral numPointQueries :: CInt)
      --                                (VS.fromList queries)
      --                                (Out numPointQueries)
      --                                (Work2D numTiles (fromIntegral threadsPerTile))
      --                                (WorkGroup [1, fromIntegral threadsPerTile])
      let queryResults = []
=======
      queryResults <- let numPointQueries = length $ tr "pointQueries" $ job ^. rJPointQueries in
                      if numPointQueries <= 0
                      then return []
                      else (toList :: CLUtil.Vector SubstanceId -> [SubstanceId]) <$>
                           runKernel (params ^. rpDevice . rasterQueryKernel)
                                     thresholdBuffer
                                     headerBuffer
                                     shapeStateBuffer
                                     thresholdQueueSliceBuffer
                                     (bicPictFacets    bic)
                                     (job    ^. rJTilePile)
                                     bitmapSize
                                     computeDepth
                                     frameCount
                                     jobIndex
                                     (fromIntegral numPointQueries :: CInt)
                                     (VS.fromList queries)
                                     (Out numPointQueries)
                                     (Work2D numTiles (fromIntegral threadsPerTile))
                                     (WorkGroup [1, fromIntegral threadsPerTile])
>>>>>>> 7833089a780ed57f917afe74093b27e7d02a47d2
      liftIO $ putStrLn ("rasterKernels Done             XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX-XXXXXXXXXXXXX");
      liftIO $ clReleaseMemObject . bufferObject $ thresholdBuffer
      liftIO $ clReleaseMemObject . bufferObject $ headerBuffer
      liftIO $ clReleaseMemObject . bufferObject $ shapeStateBuffer
      liftIO $ clReleaseMemObject . bufferObject $ thresholdQueueSliceBuffer
      return (tr "queryResults" $ zip (map pqQueryId queries) queryResults)

-- | Rasterize a rasterJob inside the CLMonad
raster :: Show token
       => RasterParams token
       -> BuffersInCommon
       -> CInt
       -> RasterJob
       -> CInt
       -> CL [(PointQueryId, SubstanceId)]
raster params bic frameCount job jobIndex =
    do  let -- width and height of the output buffer.
            bitmapSize   = P $ targetArea (params ^. rpTarget)
            -- total number of 32 bit words in the output buffer.
            outputSize   = fromIntegral $ pointArea bitmapSize
            -- get the actual target buffer we are writing to.
            buffer = targetBuffer (params ^. rpTarget)
        liftIO $ putStrLn $ ">>> rasterCall jobIndex: "++ show jobIndex ++ " frameCount: " ++ show frameCount
        -- generate a kernel call for that buffer type.
        -- liftIO $ outputRasterJob job
        queryResults <- case buffer of
                            HostBitmapTarget outputPtr ->
                                -- In this case the resulting bitmap will be stored in memory at outputPtr.
                                generateCall params bic job bitmapSize frameCount jobIndex (OutPtr outputPtr outputSize)
                            GLTextureTarget textureName ->
                                -- In this case an identifier for a Texture object that stays on the GPU would be stored∘
                                -- But currently this isn't working, so throw an error.
                                error "GLTextureTarget not implemented"
        liftIO $ putStrLn ">>> rasterCall done"
        return queryResults

data BuffersInCommon = BIC
  { bicGeoBuffer       :: CLBuffer CChar
  , bicGeoRefBuffer    :: CLBuffer GeoReference
  , bicPictMemBuffer   :: CLBuffer PictureMemoryReference
  , bicPictFacets      :: CLBuffer (HardFacet_ SubSpace TextureSpace)
  , bicSubTagBuffer    :: CLBuffer SubstanceTag
  , bicSolidColors     :: CLBuffer Color
  , bicPictBuffer      :: CLBuffer Word8
  , bicRandoms         :: CLBuffer CFloat
  }

makeTokenQuery :: M.Map SubstanceId token -> (PointQueryId, SubstanceId) -> (PointQueryId, Maybe token)
makeTokenQuery mapping (queryId, substanceId) =
  (queryId,
  if substanceId == noSubstanceId
  then Nothing
  else Just $ (M.!) mapping substanceId)

-- | Queue a list of Rasterjobs and run them inside the CLMonad.
queueRasterJobs :: (MonadIO m, Show token)
                => CInt
                -> RasterParams token
                -> [RasterJob]
                -> GeometryMonad m [(PointQueryId,Maybe token)]
queueRasterJobs frameCount params jobs =
    liftIO $ let -- Get the OpenCL state from the Library structure.
                    state = params ^. rpDevice . rasterClState
                    context = clContext state
             in
             runCL state $
             do bic <- liftIO $ do geoBuffer      <- pileToBuffer context (params ^. rpGeometryState  . geoGeometryPile   )
                                   geoRefBuffer   <- pileToBuffer context (params ^. rpGeometryState  . geoRefPile        )
                                   pictMemBuffer  <- pileToBuffer context (params ^. rpSubstanceState . suPictureMems     )
                                   facetBuffer    <- pileToBuffer context (params ^. rpSubstanceState . suFacetPile       )
                                   subTagBuffer   <- pileToBuffer context (params ^. rpSubstanceState . suSubstanceTagPile)
                                   colorBuffer    <- pileToBuffer context (params ^. rpSubstanceState . suSolidColorPile  )
                                   pictDataBuffer <- pileToBuffer context (params ^. rpPictDataPile)
                                   randoms        <- vectorToBuffer context (params ^. rpGeometryState  . geoRandomField)
                                   return $  BIC geoBuffer
                                                 geoRefBuffer
                                                 pictMemBuffer
                                                 facetBuffer
                                                 subTagBuffer
                                                 colorBuffer
                                                 pictDataBuffer
                                                 randoms
                -- Run the rasterizer over each rasterJob inside a CLMonad.
                queryResults <- concat <$> zipWithM (raster params bic frameCount) jobs [0..]
                return $ map (makeTokenQuery (params ^. rpSubstanceState . suTokenMap)) queryResults

buildRasterJobs :: (MonadIO m, Show token)
                => RasterParams token
                -> GeometryMonad m [RasterJob]
buildRasterJobs params =
  do  -- Get the tile tree from the geometryState
      tileTree <- use geoTileTree
      -- Determine the maximum number of tiles per RasterJob
      let maxThresholds = NumStrands $ fromIntegral $ params ^. rpDevice . rasterSpec . specMaxThresholds
          tilesPerCall = tr "tilesPerCall" $ fromIntegral $ params ^. rpDevice . rasterSpec . specMaxTilesPerJob
          threadsPerTile = tr "threadsPerTile" $ fromIntegral $ params ^. rpDevice . rasterSpec . specThreadsPerTile
          splitTree = tr "splitTree" $ splitTreeTiles maxThresholds tileTree

      -- Build all of the RasterJobs by traversing the TileTree.

      geoTileTree .= splitTree
      (numberedTileTree, finalState) <- runBuildJobsMonad (traverseTileTree (accumulateRasterJobs tilesPerCall threadsPerTile) splitTree)
      let jobs = finalState ^. bsCurrentJob : finalState ^. bsJobs
          pointTileQueries = tr "pointTileQueries" $ map (\(queryId, loc) -> (queryId, loc, locatePointInTileTree numberedTileTree loc)) (params ^. rpPointQueries)
          jobsWithQueries = foldl addPointQueryToRasterJobs jobs pointTileQueries
      return $ trWith (show . length) "num jobs" $ jobsWithQueries
