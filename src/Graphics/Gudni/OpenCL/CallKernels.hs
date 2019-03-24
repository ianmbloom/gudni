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
  , OpenCLKernelLibrary(..)
  , RasterParams(..)
  , queueRasterJobs
  , buildRasterJobs
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface

import Control.Concurrent

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.ShapeInfo
import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.Serialize
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Job

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.RandomField

import Graphics.Gudni.OpenCL.KernelLibrary
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

data RasterParams token = RasterParams
  { _rpLibrary         :: OpenCLKernelLibrary
  , _rpPictData        :: VS.Vector Word8
  , _rpTarget          :: DrawTarget
  , _rpGeometryState   :: GeometryState
  , _rpSubstanceState  :: SubstanceState token

  }
makeLenses ''RasterParams

-- | Generate an call the rasterizer kernel. Polymorphic over the DrawTarget type.
generateCall  :: (KernelArgs
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
              -> CL ()
generateCall params bic job bitmapSize frame jobIndex target =
  do  let numTiles     = job ^. rJTilePile . pileSize
          -- ideal number of threads per tile
          computeSize  = fromIntegral . clMaxGroupSize $ params ^. rpLibrary
          -- adjusted log2 of the number of threads
          computeDepth = adjustedLog computeSize :: CInt
      --liftIO $ outputGeometryState (params ^. rpGeometryState)
      --liftIO $ outputSubstanceState(params ^. rpSubstanceState)
      runKernel (multiTileRasterCL (params ^. rpLibrary))
                (bicGeoBuffer  bic) -- (params ^. rpGeometryState  . geoGeometryPile)
                (bicSubBuffer  bic) -- (params ^. rpSubstanceState . suSubstancePile)
                (bicPictBuffer bic) -- (params ^. rpPictData)
                (bicPictUsage  bic) -- (params ^. rpSubstanceState . suPictureUsages)
                (bicRandoms    bic) -- (params ^. rpGeometryState  . geoRandomField)
                (job    ^. rJShapePile)
                (job    ^. rJTilePile)
                (params ^. rpSubstanceState . suBackgroundColor)
                bitmapSize
                computeDepth
                frame
                jobIndex
                target
                (Work2D numTiles (fromIntegral computeSize))
                (WorkGroup [1, fromIntegral computeSize])

-- | Rasterize a rasterJob inside the CLMonad
raster :: Show token
       => RasterParams token
       -> BuffersInCommon
       -> CInt
       -> RasterJob
       -> CInt
       -> CL ()
raster params bic frame job jobIndex =
    do  let -- width and height of the output buffer.
            bitmapSize   = P $ targetArea (params ^. rpTarget)
            -- total number of 32 bit words in the output buffer.
            outputSize   = fromIntegral $ pointArea bitmapSize
            -- get the actual target buffer we are writing to.
            buffer = targetBuffer (params ^. rpTarget)
        liftIO $ putStrLn $ ">>> rasterCall jobIndex: "++ show jobIndex ++ " frame: " ++ show frame
        -- generate a kernel call for that buffer type.
        case buffer of
            HostBitmapTarget outputPtr ->
                -- In this case the resulting bitmap will be stored in memory at outputPtr.
                generateCall params bic job bitmapSize frame jobIndex (OutPtr outputPtr outputSize)
            GLTextureTarget textureName ->
                -- In this case an identifier for a Texture object that stays on the GPU would be stored∘
                -- But currently this isn't working, so throw an error.
                error "GLTextureTarget not implemented"
        liftIO $ putStrLn ">>> rasterCall done"

data BuffersInCommon = BIC
  { bicGeoBuffer :: CLBuffer CChar
  , bicSubBuffer :: CLBuffer SubstanceInfo
  , bicPictBuffer:: CLBuffer Word8
  , bicPictUsage :: CLBuffer (PictureUsage PictureMemoryReference)
  , bicRandoms   :: CLBuffer CFloat
  }


-- | Queue a list of Rasterjobs and run them inside the CLMonad.
queueRasterJobs :: (MonadIO m, Show token)
                => CInt
                -> RasterParams token
                -> [RasterJob]
                -> GeometryMonad m ()
queueRasterJobs frame params jobs =
    liftIO $ do let -- Get the OpenCL state from the Library structure.
                    state = clState (params ^. rpLibrary)
                    context = clContext state
                geoBuffer  <- pileToBuffer context (params ^. rpGeometryState  . geoGeometryPile)
                subBuffer  <- pileToBuffer context (params ^. rpSubstanceState . suSubstancePile)
                pictBuffer <- vectorToBuffer context (params ^. rpPictData)
                pictUsage  <- pileToBuffer context (params ^. rpSubstanceState . suPictureUsages)
                randoms    <- vectorToBuffer context (params ^. rpGeometryState  . geoRandomField)
                let bic = BIC geoBuffer subBuffer pictBuffer pictUsage randoms
                -- Run the rasterizer over each rasterJob inside a CLMonad.
                runCL state $ zipWithM_ (raster params bic frame) jobs [0..]

buildRasterJobs :: (MonadIO m, Show token)
                => RasterParams token
                -> GeometryMonad m [RasterJob]
buildRasterJobs params =
  do  -- Get the tile tree from the geometryState
      tileTree <- use geoTileTree
      -- Determine the maximum number of tiles per RasterJob
      let tilesPerCall = fromIntegral . clMaxGroupSize $ params ^. rpLibrary
      -- Build all of the RasterJobs by traversing the TileTree.
      jobs <- execBuildJobsMonad (traverseTileTree (accumulateRasterJobs tilesPerCall) tileTree)
      return $ trWith (show . length) "num jobs" $ jobs
