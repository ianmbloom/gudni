{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}

module Graphics.Gudni.OpenCL.CallKernels
  ( raster
  , OpenCLKernelLibrary(..)
  , geoMemoryLimit
  , groupSizeLimit
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
import Graphics.Gudni.Raster.Geometry
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Job

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.RandomField

import Graphics.Gudni.OpenCL.KernelLibrary
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.OpenCL.Instances
import Graphics.Gudni.OpenCL.GLInterop

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
import CLUtil

import Control.Concurrent.ParallelIO.Global

import Control.Monad.Morph

import Data.Word

data RasterParams token = RasterParams
  { _rpLibrary         :: OpenCLKernelLibrary
  , _rpPictData        :: Pile Word8
  , _rpTarget          :: DrawTarget
  , _rpGeometryState   :: GeometryState
  , _rpSubstanceState  :: SubstanceState token

  }
makeLenses ''RasterParams

pixelBufferSize :: CInt -> CInt -> CInt
pixelBufferSize width height = width * height

emptyPictureData :: IO (Pile Word8)
emptyPictureData = do pile <- newPile
                      (pile', _) <- addToPile "emptyPictureData" pile (0::Word8) -- openCL will fail to launch kernel if a parameter is completely empty
                      return pile'

emptyPictureRefs :: [PictureRef PictureMemory]
emptyPictureRefs = [PictureRef zeroPoint (PictureMemory 0 zeroPoint)]

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
              -> RasterJob
              -> Point2 CInt
              -> CInt
              -> a
              -> CL ()
generateCall params job bitmapSize frame target =
  do  let state        = clState (params ^. rpLibrary)
          numTiles     = job ^. rJTilePile . pileSize
          computeDepth = adjustedLog cOMPUTEsIZE :: CInt
      --liftIO $ outputGeometryState (params ^. rpGeometryState)
      --liftIO $ outputSubstanceState(params ^. rpSubstanceState)
      pictRefPile <- liftIO $ listToPile (params ^. rpSubstanceState . suPictureRefs)
      runKernel (multiTileRasterCL (params ^. rpLibrary))
                (params ^. rpGeometryState  . geoGeometryPile)
                (params ^. rpSubstanceState . suSubstancePile)
                (job    ^. rJShapePile)
                (job    ^. rJTilePile)
                (params ^. rpPictData)
                pictRefPile
                (params ^. rpGeometryState  . geoRandomField)
                (params ^. rpSubstanceState . suBackgroundColor)
                bitmapSize
                computeDepth
                frame
                target
                (Work2D numTiles (fromIntegral cOMPUTEsIZE))
                (WorkGroup [1, fromIntegral cOMPUTEsIZE])


raster :: Show token
       => CInt
       -> RasterParams token
       -> RasterJob
       -> CL ()
raster frame params job =
    do  let bitmapSize   = P $ targetArea (params ^. rpTarget)
            outputSize   = fromIntegral $ pointArea bitmapSize
        liftIO $ putStrLn $ ">>> rasterCall frame: " ++ show frame
        case targetBuffer (params ^. rpTarget) of
            HostBitmapTarget outputPtr ->
                generateCall params job bitmapSize frame (OutPtr outputPtr outputSize)
            GLTextureTarget textureName -> error "GLTextureTarget not implemented"
        liftIO $ putStrLn ">>> rasterCall done"

queueRasterJobs :: (MonadIO m, Show token)
                => CInt
                -> RasterParams token
                -> [RasterJob]
                -> GeometryMonad m ()
queueRasterJobs frame params jobs =
    do  let state = clState (params ^. rpLibrary)
        liftIO $ mapM_ (runCL state . raster frame params) jobs

buildRasterJobs :: (MonadIO m, Show token)
                => RasterParams token
                -> GeometryMonad m [RasterJob]
buildRasterJobs params =
  do  tileTree <- use geoTileTree
      let tilesPerCall = fromIntegral . clMaxGroupSize $ params ^. rpLibrary
      jobs <- execBuildJobsMonad (traverseTileTree (buildRasterJob tilesPerCall) tileTree)
      return jobs
