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
  , buildAndQueueRasterJobs
  , RasterParams(..)
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

import CLUtil.KernelArgs
import CLUtil

import Control.Concurrent.ParallelIO.Global

import Control.Monad.Morph

import Data.Word

data RasterParams = RasterParams
  { _rpLibrary         :: OpenCLKernelLibrary
  , _rpTarget          :: DrawTarget
  , _rpGeometryPile    :: GeometryPile
  , _rpPictData        :: Maybe (Pile Word8)
  , _rpPictRefs        :: [PictureRef PictureMemory]
  , _rpRandomField     :: RandomField
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
                 , Show a
                 )
              => OpenCLState
              -> CLKernel
              -> GeometryPile
              -> Maybe (Pile Word8)
              -> [PictureRef PictureMemory]
              -> VS.Vector CFloat
              -> CInt
              -> CInt
              -> CInt
              -> RasterJob
              -> a
              -> CL ()
generateCall state kernel geometryHeap pictData pictRefs randomField bitmapWidth bitmapHeight frame job target =
    let shapeHeap       = job ^. rJShapePile
        groupPile       = job ^. rJGroupPile
        tileHeap        = job ^. rJTilePile
        numTiles        = tileHeap ^. pileSize
        backgroundColor = job ^. rJBackgroundColor
        commonKernel pictPile pictRefPile =
            runKernel kernel
                      geometryHeap
                      shapeHeap
                      groupPile
                      tileHeap
                      pictPile
                      pictRefPile
                      randomField
                      backgroundColor
                      bitmapWidth
                      bitmapHeight
                      frame
                      target
    in
    do  pictPile <- case pictData of Nothing -> liftIO $ emptyPictureData
                                     Just p  -> liftIO $ return p
        pictRefPile <- liftIO $ listToPile pictRefs
        commonKernel pictPile pictRefPile
                     (Work2D (job ^. rJTilePile . pileSize) (fromIntegral cOMPUTEsIZE))
                     (WorkGroup [1, fromIntegral cOMPUTEsIZE])

raster :: CInt
       -> RasterParams
       -> RasterJob
       -> CL ()
raster frame params job =
    let tileW        = (fromIntegral $ mAXtILEsIZE ^. pX) :: CInt
        tileH        = (fromIntegral $ mAXtILEsIZE ^. pY) :: CInt
        (V2 w h)     = targetArea (params ^. rpTarget)
        outputSize   = fromIntegral $ w * h
        state        = clState (params ^. rpLibrary)
        numTiles     = (fromIntegral $ job ^. rJTilePile ^. pileSize) :: CInt
        rasterCall :: CL ()
        rasterCall =
            case targetBuffer (params ^. rpTarget) of
                HostBitmapTarget outputPtr ->
                    generateCall state (multiTileRasterCL (params ^. rpLibrary))
                                       (params ^. rpGeometryPile)
                                       (params ^. rpPictData)
                                       (params ^. rpPictRefs)
                                       (params ^. rpRandomField)
                                       w
                                       h
                                       frame
                                       job
                                       (OutPtr outputPtr outputSize)
                GLTextureTarget textureName -> error "GLTextureTarget not implemented"
    in do liftIO $ putStrLn $ ">>> rasterCall frame: " ++ show frame
          rasterCall
          liftIO $ putStrLn ">>> rasterCall done"

rasterSection :: CInt
              -> RasterParams
              -> RasterJobInput
              -> [Tile]
              -> CL ()
rasterSection frame params input section =
  do  job <- liftIO $ buildRasterJob input section
      raster frame params job

buildAndQueueRasterJobs :: CInt
                        -> RasterParams
                        -> RasterJobInput
                        -> TileTree
                        -> IO ()
buildAndQueueRasterJobs frame params input tileTree =
  do  let tilesPerCall = tr "tilesPerCall" $ fromIntegral . clMaxGroupSize $ params ^. rpLibrary
          tileGroups = trWith (show .  length) "tileGroups" $ breakList tilesPerCall . tileTreeToList $ tileTree
          state = clState (params ^. rpLibrary)
      liftIO $ mapM_ (runCL state . rasterSection frame params input) tileGroups
