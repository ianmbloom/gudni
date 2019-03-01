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
import Graphics.Gudni.Raster.Primitive
import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Raster.TileTree
import Graphics.Gudni.Raster.Job

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Bag
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
  , _rpPictData        :: Maybe (Pile Word8)
  , _rpPictRefs        :: [PictureRef PictureMemory]
  , _rpRandomField     :: RandomField
  }
makeLenses ''RasterParams

pixelBufferSize :: CInt -> CInt -> CInt
pixelBufferSize width height = width * height

checkJob :: RasterJob -> Bool
checkJob job = not $ (isEmptyPile . view rJGeometryPile $ job) ||
                     (isEmptyPile . view rJGroupPile    $ job) ||
                     (isEmptyPile . view rJShapeRefPile  $ job) ||
                     (isEmptyPile . view rJTilePile     $ job)

emptyPictureData :: IO (Pile Word8)
emptyPictureData = do pile <- newPile
                      (pile', _) <- addToPile "emptyPictureData" pile (0::Word8) -- openCL will fail to launch kernel if a parameter is completely empty
                      return pile'

emptyPictureRefs :: [PictureRef PictureMemory]
emptyPictureRefs = [PictureRef origin 1 (PictureMemory 0 origin)]

generateCall  :: (KernelArgs
                      'KernelSync
                      'NoWorkGroups
                      'UnknownWorkItems
                      'Z
                      (a
                       -> CInt
                       -> CInt
                       -> CInt
                       -> CInt
                       -> CInt
                       -> CInt
                       -> OutputPtr CChar
                       -> CInt
                       -> Pile CUInt
                       -> NumWorkItems
                       -> WorkGroup
                       -> CL ())
                 , Show a
                 )
              => OpenCLState
              -> CLKernel
              -> Maybe (Pile Word8)
              -> [PictureRef PictureMemory]
              -> VS.Vector CFloat
              -> CInt
              -> CInt
              -> CInt
              -> CInt
              -> CInt
              -> CInt
              -> RasterJob
              -> OutputPtr CChar
              -> CInt
              -> a
              -> CL ()
generateCall state kernel pictData pictRefs randomField tileWidth tileHeight gridWidth bitmapWidth bitmapHeight frame job continuations passCount target =
    let geometryHeap    = job ^. rJGeometryPile
        shapeHeap       = job ^. rJShapePile
        shapeRefHeap    = job ^. rJShapeRefPile
        groupPile       = job ^. rJGroupPile
        tileHeap        = job ^. rJTilePile
        numTiles        = tileHeap ^. pileSize
        backgroundColor = job ^. rJBackgroundColor
        pictRefs' = if null pictRefs then emptyPictureRefs else pictRefs
        commonKernel pictPile pictRefPile continuations =
            runKernel kernel
                      geometryHeap
                      shapeHeap
                      shapeRefHeap
                      groupPile
                      tileHeap
                      pictPile
                      pictRefPile
                      randomField
                      backgroundColor
                      target
                      bitmapWidth
                      bitmapHeight
                      gridWidth
                      tileWidth
                      tileHeight
                      frame
                      continuations
                      passCount
    in
    do  pictPile <- case pictData of Nothing -> liftIO $ emptyPictureData
                                     Just p  -> liftIO $ return p
        pictRefPile <- liftIO $ listToPile pictRefs'
        tileIndexPile <- liftIO $ listToPile (reverse $ job ^. rJTileIndexList)
        commonKernel pictPile pictRefPile continuations tileIndexPile
                         (Work2D (tileIndexPile ^. pileSize) (fromIntegral tileWidth))
                         (WorkGroup [1, fromIntegral tileWidth])

generateFillCall  :: ( KernelArgs
                      'KernelSync
                      'NoWorkGroups
                      'UnknownWorkItems
                      'Z
                        ( a
                        -> CInt
                        -> CInt
                        -> CInt
                        -> CInt
                        -> CInt
                        -> Pile CUInt
                        -> NumWorkItems
                        -> WorkGroup
                        -> CL ())
                     , Show a
                     )
                  => OpenCLState
                  -> CLKernel
                  -> CInt
                  -> CInt
                  -> CInt
                  -> CInt
                  -> CInt
                  -> RasterJob
                  -> a
                  -> CL ()
generateFillCall state kernel tileWidth tileHeight gridWidth bitmapWidth bitmapHeight job target =
    let backgroundColor = job ^. rJBackgroundColor
        commonKernel = runKernel kernel
                                 backgroundColor
                                 target
                                 bitmapWidth
                                 bitmapHeight
                                 gridWidth
                                 tileWidth
                                 tileHeight
    in
    do  tileIndexPile <- liftIO $ listToPile (reverse $ job ^. rJTileIndexList)
        commonKernel tileIndexPile
               (Work2D (tileIndexPile ^. pileSize) (fromIntegral tileWidth))
               (WorkGroup [1, fromIntegral tileWidth])

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
        rasterCall :: OutputPtr CChar -> CInt -> CL ()
        rasterCall continuations passCount =
            case targetBuffer (params ^. rpTarget) of
                HostBitmapTarget outputPtr ->
                    generateCall state (multiTileRasterCL (params ^. rpLibrary))
                                       (params ^. rpPictData)
                                       (params ^. rpPictRefs)
                                       (params ^. rpRandomField)
                                       tileW
                                       tileH
                                       w
                                       h
                                       frame
                                       job
                                       continuations
                                       passCount
                                       (OutPtr outputPtr outputSize)
                GLTextureTarget textureName -> error "GLTextureTarget not implemented"
        fillCall =
            case targetBuffer (params ^. rpTarget) of
                HostBitmapTarget outputPtr ->
                    generateFillCall state (fillBackgroundCL (params ^. rpLibrary))
                                           tileW
                                           tileH
                                           gridWidth
                                           w
                                           h
                                           job
                                           (OutPtr outputPtr outputSize)
                GLTextureTarget textureName -> error "GLTextureTarget not implemented"
        numColumns = tr "numColumns" $ fromIntegral $ numTiles * tileW
        checkContinuations :: OutputPtr CChar -> CL (Vector Bool, Vector CInt)
        checkContinuations continuations = runKernel (checkContinuationCL (params ^. rpLibrary)) continuations (fromIntegral numColumns::CInt) (Out numColumns) (Out 1) (Work1D numColumns)
    in
    if checkJob job
    then do --liftIO $ outputRasterJob job
            let continuationSize = fromIntegral $ numTiles * tileW * cONTINUATIONaLIGN
            contPtr <- liftIO (mallocBytes continuationSize :: IO (Ptr CChar))
            let continuations = OutPtr contPtr continuationSize
                loop passCount passLimit = do liftIO $ putStrLn $ ">>> rasterCall pass:" ++ show passCount ++ " frame: " ++ show frame
                                              rasterCall continuations passCount
                                              --liftIO $ threadDelay 1000000
                                              liftIO $ putStr ">>> checkContinuation "
                                              isContinued <- VS.head . fst <$> checkContinuations continuations
                                              liftIO $ putStrLn $ show isContinued
                                              --liftIO $ threadDelay 1000000
                                              when (isContinued && passCount < passLimit) (loop (passCount + 1) passLimit)
            --fillCall
            loop 0 1000
            liftIO $ free contPtr
            liftIO $ putStrLn ">>> rasterCall done"

    else do --liftIO $ putStrLn "fillCall"
            fillCall


rasterSection :: CInt
              -> RasterParams
              -> RasterJobInput
              -> [Int]
              -> CL ()
rasterSection frame params input section =
 do let memoryLimit  = geoMemoryLimit (params ^. rpLibrary)
    internalJobs <- liftIO $ buildRasterJobs memoryLimit input section
    liftIO $ putStrLn $ show (length internalJobs) ++ " jobs."
    mapM_ (raster frame params) internalJobs

buildAndQueueRasterJobs :: MonadIO m
                        => CInt
                        -> RasterParams
                        -> RasterJobInput
                        -> TileTree
                        -> IO ()
buildAndQueueRasterJobs frame params input tileTree =
  do  tileGroups = tileTreeToGroups mAXtILESpERcALL tileTree

      tileIndexSections <- divideTiles (tr "clMaxGroupSize" $ clMaxGroupSize $ params ^. rpLibrary)
      let state = clState (params ^. rpLibrary)
      liftIO $
          do let threads :: IO ()
                 threads = mapM_ (runCL state . rasterSection frame params input) tileIndexSections
             threads
             --syncEvents <- threads
             --runCL state $ waitAll_ (concat syncEvents)
