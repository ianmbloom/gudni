{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Thresholds.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for preparing buffers and calling the rasterizer kernel∘

module Graphics.Gudni.Raster.Thresholds.OpenCL.PrepareBuffers
  ( BuffersInCommon(..)
  , bicGeometryHeap
  , bicFacetHeap
  , bicItemTagHeap
  , bicSubTagHeap
  , bicDescriptions
  , bicPictHeap
  , bicRandoms

  , readBufferToPtr
  , withBuffersInCommon

  , BlockId(..)
  , BlockPtr(..)
  , BlockSection(..)
  , sectTileBuffer
  , sectThresholdBuffer
  , sectThresholdTagBuffer
  , sectQueueSliceBuffer
  , sectBlockIdBuffer
  , sectNumToRender
  , sectNumActive
  , sectActiveFlagBuffer
  , sectFirstTile
  , sectLastTile
  , createBlockSection
  , releaseBlockSection

  , nullTile
  , PointQuery(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface
import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Raster.Thresholds.Constants
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Raster.Thresholds.ItemInfo
import Graphics.Gudni.Raster.Thresholds.SubstanceInfo
import Graphics.Gudni.Raster.Thresholds.Enclosure
import Graphics.Gudni.Raster.Thresholds.Serialize
import Graphics.Gudni.Raster.Thresholds.TileTree
import Graphics.Gudni.Raster.Thresholds.Params

import Graphics.Gudni.Raster.Thresholds.OpenCL.RasterState
import Graphics.Gudni.Raster.OpenCL.Buffer
import Graphics.Gudni.Raster.OpenCL.DeviceQuery
import Graphics.Gudni.Raster.OpenCL.Instances
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.CTypeConversion

import Control.Concurrent

import Control.Monad
import Control.Monad.State
import Control.Lens

import qualified Data.Vector.Storable as VS

import Foreign.C.Types(CUInt, CChar, CBool)
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
import qualified Data.Sequence as S

import Data.Word
import qualified Data.Map as M
import GHC.Exts

data BuffersInCommon = BuffersInCommon
  { _bicGeometryHeap   :: CLBuffer CChar
  , _bicFacetHeap      :: CLBuffer (Facet SubSpace)
  , _bicItemTagHeap    :: CLBuffer PrimTag
  , _bicSubTagHeap     :: CLBuffer SubstanceTag
  , _bicDescriptions   :: CLBuffer CChar
  , _bicPictHeap       :: CLBuffer CFloat
  , _bicRandoms        :: CLBuffer CFloat
  }
makeLenses ''BuffersInCommon

withBuffersInCommon :: RasterParams token -> (BuffersInCommon -> CL a) -> CL a
withBuffersInCommon params code =
   do  bic <- createBuffersInCommon params
       result <- code bic
       releaseBuffersInCommon bic
       return result

readBufferToPtr :: forall a . Storable a => Ptr a -> Int -> CLBuffer a -> CL ()
readBufferToPtr ptr size buffer =
  do let m = sizeOf (undefined :: a)
     queue <- clQueue <$> ask
     liftIO $ do ev <- clEnqueueReadBuffer queue (bufferObject buffer) False 0 (m*size) (castPtr ptr) []
                 _  <- clWaitForEvents [ev]
                 void $ clReleaseEvent ev
     return ()

createBuffersInCommon :: RasterParams token -> CL BuffersInCommon
createBuffersInCommon params =
    do  geoBuffer       <- bufferFromPile   "geoBuffer        " (params ^. rpSerialState . serGeometryPile    )
        facetBuffer     <- bufferFromPile   "facetBuffer      " (params ^. rpSerialState . serFacetPile       )
        itemTagBuffer   <- bufferFromPile   "itemTagBuffer    " (params ^. rpSerialState . serItemTagPile     )
        subTagBuffer    <- bufferFromPile   "subTagBuffer     " (params ^. rpSerialState . serSubstanceTagPile)
        descriptionBuffer <- bufferFromPile "descriptionBuffer" (params ^. rpSerialState . serDescriptionPile  )
        pixelPileBuffer  <- bufferFromPile   "pixelPileBuffer   " (params ^. rpPixelPile)
        randoms         <- bufferFromVector "randoms          " (params ^. rpRasterState  . rasterRandomField  )
        return $  BuffersInCommon
                  { _bicGeometryHeap   = geoBuffer
                  , _bicFacetHeap      = facetBuffer
                  , _bicItemTagHeap    = itemTagBuffer
                  , _bicSubTagHeap     = subTagBuffer
                  , _bicDescriptions   = descriptionBuffer
                  , _bicPictHeap       = pixelPileBuffer
                  , _bicRandoms        = randoms
                  }

releaseBuffersInCommon :: BuffersInCommon -> CL ()
releaseBuffersInCommon bic =
    do  releaseBuffer "bicGeometryHeap " $ bic ^. bicGeometryHeap
        releaseBuffer "bicFacetHeap    " $ bic ^. bicFacetHeap
        releaseBuffer "bicItemTagHeap  " $ bic ^. bicItemTagHeap
        releaseBuffer "bicSubTagHeap   " $ bic ^. bicSubTagHeap
        releaseBuffer "bicDescriptions " $ bic ^. bicDescriptions
        releaseBuffer "bicPictHeap     " $ bic ^. bicPictHeap
        releaseBuffer "bicRandoms      " $ bic ^. bicRandoms
        return ()

newtype BlockId = BlockId {unBlockId :: Int} deriving (Eq, Ord, Num)
newtype BlockPtr = BlockPtr {unBlockPtr :: Int} deriving (Eq, Ord, Num)

instance Show BlockId where
  show (BlockId id) = show id

instance Show BlockPtr where
  show (BlockPtr id) = show id

data BlockSection = BlockSection
  { _sectTileBuffer         :: CLBuffer Tile
  , _sectThresholdBuffer    :: CLBuffer THRESHOLDTYPE
  , _sectThresholdTagBuffer :: CLBuffer HEADERTYPE
  , _sectQueueSliceBuffer   :: CLBuffer (Slice Int)
  , _sectBlockIdBuffer      :: CLBuffer BlockId
  , _sectNumToRender        :: Int
  , _sectNumActive          :: Int
  , _sectFirstTile          :: Tile
  , _sectLastTile           :: Tile
  , _sectActiveFlagBuffer   :: CLBuffer CBool
  }
makeLenses ''BlockSection

v4ToBox (V4 a b c d) = makeBox (fromIntegral a)
                               (fromIntegral b)
                               (fromIntegral c)
                               (fromIntegral d)

nullTile = Tile $ v4ToBox nULLtILE

createBlockSection :: RasterParams token
                   -> CL BlockSection
createBlockSection params =
  do let blocksToAlloc   = params ^. rpRasterState . rasterDeviceSpec . specBlocksPerSection
         columnsPerBlock = params ^. rpRasterState . rasterDeviceSpec . specColumnsPerBlock
         maxThresholds   = params ^. rpRasterState . rasterDeviceSpec . specMaxThresholds
         blockSize       = blocksToAlloc * columnsPerBlock * maxThresholds
     --liftIO $ putStrLn "createBlockSection"
     context <- clContext <$> ask
     tileBuffer       <- newBuffer "tileBuffer      "  blocksToAlloc :: CL (CLBuffer Tile)
     thresholdBuffer  <- newBuffer "thresholdBuffer "  blockSize :: CL (CLBuffer THRESHOLDTYPE)
     headerBuffer     <- newBuffer "headerBuffer    "  blockSize :: CL (CLBuffer HEADERTYPE   )
     queueSliceBuffer <- newBuffer "queueSliceBuffer"  (blocksToAlloc * columnsPerBlock) :: CL (CLBuffer (Slice Int))
     let blockIdVector = VS.generate blocksToAlloc BlockId
     blockIdBuffer    <- bufferFromVector "blockIdBuffer" $ blockIdVector :: CL (CLBuffer BlockId)
     let activeFlagVector = VS.replicate blocksToAlloc . toCBool $ False
     activeFlagBuffer <- bufferFromVector "activeFlagVector" $ activeFlagVector :: CL (CLBuffer CBool)
     return $ BlockSection
              { _sectTileBuffer         = tileBuffer
              , _sectThresholdBuffer    = thresholdBuffer
              , _sectThresholdTagBuffer = headerBuffer
              , _sectQueueSliceBuffer   = queueSliceBuffer
              , _sectBlockIdBuffer      = blockIdBuffer
              , _sectNumToRender        = 0
              , _sectActiveFlagBuffer   = activeFlagBuffer
              , _sectNumActive          = 0
              , _sectFirstTile          = nullTile
              , _sectLastTile           = nullTile
              }

releaseBlockSection :: BlockSection -> CL ()
releaseBlockSection blockSection =
  do releaseBuffer "sectTileBuffer         " $ blockSection ^. sectTileBuffer
     releaseBuffer "sectThresholdBuffer    " $ blockSection ^. sectThresholdBuffer
     releaseBuffer "sectThresholdTagBuffer " $ blockSection ^. sectThresholdTagBuffer
     releaseBuffer "sectQueueSliceBuffer   " $ blockSection ^. sectQueueSliceBuffer
     releaseBuffer "sectBlockIdBuffer      " $ blockSection ^. sectBlockIdBuffer
     releaseBuffer "sectActiveFlagBuffer   " $ blockSection ^. sectActiveFlagBuffer
     return ()

data PointQuery = PointQuery
    { pqTileId :: TileId
    , pqQueryId :: PointQueryId
    , pqLocation :: (Point2 SubSpace)
    } deriving (Show)

instance StorableM PointQuery where
    sizeOfM _ = do sizeOfM (undefined :: Point2 SubSpace)
                   sizeOfM (undefined :: TileId)
                   sizeOfM (undefined :: PointQueryId)
                   sizeOfM (undefined :: CInt) -- filler

    alignmentM _ = do alignmentM (undefined :: Point2 SubSpace)
                      alignmentM (undefined :: TileId)
                      alignmentM (undefined :: PointQueryId)
                      alignmentM (undefined :: CInt) -- filler
    peekM = do location     <- peekM
               tileId       <- peekM
               pointQueryId <- peekM
               return (PointQuery tileId pointQueryId location)
    pokeM (PointQuery tileId pointQueryId location) =
            do pokeM location
               pokeM tileId
               pokeM pointQueryId

instance Storable PointQuery where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance Storable BlockId where
    sizeOf (BlockId a) = sizeOf (undefined :: CInt)
    alignment (BlockId a) = alignment (undefined :: CInt)
    peek i = BlockId . fromCInt <$> peek (castPtr i)
    poke i (BlockId a) = poke (castPtr i) . toCInt $ a
