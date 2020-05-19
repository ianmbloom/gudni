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
-- Module      :  Graphics.Gudni.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for preparing buffers and calling the rasterizer kernel∘

module Graphics.Gudni.OpenCL.PrepareBuffers
  ( BuffersInCommon(..)
  , bicGeometryHeap
  , bicPictMemRefHeap
  , bicFacetHeap
  , bicItemTagHeap
  , bicItemTagIdHeap
  , bicSubTagHeap
  , bicSolidColors
  , bicPictHeap
  , bicRandoms

  , newBuffer
  , releaseBuffer
  , bufferFromPile
  , bufferFromVector
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
import Graphics.Gudni.Raster.Params

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.CTypeConversion

import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.OpenCL.Instances
import Graphics.Gudni.Interface.Query


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
import Control.DeepSeq
import qualified Data.Sequence as S


import Data.Word
import qualified Data.Map as M
import GHC.Exts

data BuffersInCommon = BuffersInCommon
  { _bicGeometryHeap   :: CLBuffer CChar
  , _bicPictMemRefHeap :: CLBuffer PictureMemoryReference
  , _bicFacetHeap      :: CLBuffer (HardFacet_ SubSpace TextureSpace)
  , _bicItemTagHeap    :: CLBuffer ItemTag
  , _bicItemTagIdHeap  :: CLBuffer ItemTagId
  , _bicSubTagHeap     :: CLBuffer SubstanceTag
  , _bicSolidColors    :: CLBuffer Color
  , _bicPictHeap       :: CLBuffer Word8
  , _bicRandoms        :: CLBuffer CFloat
  }
makeLenses ''BuffersInCommon

newBuffer :: (Storable a) => String -> Int -> CL (CLBuffer a)
newBuffer message size =
  do buffer <- allocBuffer [CL_MEM_READ_WRITE] (max 1 size)
     --liftIO $ putStrLn $ "   newBuffer " ++ message ++ " " ++ (show . bufferObject $ buffer)
     return buffer

releaseBuffer :: String -> CLBuffer a -> CL Bool
releaseBuffer message buffer =
  do result <- liftIO $ clReleaseMemObject . bufferObject $ buffer
     --liftIO $ putStrLn $ "releaseBuffer " ++ message ++ " " ++ (show . bufferObject $ buffer)
     return result

bufferFromPile :: (Storable a) => String -> Pile a -> CL (CLBuffer a)
bufferFromPile message pile =
   do context <- clContext <$> ask
      buffer <- liftIO $ pileToBuffer context pile
      --liftIO $ putStrLn $ "  pileBuffer " ++ message ++ " " ++ (show . bufferObject $ buffer)
      return buffer

bufferFromVector :: (Storable a) => String -> VS.Vector a -> CL (CLBuffer a)
bufferFromVector message vector =
   do context <- clContext <$> ask
      buffer <- liftIO $ vectorToBuffer context vector
      --liftIO $ putStrLn $ "vectorBuffer " ++ message ++ " " ++ (show . bufferObject $ buffer)
      return buffer

withBuffersInCommon :: RasterParams token -> Pile ItemTagId -> (BuffersInCommon -> CL a) -> CL a
withBuffersInCommon params itemTagIdPile code =
   do  bic <- createBuffersInCommon params itemTagIdPile
       result <- code bic
       releaseBuffersInCommon bic
       return result

createBuffersInCommon :: RasterParams token -> Pile ItemTagId -> CL BuffersInCommon
createBuffersInCommon params itemTagIdPile =
    do  itemTagIdBuffer <- bufferFromPile   "itemTagIdPile " itemTagIdPile
        geoBuffer       <- bufferFromPile   "geoBuffer     " (params ^. rpSerialState . serGeometryPile    )
        pictMemBuffer   <- bufferFromPile   "pictMemBuffer " (params ^. rpSerialState . serPictureMems     )
        facetBuffer     <- bufferFromPile   "facetBuffer   " (params ^. rpSerialState . serFacetPile       )
        itemTagBuffer   <- bufferFromPile   "itemTagBuffer " (params ^. rpSerialState . serItemTagPile     )
        subTagBuffer    <- bufferFromPile   "subTagBuffer  " (params ^. rpSerialState . serSubstanceTagPile)
        colorBuffer     <- bufferFromPile   "colorBuffer   " (params ^. rpSerialState . serSolidColorPile  )
        pictDataBuffer  <- bufferFromPile   "pictDataBuffer" (params ^. rpPictDataPile)
        randoms         <- bufferFromVector "randoms       " (params ^. rpRasterizer  . rasterRandomField  )
        return $  BuffersInCommon
                  { _bicGeometryHeap   = geoBuffer
                  , _bicPictMemRefHeap = pictMemBuffer
                  , _bicFacetHeap      = facetBuffer
                  , _bicItemTagHeap    = itemTagBuffer
                  , _bicItemTagIdHeap  = itemTagIdBuffer
                  , _bicSubTagHeap     = subTagBuffer
                  , _bicSolidColors    = colorBuffer
                  , _bicPictHeap       = pictDataBuffer
                  , _bicRandoms        = randoms
                  }

releaseBuffersInCommon :: BuffersInCommon -> CL ()
releaseBuffersInCommon bic =
    do
        releaseBuffer "bicGeometryHeap  " $ bic ^. bicGeometryHeap
        releaseBuffer "bicPictMemRefHeap" $ bic ^. bicPictMemRefHeap
        releaseBuffer "bicFacetHeap     " $ bic ^. bicFacetHeap
        releaseBuffer "bicItemTagHeap   " $ bic ^. bicItemTagHeap
        releaseBuffer "bicItemTagIdHeap " $ bic ^. bicItemTagIdHeap
        releaseBuffer "bicSubTagHeap    " $ bic ^. bicSubTagHeap
        releaseBuffer "bicSolidColors   " $ bic ^. bicSolidColors
        releaseBuffer "bicPictHeap      " $ bic ^. bicPictHeap
        releaseBuffer "bicRandoms       " $ bic ^. bicRandoms
        return ()

newtype BlockId = BlockId {unBlockId :: Int} deriving (Eq, Ord, Num)
newtype BlockPtr = BlockPtr {unBlockPtr :: Int} deriving (Eq, Ord, Num)

instance Show BlockId where
  show (BlockId id) = show id

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
  do let blocksToAlloc   = params ^. rpRasterizer . rasterDeviceSpec . specBlocksPerSection
         columnsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specColumnsPerBlock
         maxThresholds   = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
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
  do releaseBuffer "sectTileBuffer      " $ blockSection ^. sectTileBuffer
     releaseBuffer "sectThresholdBuffer " $ blockSection ^. sectThresholdBuffer
     releaseBuffer "sectThresholdTagBuffer    " $ blockSection ^. sectThresholdTagBuffer
     releaseBuffer "sectQueueSliceBuffer" $ blockSection ^. sectQueueSliceBuffer
     releaseBuffer "sectBlockIdBuffer   " $ blockSection ^. sectBlockIdBuffer
     releaseBuffer "sectActiveFlagBuffer" $ blockSection ^. sectActiveFlagBuffer
     return ()

data PointQuery = PointQuery
    { pqTileId :: TileId
    , pqQueryId :: PointQueryId
    , pqLocation :: (Point2 SubSpace)
    } deriving (Show)

instance NFData PointQuery where
    rnf (PointQuery a b c) = a `deepseq` b `deepseq` c `deepseq` ()

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
