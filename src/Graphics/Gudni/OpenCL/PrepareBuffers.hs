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

module Graphics.Gudni.OpenCL.PrepareBuffers
  ( BuffersInCommon(..)
  , bicGeoBuffer
  , bicGeoRefBuffer
  , bicPictMemBuffer
  , bicPictFacets
  , bicItemTagBuffer
  , bicSubTagBuffer
  , bicSolidColors
  , bicPictBuffer
  , bicRandoms

  , newBuffer
  , createBuffersInCommon

  , ThresholdBuffers(..)
  , tbThresholdBuffer
  , tbHeaderBuffer
  , tbQueueSliceBuffer

  , createThresholdBuffers
  , releaseThresholdBuffers

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

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.Util.StorableM

import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.OpenCL.Instances
import Graphics.Gudni.Interface.Query


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
import Control.DeepSeq

import Data.Word
import qualified Data.Map as M
import GHC.Exts

data BuffersInCommon = BuffersInCommon
  { _bicGeoBuffer       :: CLBuffer CChar
  , _bicGeoRefBuffer    :: CLBuffer GeoReference
  , _bicPictMemBuffer   :: CLBuffer PictureMemoryReference
  , _bicPictFacets      :: CLBuffer (HardFacet_ SubSpace TextureSpace)
  , _bicItemTagBuffer   :: CLBuffer ItemTag
  , _bicSubTagBuffer    :: CLBuffer SubstanceTag
  , _bicSolidColors     :: CLBuffer Color
  , _bicPictBuffer      :: CLBuffer Word8
  , _bicRandoms         :: CLBuffer CFloat
  }
makeLenses ''BuffersInCommon

createBuffersInCommon :: RasterParams token -> CLContext -> CL BuffersInCommon
createBuffersInCommon params context = liftIO $
    do geoBuffer      <- pileToBuffer context (params ^. rpGeometryState  . geoGeometryPile   )
       geoRefBuffer   <- pileToBuffer context (params ^. rpGeometryState  . geoRefPile        )
       pictMemBuffer  <- pileToBuffer context (params ^. rpSubstanceState . suPictureMems     )
       facetBuffer    <- pileToBuffer context (params ^. rpSubstanceState . suFacetPile       )
       itemTagBuffer  <- pileToBuffer context (params ^. rpSubstanceState . suItemTagPile     )
       subTagBuffer   <- pileToBuffer context (params ^. rpSubstanceState . suSubstanceTagPile)
       colorBuffer    <- pileToBuffer context (params ^. rpSubstanceState . suSolidColorPile  )
       pictDataBuffer <- pileToBuffer context (params ^. rpPictDataPile)
       randoms        <- vectorToBuffer context (params ^. rpGeometryState  . geoRandomField)
       return $  BuffersInCommon
                 { _bicGeoBuffer     = geoBuffer
                 , _bicGeoRefBuffer  = geoRefBuffer
                 , _bicPictMemBuffer = pictMemBuffer
                 , _bicPictFacets    = facetBuffer
                 , _bicItemTagBuffer = itemTagBuffer
                 , _bicSubTagBuffer  = subTagBuffer
                 , _bicSolidColors   = colorBuffer
                 , _bicPictBuffer    = pictDataBuffer
                 , _bicRandoms       = randoms
                 }

data ThresholdBuffers = ThresholdBuffers
  { _tbThresholdBuffer  :: CLBuffer THRESHOLDTYPE
  , _tbHeaderBuffer     :: CLBuffer HEADERTYPE
  , _tbQueueSliceBuffer :: CLBuffer (Slice Int)
  }
makeLenses ''ThresholdBuffers

newBuffer :: Storable a => Int -> CL (CLBuffer a)
newBuffer size = allocBuffer [CL_MEM_READ_WRITE] $ tr "size" (max 1 size)

createThresholdBuffers :: RasterParams token
                       -> Int
                       -> CL ThresholdBuffers
createThresholdBuffers params blocksToAlloc =
  do let threadsPerBlock = params ^. rpRasterizer . rasterDeviceSpec . specThreadsPerBlock
         maxThresholds   = params ^. rpRasterizer . rasterDeviceSpec . specMaxThresholds
         blockSize       = blocksToAlloc * threadsPerBlock * maxThresholds
     thresholdBuffer  <- newBuffer blockSize :: CL (CLBuffer THRESHOLDTYPE)
     headerBuffer     <- newBuffer blockSize :: CL (CLBuffer HEADERTYPE   )
     queueSliceBuffer <- newBuffer (blocksToAlloc * threadsPerBlock) :: CL (CLBuffer (Slice Int))
     return $ ThresholdBuffers
         { _tbThresholdBuffer  = thresholdBuffer
         , _tbHeaderBuffer     = headerBuffer
         , _tbQueueSliceBuffer = queueSliceBuffer
         }

releaseThresholdBuffers :: ThresholdBuffers -> CL ()
releaseThresholdBuffers thresholdBuffers =
  liftIO $ do clReleaseMemObject . bufferObject $ thresholdBuffers ^. tbThresholdBuffer
              clReleaseMemObject . bufferObject $ thresholdBuffers ^. tbHeaderBuffer
              clReleaseMemObject . bufferObject $ thresholdBuffers ^. tbQueueSliceBuffer
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
