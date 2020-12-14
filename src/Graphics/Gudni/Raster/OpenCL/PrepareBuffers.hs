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
-- Module      :  Graphics.Gudni.Raster.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for preparing buffers and calling the rasterizer kernel∘

module Graphics.Gudni.Raster.OpenCL.PrepareBuffers
  ( BuffersInCommon(..)
  , bicPrimBezierHeap
  , bicPrimFacetHeap
  , bicPrimBoxHeap
  , bicPrimTagHeap
  , bicFabricTagHeap
  , bicFabricHeap
  , bicTreeConfineHeap
  , bicTreeDecoHeap
  , bicCrossingPile
  , bicPictHeap

  , withBuffersInCommon
  , createBuffersInCommon
  , releaseBuffersInCommon
  , withOutputBuffer
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.ConfineTree.Primitive.Storage
import Graphics.Gudni.Raster.Fabric.Storage
import Graphics.Gudni.Raster.Storage
import Graphics.Gudni.Raster.State
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.OpenCL.Util.DeviceQuery
import Graphics.Gudni.Raster.OpenCL.Util.KernelArg
import Graphics.Gudni.Raster.OpenCL.Util.Buffer

import Graphics.Gudni.Raster.OpenCL.Rasterizer

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Debug
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

data BuffersInCommon s =
    BuffersInCommon
    { _bicPrimBezierHeap   :: CLBuffer (Bezier s)
    , _bicPrimFacetHeap    :: CLBuffer (Facet  s)
    , _bicPrimBoxHeap      :: CLBuffer (Box    s)
    , _bicPrimTagHeap      :: CLBuffer PrimTag
    , _bicFabricTagHeap    :: CLBuffer FabricTag
    , _bicFabricHeap       :: CLBuffer CChar
    , _bicTreeConfineHeap  :: CLBuffer (ConfineTag s)
    , _bicTreeDecoHeap     :: CLBuffer (DecoTag s)
    , _bicCrossingPile     :: CLBuffer FabricTagId
    , _bicPictHeap         :: CLBuffer CFloat
    }
makeLenses ''BuffersInCommon

withBuffersInCommon :: (Storable s, Space s)
                    => DagStorage s
                    -> (BuffersInCommon s -> CL a)
                    -> CL a
withBuffersInCommon storage code =
   do  bic <- createBuffersInCommon storage
       result <- code bic
       releaseBuffersInCommon bic
       return result

createBuffersInCommon :: (Storable s, Space s)
                      => DagStorage s
                      -> CL (BuffersInCommon s)
createBuffersInCommon storage =
    do  primBezierHeap   <- bufferFromPile   "primBezierHeap  " (storage ^. dagTreeStorage . treePrimStorage . primBezierPile  )
        primFacetHeap    <- bufferFromPile   "primFacetHeap   " (storage ^. dagTreeStorage . treePrimStorage . primFacetPile   )
        primBoxHeap      <- bufferFromPile   "primBoxHeap     " (storage ^. dagTreeStorage . treePrimStorage . primBoxPile     )
        primTagHeap      <- bufferFromPile   "primTagHeap     " (storage ^. dagTreeStorage . treePrimStorage . primTagPile     )
        fabricTagHeap    <- bufferFromPile   "fabricTagHeap   " (storage ^. dagFabricStorage . fabricTagPile   )
        fabricHeap       <- bufferFromPile   "fabricHeap      " (storage ^. dagFabricStorage . fabricHeapPile  )
        treeConfineHeap  <- bufferFromPile   "treeConfineHeap " (storage ^. dagTreeStorage   . treeConfinePile )
        treeDecoHeap     <- bufferFromPile   "treeDecoHeap    " (storage ^. dagTreeStorage   . treeDecoPile    )
        treeCrossingPile <- bufferFromPile   "treeCrossingPile" (storage ^. dagTreeStorage   . treeCrossingPile)
        pictHeap         <- bufferFromPile   "pictHeap        " (storage ^. dagPixelPile                       )
        return $  BuffersInCommon
                  { _bicPrimBezierHeap   = primBezierHeap
                  , _bicPrimFacetHeap    = primFacetHeap
                  , _bicPrimBoxHeap      = primBoxHeap
                  , _bicPrimTagHeap      = primTagHeap
                  , _bicFabricTagHeap    = fabricTagHeap
                  , _bicFabricHeap       = fabricHeap
                  , _bicTreeConfineHeap  = treeConfineHeap
                  , _bicTreeDecoHeap     = treeDecoHeap
                  , _bicCrossingPile     = treeCrossingPile
                  , _bicPictHeap         = pictHeap
                  }

releaseBuffersInCommon :: (Storable s)
                       => BuffersInCommon s
                       -> CL ()
releaseBuffersInCommon bic =
    do  releaseBuffer "bicPrimBezierHeap  " ( bic ^. bicPrimBezierHeap   )
        releaseBuffer "bicPrimFacetHeap   " ( bic ^. bicPrimFacetHeap    )
        releaseBuffer "bicPrimBoxHeap     " ( bic ^. bicPrimBoxHeap      )
        releaseBuffer "bicPrimTagHeap     " ( bic ^. bicPrimTagHeap      )
        releaseBuffer "bicFabricTagHeap   " ( bic ^. bicFabricTagHeap    )
        releaseBuffer "bicFabricHeap      " ( bic ^. bicFabricHeap       )
        releaseBuffer "bicTreeConfineHeap " ( bic ^. bicTreeConfineHeap  )
        releaseBuffer "bicTreeDecoHeap    " ( bic ^. bicTreeDecoHeap     )
        releaseBuffer "bicCrossingPile    " ( bic ^. bicCrossingPile     )
        releaseBuffer "bicPictHeap        " ( bic ^. bicPictHeap         )
        return ()

readBufferToPtr :: forall a . Storable a => Ptr a -> Int -> CLBuffer a -> CL ()
readBufferToPtr ptr size buffer =
  do let m = sizeOf (undefined :: a)
     queue <- clQueue <$> ask
     liftIO $ do ev <- clEnqueueReadBuffer queue (bufferObject buffer) False 0 (m*size) (castPtr ptr) []
                 _  <- clWaitForEvents [ev]
                 void $ clReleaseEvent ev
     return ()

withOutputBuffer :: Point2 PixelSpace
                 -> DrawTarget
                 -> (CLBuffer CUInt -> CL ())
                 -> CL ()
withOutputBuffer canvasSize target code =
                   case target ^. targetBuffer of
                       HostBitmapTarget outputPtr ->
                           let outputSize   = fromIntegral $ pointArea canvasSize
                           in  do -- In this case the resulting bitmap will be stored in memory at outputPtr.
                                  outputBuffer <- allocBuffer [CL_MEM_WRITE_ONLY] outputSize
                                  code outputBuffer
                                  readBufferToPtr outputPtr outputSize outputBuffer
                                  releaseBuffer "outputBuffer" outputBuffer
                                  return ()
                       GLTextureTarget textureName ->
                           -- In this case an identifier for a Texture object that stays on the GPU would be stored∘
                           -- But currently this isn't working, so throw an error.
                           error "GLTextureTarget not implemented"
