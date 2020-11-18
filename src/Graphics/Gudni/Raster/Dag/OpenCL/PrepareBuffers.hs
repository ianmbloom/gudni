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
-- Module      :  Graphics.Gudni.Raster.Dag.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for preparing buffers and calling the rasterizer kernel∘

module Graphics.Gudni.Raster.Dag.OpenCL.PrepareBuffers
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
  , bicRandomHeap

  , withBuffersInCommon
  , createBuffersInCommon
  , releaseBuffersInCommon
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Interface
import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.Primitive.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Storage
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Thresholds.Constants
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.OpenCL.DeviceQuery
import Graphics.Gudni.Raster.OpenCL.Instances
import Graphics.Gudni.Raster.OpenCL.Buffer

import Graphics.Gudni.Raster.Dag.OpenCL.RasterState

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

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
    , _bicCrossingPile     :: CLBuffer ShapeId
    , _bicPictHeap         :: CLBuffer CFloat
    , _bicRandomHeap       :: CLBuffer CFloat
    }
makeLenses ''BuffersInCommon

withBuffersInCommon :: (Storable s)
                    => DagState token s
                    -> (BuffersInCommon s -> CL a)
                    -> CL a
withBuffersInCommon params code =
   do  bic <- createBuffersInCommon params
       result <- code bic
       releaseBuffersInCommon bic
       return result

createBuffersInCommon :: (Storable s)
                      => DagState token s
                      -> CL (BuffersInCommon s)
createBuffersInCommon state =
    do  primBezierHeap   <- bufferFromPile   "primBezierHeap  " (state ^. dagStorage . dagPrimStorage   . primBezierPile  )
        primFacetHeap    <- bufferFromPile   "primFacetHeap   " (state ^. dagStorage . dagPrimStorage   . primFacetPile   )
        primBoxHeap      <- bufferFromPile   "primBoxHeap     " (state ^. dagStorage . dagPrimStorage   . primBoxPile     )
        primTagHeap      <- bufferFromPile   "primTagHeap     " (state ^. dagStorage . dagPrimStorage   . primTagPile     )
        fabricTagHeap    <- bufferFromPile   "fabricTagHeap   " (state ^. dagStorage . dagFabricStorage . fabricTagPile   )
        fabricHeap       <- bufferFromPile   "fabricHeap      " (state ^. dagStorage . dagFabricStorage . fabricHeapPile  )
        treeConfineHeap  <- bufferFromPile   "treeConfineHeap " (state ^. dagStorage . dagTreeStorage   . treeConfinePile )
        treeDecoHeap     <- bufferFromPile   "treeDecoHeap    " (state ^. dagStorage . dagTreeStorage   . treeDecoPile    )
        treeCrossingPile <- bufferFromPile   "treeCrossingPile" (state ^. dagStorage . dagTreeStorage   . treeCrossingPile)
        pictHeap         <- bufferFromPile   "pictHeap        " (state ^. dagStorage . dagPixelPile                       )
        randomHeap       <- bufferFromVector "randomHeap      " (state ^. dagRasterState . rasterRandomField              )
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
                  , _bicRandomHeap       = randomHeap
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
        releaseBuffer "bicRandomHeap      " ( bic ^. bicRandomHeap       )
        return ()
