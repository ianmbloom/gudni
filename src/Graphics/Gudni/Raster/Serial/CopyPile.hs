{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Util.Pile
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A pile is an appendable buffer of data that reallocates memory as it grows∘
-- Piles can be reused without reallocating memory.

module Graphics.Gudni.Raster.Serial.CopyPile
  ( CanCopyPile (..)
  )
where

import Graphics.Gudni.Util.Debug

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import qualified Data.Vector.Storable as VS

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Utils(copyBytes)


import GHC.Ptr

class CanCopyPile a b where
  copyIntoPile :: MonadIO m => Pile a -> b -> m (Pile a, Slice a)

instance (Storable a) => CanCopyPile a (VS.Vector a) where
    -- | Add a vector of items to a pile. Return a reference to the beggining.
    copyIntoPile destination vector =
      liftIO $ VS.unsafeWith vector $ \sourcePtr ->
      let sourceSize =  VS.length vector
      in  copyFromPtrIntoPile destination sourcePtr sourceSize

instance (Storable a) => CanCopyPile a (Pile a) where
    -- | Copy the contents of a pile into another pile.
    copyIntoPile destination source =
       let sourcePtr = _pileData source
           sourceSize = _pileCursor source
       in  copyFromPtrIntoPile destination sourcePtr (fromIntegral sourceSize)

copyFromPtrIntoPile :: forall t a m . (Storable t, MonadIO m) => Pile t -> Ptr a -> Int -> m (Pile t, Slice t)
copyFromPtrIntoPile destination@(Pile destCursor destAllocated startPtr) sourcePtr sourceSize =
    let
        sourceSizeInBytes = sourceSize * sizeOf (undefined :: t)
        cursorPtr = startPtr `plusPtr` (fromIntegral destCursor * sizeOf (undefined :: t))
        end = destCursor + fromIntegral sourceSize
    in
    if end < destAllocated
    then
      do liftIO $ copyBytes cursorPtr sourcePtr sourceSizeInBytes
         return (destination {_pileCursor = destCursor + fromIntegral sourceSize}, Slice (Ref $ fromIntegral destCursor) (Ref $ fromIntegral sourceSize))
    else
      do e <- extendPile destination
         copyFromPtrIntoPile e sourcePtr sourceSize
