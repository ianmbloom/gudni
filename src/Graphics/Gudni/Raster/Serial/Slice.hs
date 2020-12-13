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

module Graphics.Gudni.Raster.Serial.Slice
  ( Slice (..)
  , emptySlice
  , combineSlices
  , mapSliceM_
  , mapSliceM
  )
where

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Raster.Serial.Reference

import Control.Lens
import Control.Loop

import Foreign.Storable

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import GHC.Ptr

combineSlices :: Slice a -> Slice a -> Slice a
combineSlices a b = Slice (sliceStart a) (sliceLength a + sliceLength b)

-- | A slice defines a range in an array with the starting point and the length.
data Slice t = Slice
  { sliceStart   :: !(Reference t)
  , sliceLength  :: !(Reference t)
  } deriving (Eq)

emptySlice :: Slice t
emptySlice = Slice 0 0

mapSliceM_ :: Monad m => (Reference i -> m ()) -> Slice i -> m ()
mapSliceM_ body slice = let len = sliceLength slice
                        in if len > 0
                           then numLoop (sliceStart slice) (sliceStart slice + sliceLength slice - 1) body
                           else return ()

mapSliceM :: Monad m => (Reference i -> m a) -> Slice i -> m [a]
mapSliceM body slice = let end = sliceStart slice + sliceLength slice - 1
                           len = sliceLength slice
                       in  if len > 0
                           then  numLoopState 0 (len - 1) [] $
                                   \l i -> do a <- body (end - i)
                                              return (a:l)
                           else return []

instance Show (Slice t) where
  show (Slice (Ref a) (Ref b)) = "(" ++ show a ++ "," ++ show b ++ ")"

instance Out (Slice t) where
      doc slice = text (show slice)
      docPrec _ = doc

instance StorableM (Slice t) where
  sizeOfM _ = do sizeOfM (undefined :: Reference t)
                 sizeOfM (undefined :: Reference t)
  alignmentM _ = do alignmentM (undefined :: Reference t)
                    alignmentM (undefined :: Reference t)
  peekM =
    do start <- peekM
       len   <- peekM
       return (Slice start len)
  pokeM (Slice start len) =
     do pokeM start
        pokeM len

instance Storable (Slice t) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
