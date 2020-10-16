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
  , combineSlices
  , mapSliceM
  )
where

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Serial.Reference

import Control.Lens
import Control.Loop

import Foreign.Storable
import Foreign.C.Types

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

mapSliceM :: Monad m => (Reference i -> m ()) -> Slice i -> m ()
mapSliceM body slice = numLoop (sliceStart slice) (sliceStart slice + sliceLength slice) body

instance Show (Slice t) where
  show (Slice (Ref a) (Ref b)) = "(" ++ show a ++ "," ++ show b ++ ")"

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
