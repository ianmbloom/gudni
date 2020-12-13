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
-- Module      :  Graphics.Gudni.Serial.Reference
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A pile is an appendable buffer of data that reallocates memory as it grows∘
-- Piles can be reused without reallocating memory.

module Graphics.Gudni.Raster.Serial.Reference
  ( Reference (..)
  , Reference_ (..)
  , nullReference
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Raster.Constants

import Foreign.Storable
import Foreign.C.Types(CUInt)
import Foreign.Ptr

type Reference_ = CUInt
instance Out Reference_ where
    doc x = text . show $ x
    docPrec _ = doc

nullReference = Ref nULLrEFERENCE

newtype Reference t = Ref {unRef :: Reference_}    deriving (Eq, Ord, Num, Enum, Real, Integral, Generic)

instance Show (Reference t) where
  show (Ref i) = show i

instance Out (Reference t)

instance Storable (Reference t) where
  sizeOf    _ = sizeOf    (undefined :: Reference_)
  alignment _ = alignment (undefined :: Reference_)
  poke ptr (Ref i) = poke (castPtr ptr) i
  peek ptr = Ref <$> peek (castPtr ptr)
