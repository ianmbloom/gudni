{-# LANGUAGE GADTs         #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Filter.Type
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Unary post filters that can be applied to answers, usually to Colors

module Graphics.Gudni.Raster.Fabric.Filter.Type
  ( FFilter(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Substance

data FFilter where
     FSqrt     :: FFilter
     FInvert   :: FFilter -- (1-x)
     FCos      :: FFilter
     FSin      :: FFilter
     FClamp    :: FFilter
     deriving (Enum, Show, Generic)

instance Out FFilter
