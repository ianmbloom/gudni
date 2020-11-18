{-# LANGUAGE GADTs         #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Fabric.Ray.Filter
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A Substance is basically one of stock ways that a shape or area can be filled.
-- This includes textures that are access to an image.

module Graphics.Gudni.Raster.Dag.Fabric.Ray.Filter
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
     deriving (Enum, Show, Generic)

instance Out FFilter
