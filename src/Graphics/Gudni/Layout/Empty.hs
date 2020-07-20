-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Empty
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A typeclass for objects that have an empty representation.

module Graphics.Gudni.Layout.Empty
  ( HasEmpty(..)
  )
where

import Graphics.Gudni.Figure

class HasEmpty a where
  emptyItem :: a
  isEmpty   :: a -> Bool
