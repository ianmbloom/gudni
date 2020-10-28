{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Alignment
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Ways of alignment.

module Graphics.Gudni.Layout.Alignment
  ( Alignment(..)
  )
where

import Graphics.Gudni.Base

data Alignment
    = AlignMin
    | AlignMax
    | AlignCenter
    deriving (Show, Generic)

instance Out Alignment
