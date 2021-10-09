{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Class
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Type class of layouts

module Graphics.Gudni.Layout.Class
  ( IsLayout(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Empty
import Graphics.Gudni.Layout.Alignment
import Graphics.Gudni.Layout.Style

class (HasEmpty t, HasStyle t, HasDefault (Meld t)) => IsLayout t where
    type Meld t :: *
    place   :: Shape (SpaceOf t) -> t
    nextTo  :: ToEitherAxis axis => axis -> StyleOf t -> Maybe Alignment -> Meld t -> t -> t -> t
    onTopOf :: StyleOf t -> Maybe Alignment -> Maybe Alignment -> Meld t -> t -> t -> t
