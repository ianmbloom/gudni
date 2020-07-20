{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Adjacent
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Top level functions for creating bounding box based layouts.

module Graphics.Gudni.Layout.Adjacent
  ( Overlappable (..)
  , rackOf
  , rack
  , stackOf
  , stack
  , nextTo
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Layout
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Empty
import Graphics.Gudni.Layout.Overlappable
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Util.Debug
import Linear
import Data.List
import Data.Char
import Data.Maybe

import Control.Lens
import Control.Applicative
import Control.Monad.State

nextTo :: ( HasSpace leaf
          , Axis axis
          , SwitchAxis axis
          , IsStyle style
          )
       => axis
       -> style
       -> Maybe Alignment
       -> meld
       -> STree (AdjacentMeld style meld) leaf
       -> STree (AdjacentMeld style meld) leaf
       -> STree (AdjacentMeld style meld) leaf
nextTo axis style alignment meld = SMeld (AdjacentMeld style (NextTo (eitherAxis axis) alignment) meld)

loaf :: ( HasSpace leaf
        , Axis axis
        , SwitchAxis axis
        , IsStyle style)
        => axis
        -> style
        -> Maybe Alignment
        -> meld
        -> [STree (AdjacentMeld style meld) leaf]
        -> STree (AdjacentMeld style meld) leaf
loaf axis style alignment meld =
  foldl (nextTo axis style alignment meld) emptyItem

rackOf :: ( HasSpace leaf
          , IsStyle style
          )
          => style
          -> Maybe Alignment
          -> meld
          -> [STree (AdjacentMeld style meld) leaf]
          -> STree (AdjacentMeld style meld) leaf
rackOf = loaf Horizontal

rack :: ( HasSpace leaf
        , IsStyle style
        , HasDefault meld
        )
        => [STree (AdjacentMeld style meld) leaf]
        -> STree (AdjacentMeld style meld) leaf
rack = rackOf defaultValue Nothing defaultValue

stackOf ::( HasSpace leaf
          , IsStyle style)
          => style
          -> Maybe Alignment
          -> meld
          -> [STree (AdjacentMeld style meld) leaf]
          -> STree (AdjacentMeld style meld) leaf
stackOf = loaf Vertical

stack :: ( HasSpace leaf
         , IsStyle style
         , HasDefault meld
         )
         => [STree (AdjacentMeld style meld) leaf]
         -> STree (AdjacentMeld style meld) leaf
stack = stackOf defaultValue Nothing defaultValue
