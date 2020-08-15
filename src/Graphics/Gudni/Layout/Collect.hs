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
-- Module      :  Graphics.Gudni.Layout.Collect
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Top level functions for creating bounding box based layouts.

module Graphics.Gudni.Layout.Collect
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
import Graphics.Gudni.Layout.Alignment
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

loaf :: ( IsLayout layout
        , Axis axis
        , SwitchAxis axis
        )
        => axis
        -> StyleOf layout
        -> Maybe Alignment
        -> Meld layout
        -> [layout]
        -> layout
loaf axis style alignment meld =
  foldl (nextTo axis style alignment meld) emptyItem

rackOf :: ( IsLayout layout
          )
          => StyleOf layout
          -> Maybe Alignment
          -> Meld layout
          -> [layout]
          -> layout
rackOf = loaf Horizontal

rack :: ( IsLayout layout
        , HasDefault (StyleOf layout)
        , HasDefault (Meld layout)
        )
        => [layout]
        -> layout
rack = rackOf defaultValue Nothing defaultValue

stackOf ::( IsLayout layout
          )
          => StyleOf layout
          -> Maybe Alignment
          -> Meld layout
          -> [layout]
          -> layout
stackOf = loaf Vertical

stack :: ( IsLayout layout
         , HasDefault (Meld layout)
         , HasDefault (StyleOf layout)
         )
         => [layout]
         -> layout
stack = stackOf defaultValue Nothing defaultValue
