{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Compound
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic functions for constructing drawings.

module Graphics.Gudni.Layout.Compound
  ( Compoundable(..)
  )
where

import Graphics.Gudni.Figure

-- | Typeclass of shape representations that can be combined with other shapes.
class Compoundable a where
  addOver      :: a -> a -> a
  subtractFrom :: a -> a -> a

-- | Instance for combining simple compound shapes.
instance Compoundable (STree (CompoundTree_)) where
  addOver      = SMeld CompoundAdd
  subtractFrom = SMeld CompoundSubtract -- the subtracted shape must be above what is being subtracted in the stack.
