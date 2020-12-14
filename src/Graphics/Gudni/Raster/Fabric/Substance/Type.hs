{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}
--
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Substance.Type
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A Substance is basically one of stock ways that a shape or area can be filled.
-- This includes textures that are access to an image.

module Graphics.Gudni.Raster.Fabric.Substance.Type
  ( SubstanceType(..)
  , FSubstance(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle

class HasSpace i => SubstanceType i where
    type FTex   i :: *
    type FQuery i :: *

data FSubstance i where
     FConst     :: FQuery i    -> FSubstance i
     FTexture   :: FTex   i    -> FSubstance i
     FLinear    ::                FSubstance i
     FQuadrance ::                FSubstance i
     deriving (Generic)

instance HasSpace i => HasSpace (FSubstance i) where
    type SpaceOf (FSubstance i) = SpaceOf i

deriving instance (Show (FQuery i), Show (FTex i)) => Show (FSubstance i)

instance (Out (FTex i), Out (FQuery i)) => Out (FSubstance i)
