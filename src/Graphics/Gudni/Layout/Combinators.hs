
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Combinators
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Type class of layouts

module Graphics.Gudni.Layout.Combinators
  ( constantLayer       --
  , textureLayer        --
  , linearLayer         --
  , quadranceLayer      --
  , sqrtOf              --
  , inverseOf           --
  , cosOf               --
  , sinOf               --
  , clampOf             --
  , compositeWith       --
  , maskWith            --
  , withBackgroundColor --
  , addOver             --
  , subtractFrom        --
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Combine.Type
import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.Fabric.FromLayout

import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Empty
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Class
import Graphics.Gudni.Layout.Type
import Graphics.Gudni.Raster.TextureReference

constantLayer :: Color (SpaceOf style) -> Layout Rgba style
constantLayer = Layout . FLeaf . ShapeLeaf . FSubstance . FConst

textureLayer :: NamedTexture -> Layout Rgba style
textureLayer = Layout . FLeaf . ShapeLeaf . FSubstance . FTexture

linearLayer :: Layout Mono style
linearLayer = Layout . FLeaf . ShapeLeaf . FSubstance $ FLinear

quadranceLayer :: Layout Mono style
quadranceLayer = Layout . FLeaf . ShapeLeaf . FSubstance $ FQuadrance

addFilter :: FFilter -> Layout x style -> Layout x style
addFilter filt (Layout child) = Layout . FUnaryPost filt $ child
addFilter filt (EmptyLayout)  = EmptyLayout

sqrtOf    :: Layout x style -> Layout x style
inverseOf :: Layout x style -> Layout x style
cosOf     :: Layout x style -> Layout x style
sinOf     :: Layout x style -> Layout x style
clampOf   :: Layout x style -> Layout x style
sqrtOf    = addFilter FSqrt
inverseOf = addFilter FInvert
cosOf     = addFilter FCos
sinOf     = addFilter FSin
clampOf   = addFilter FClamp

compositeWith :: (IsStyle style) => Layout Rgba style -> Layout Rgba style -> Layout Rgba style
compositeWith (Layout a) (Layout b) = Layout $ FBinary (ProximityMeld noProximity defaultValue FComposite) a b

maskWith :: (IsStyle style) => Layout Mono style -> Layout x style -> Layout x style
maskWith (Layout mask) (Layout item) = Layout $ FBinary (ProximityMeld noProximity defaultValue FMask) mask item

withBackgroundColor :: (IsStyle style) => Color (SpaceOf style) -> Layout Rgba style -> Layout Rgba style
withBackgroundColor color layout = compositeWith layout (constantLayer color)

addOver :: (IsStyle style) => Layout Mono style -> Layout Mono style -> Layout Mono style
addOver      a b = layoutEither (FBinary (ProximityMeld noProximity defaultValue FFloatOr)) a b
subtractFrom :: (IsStyle style) => Layout Mono style -> Layout Mono style -> Layout Mono style
subtractFrom a b = layoutEither (FBinary (ProximityMeld noProximity defaultValue FMask   )) (overLayout (FUnaryPost FInvert) a) b
