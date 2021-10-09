
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

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

module Graphics.Gudni.Layout.Type
  ( Rgba(..)
  , Hsv(..)
  , Mono(..)
  , Layout(..)
  , layoutEither
  , layoutBoth
  , overLayout
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Fabric.Type
import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Combine.Type
import Graphics.Gudni.Raster.Fabric.FromLayout
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Empty
import Graphics.Gudni.Layout.Token
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Class

data Rgba
data Hsv
data Mono

type BaseFabric style = Fabric (LayoutStart String style)

data Layout channels style
    = Layout {unLayout :: BaseFabric style}
    | EmptyLayout

instance HasEmpty (Layout channels style) where
    isEmpty EmptyLayout = True
    isEmpty _           = False
    emptyItem = EmptyLayout

instance HasSpace style => HasSpace (Layout channels style) where
    type SpaceOf (Layout channels style) = SpaceOf style

instance IsStyle style => HasStyle (Layout channels style) where
    type StyleOf (Layout channels style) = style

instance HasToken style => HasToken (Layout x style) where
    type TokenOf (Layout x style) = TokenOf style

instance IsStyle style => IsLayout (Layout Rgba style) where
    type Meld (Layout Rgba style) = FCombineType
    place shape = Layout . FLeaf . ShapeLeaf . FShape $ WithBox shape (boxOf shape)
    nextTo  axis style mAlign          meld = layoutEither . FBinary $ ProximityMeld (NextTo (eitherAxis axis) mAlign ) style meld
    onTopOf      style mAlignV mAlignH meld = layoutEither . FBinary $ ProximityMeld (OnTopOf mAlignV mAlignH         ) style meld

instance IsStyle style => IsLayout (Layout Mono style) where
    type Meld (Layout Mono style) = FCombineType
    place shape = Layout . FLeaf . ShapeLeaf . FShape $ WithBox shape (boxOf shape)
    nextTo  axis style mAlign          meld = layoutEither . FBinary $ ProximityMeld (NextTo (eitherAxis axis) mAlign ) style meld
    onTopOf      style mAlignV mAlignH meld = layoutEither . FBinary $ ProximityMeld (OnTopOf mAlignV mAlignH         ) style meld

instance IsStyle style => Transformable (Layout x style) where
    translateBy p (Layout fabric) = Layout $ translateBy p fabric
    stretchBy   v (Layout fabric) = Layout $ stretchBy   v fabric
    rotateBy    a (Layout fabric) = Layout $ rotateBy    a fabric

instance IsStyle style => Projectable (Layout x style) where
    projectOnto path (Layout fabric) = Layout $ projectOnto path fabric

layoutEither :: (BaseFabric style -> BaseFabric style -> BaseFabric style) -> Layout a style -> Layout b style -> Layout c style
layoutEither f (Layout a)  (Layout b)  = Layout $ f a b
layoutEither f (Layout a)  EmptyLayout = Layout a
layoutEither f EmptyLayout (Layout b)  = Layout b
layoutEither f EmptyLayout EmptyLayout = EmptyLayout

layoutBoth :: (BaseFabric style -> BaseFabric style -> BaseFabric style) -> Layout a style -> Layout b style -> Layout c style
layoutBoth f (Layout a)  (Layout b)  = Layout $ f a b
layoutBoth f (Layout a)  EmptyLayout = EmptyLayout
layoutBoth f EmptyLayout (Layout b)  = EmptyLayout
layoutBoth f EmptyLayout EmptyLayout = EmptyLayout

overLayout :: (BaseFabric style -> BaseFabric style) -> Layout a style -> Layout b style
overLayout f (Layout a) = Layout $ f a
