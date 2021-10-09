{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}

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

module Graphics.Gudni.Layout.Style
  ( GlyphLeaf(..)
  , HasStyle(..)
  , IsStyle(..)
  , DefaultStyle(..)
  , StyleAxis(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.StorableInstances
import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Raster.Fabric.Transformer.Type
import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.Fabric.Combine.Type
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Type

import Graphics.Gudni.Layout.Token
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Alignment
import Graphics.Gudni.Layout.Font

import Graphics.Gudni.Util.Debug
import Linear
import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Control.Applicative

import Foreign.Storable

data GlyphLeaf glyphType i
    = GlyphLeaf glyphType
    | ShapeLeaf (FLeaf i)

class IsStyle (StyleOf a) => HasStyle a where
    type StyleOf a :: *

instance (HasStyle i) => HasStyle (Fabric i) where
    type StyleOf (Fabric i) = StyleOf i

class ( HasToken style
      , HasSpace style
      , HasDefault style
      , Storable (SpaceOf style)
      , Ord style
      , Show style
      )
      => IsStyle style where
    styleTextAlignX :: style -> Maybe Alignment
    styleTextAlignY :: style -> Maybe Alignment
    styleGapX       :: style -> SpaceOf style
    styleGapY       :: style -> SpaceOf style
    styleGlyph      :: ( FLeafType i ~ GlyphLeaf (FontName, CodePoint) i
                       , Transformable (Fabric i)
                       , SpaceOf i ~ SpaceOf style
                       )
                    => (style, CodePoint)
                    -> Fabric i

data DefaultStyle = Title | Heading | Normal | Wide deriving (Eq, Ord, Show, Generic)

instance HasSpace DefaultStyle where
  type SpaceOf DefaultStyle = SubSpace

instance HasDefault DefaultStyle where
  defaultValue = Normal

instance HasToken DefaultStyle where
  type TokenOf DefaultStyle = Int

instance IsStyle DefaultStyle where
  styleTextAlignX _ = Just AlignMin
  styleTextAlignY _ = Just AlignMax
  styleGapX style =
      case style of
          Wide -> 1
          _    -> 0.1
  styleGapY style =
      case style of
          Wide -> 1
          _    -> 0.1
  styleGlyph (style, codePoint) =
    case style of
      Title   -> scaleBy 1.5 . FLeaf $ GlyphLeaf ("", codePoint)
      Heading -> scaleBy 1.2 . FLeaf $ GlyphLeaf ("", codePoint)
      Normal  ->               FLeaf $ GlyphLeaf ("", codePoint)
      Wide    -> styleGlyph (Normal, codePoint)

instance Out DefaultStyle

class StyleAxis axis where
  styleTextAlign :: IsStyle style => axis -> style -> Maybe Alignment
  styleGap       :: IsStyle style => axis -> style -> SpaceOf style

instance StyleAxis Horizontal where
  styleTextAlign Horizontal = styleTextAlignX
  styleGap       Horizontal = styleGapX

instance StyleAxis Vertical where
  styleTextAlign Vertical = styleTextAlignY
  styleGap      Vertical  = styleGapY

instance ( Out glyphType
         , Out (FLeaf i)
         ) => Out (GlyphLeaf glyphType i) where
    doc tree =
      case tree of
          GlyphLeaf glyph ->
               text "Glyph" <+> doc glyph
          ShapeLeaf leaf ->
               doc leaf
    docPrec _ = doc

deriving instance (IsStyle style
                  , Show (SpaceOf i)
                  , Show (FChildType i)
                  , Show (FQuery i)
                  , Show (FTex i)
                  ) => Show (GlyphLeaf style i)
