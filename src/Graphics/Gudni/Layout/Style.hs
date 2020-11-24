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
  ( HasStyle(..)
  , IsStyle(..)
  , DefaultStyle(..)
  , StyleAxis(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.StorableInstances

import Graphics.Gudni.Layout.Token
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Alignment
import Graphics.Gudni.Layout.Overlappable
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Util.Debug
import Linear
import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Control.Applicative

import Foreign.Storable

class IsStyle (StyleOf a) => HasStyle a where
    type StyleOf a :: *

class ( HasToken style
      , HasSpace style
      , HasDefault style
      , Storable (SpaceOf style)
      , Show style
      )
      => IsStyle style where
    styleTextAlignX :: style -> Maybe Alignment
    styleTextAlignY :: style -> Maybe Alignment
    styleGapX       :: style -> SpaceOf style
    styleGapY       :: style -> SpaceOf style
    styleGlyph      :: Monad m => style -> CodePoint -> FontMonad style m (ProximityCompoundTree style)

data DefaultStyle = Title | Heading | Normal | Wide deriving (Eq, Show, Generic)

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
  styleGlyph style codePoint =
    case style of
      Title   -> scaleBy 1.5 <$> getGlyph codePoint
      Heading -> scaleBy 1.2 <$> getGlyph codePoint
      Normal  -> getGlyph codePoint
      Wide    -> styleGlyph Normal codePoint

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
