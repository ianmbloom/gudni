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

module Graphics.Gudni.Layout.Style
  ( HasStyle(..)
  , IsStyle(..)
  , DefaultStyle(..)
  )
where

import Graphics.Gudni.Figure
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

class HasStyle a where
    type StyleOf a :: *

class ( HasToken style
      , HasSpace style
      , HasDefault style
      , Show style)
      => IsStyle style where
    styleTextAlignX :: style -> Maybe Alignment
    styleTextAlignY :: style -> Maybe Alignment
    styleGapX :: style -> (SpaceOf style)
    styleGapY :: style -> (SpaceOf style)
    styleGlyph :: Monad m => style -> CodePoint -> FontMonad style m (ProximityCompoundTree style)

data DefaultStyle = Title | Heading | Normal deriving (Eq, Show)

instance HasSpace DefaultStyle where
  type SpaceOf DefaultStyle = SubSpace

instance HasDefault DefaultStyle where
  defaultValue = Normal

instance HasToken DefaultStyle where
  type TokenOf DefaultStyle = Int
  
instance IsStyle DefaultStyle where
  styleTextAlignX _ = Just AlignMin
  styleTextAlignY _ = Just AlignMax
  styleGapX _ = 0.1
  styleGapY _ = 0.1
  styleGlyph style codePoint =
    case style of
      Title   -> scaleBy 1.5 <$> getGlyph codePoint
      Heading -> scaleBy 1.2 <$> getGlyph codePoint
      Normal  -> getGlyph codePoint
