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

module Graphics.Gudni.Layout.Style
  ( HasStyle(..)
  , IsStyle(..)
  , DefaultStyle(..)
  )
where

import Graphics.Gudni.Figure
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

class (HasSpace style, HasDefault style, Show style) => IsStyle style where
  styleTextAlignX :: style -> Maybe Alignment
  styleTextAlignY :: style -> Maybe Alignment
  styleGapX :: style -> (SpaceOf style)
  styleGapY :: style -> (SpaceOf style)
  styleGlyph :: Monad m => style -> CodePoint -> FontMonad m (Shape (SpaceOf style), Maybe (Box (SpaceOf style)))

data DefaultStyle = Title | Heading | Normal deriving (Eq, Show)

instance HasSpace DefaultStyle where
  type SpaceOf DefaultStyle = SubSpace

overGlyph :: (Shape s -> Shape s) -> (Box s -> Box s) -> (Shape s, Maybe (Box s)) -> (Shape s, Maybe (Box s))
overGlyph sf bf (shape, box) = (sf shape, fmap bf box)

instance HasDefault DefaultStyle where
  defaultValue = Normal

instance IsStyle DefaultStyle where
  styleTextAlignX _ = Just AlignMin
  styleTextAlignY _ = Just AlignMax
  styleGapX _ = 0.1
  styleGapY _ = 0.1
  styleGlyph style codePoint =
    case style of
      Title   -> overGlyph (scaleBy 1.5) (scaleBy 1.5) <$> getGlyph codePoint
      Heading -> overGlyph (scaleBy 1.2) (scaleBy 1.2) <$> getGlyph codePoint
      Normal  -> getGlyph codePoint
