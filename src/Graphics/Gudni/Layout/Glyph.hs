{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Glyph
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for bounding box based layouts.
-- A glyph is defined as any representation of a shape that has a defined bounding box.

module Graphics.Gudni.Layout.Glyph
  ( Glyph(..)
  , glyphBox
  , unGlyph
  , mapGlyph
  , showGlyph
  )
where

import Graphics.Gudni.Figure

import Control.DeepSeq
import Data.Hashable
import Control.Lens

data Glyph a = Glyph
  { _glyphBox :: Box (SpaceOf a)
  , _unGlyph :: Maybe a
  }
makeLenses ''Glyph

deriving instance (Show (SpaceOf a), Show a) => Show (Glyph a)
deriving instance (Eq   (SpaceOf a), Eq   a) => Eq   (Glyph a)
deriving instance (Ord  (SpaceOf a), Ord  a) => Ord  (Glyph a)

mapGlyph :: forall a b . (SpaceOf a ~ SpaceOf b) => (a->b) -> Glyph a -> Glyph b
mapGlyph f (Glyph box a) = Glyph box (fmap f a)

instance SimpleTransformable a => SimpleTransformable (Glyph a) where
  tTranslate p (Glyph box a) = Glyph (tTranslate p box) (fmap (tTranslate p) a)
  tScale f (Glyph box a) = Glyph (tScale f box) (fmap (tScale f) a)

instance HasSpace a => HasSpace (Glyph a) where
  type SpaceOf (Glyph a) = SpaceOf a

instance (Hashable a, Hashable (SpaceOf a)) => Hashable (Glyph a) where
    hashWithSalt s (Glyph a b) = s `hashWithSalt` a `hashWithSalt` b

instance (NFData a, NFData (SpaceOf a)) => NFData (Glyph a) where
  rnf (Glyph a b) = a `deepseq` b `deepseq` ()

showGlyph :: (HasSpace t) => Glyph t -> [Char]
showGlyph = show . view glyphBox
