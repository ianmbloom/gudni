{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Gudni.Layout.Overlappable
  ( Overlappable(..)
  , overlap
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Glyph

import Control.Applicative

class Overlappable a where
  combine :: a -> a -> a

instance (HasSpace rep, HasDefault o) => Overlappable (Glyph (STree o rep)) where
  combine = combineGlyph (SMeld defaultValue)

instance (HasDefault o) => Overlappable (STree o rep) where
  combine = SMeld defaultValue

instance {-# Overlappable #-} (Applicative f, Overlappable a) => Overlappable (f a) where
  combine = liftA2 (combine :: a -> a -> a)

overlap :: (Overlappable a, HasEmpty a, Foldable f) => f a -> a
overlap = foldl combine emptyItem
