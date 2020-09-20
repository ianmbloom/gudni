{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Layout.Overlappable
  ( Overlappable(..)
  , overlap
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Layout.Empty

import Control.Applicative

class Overlappable a where
  combine :: a -> a -> a

instance (HasDefault (Meld i)) => Overlappable (STree i) where
  combine = SMeld defaultValue

instance Overlappable (ShapeTree token s) where
  combine = liftShapeTree combine

instance Overlappable (CompoundTree s) where
  combine = liftCompoundTree combine

instance {-# Overlappable #-} (Applicative f, Overlappable a) => Overlappable (f a) where
  combine = liftA2 (combine :: a -> a -> a)

overlap :: (Overlappable a, HasEmpty a, Foldable f) => f a -> a
overlap = foldl combine emptyItem
