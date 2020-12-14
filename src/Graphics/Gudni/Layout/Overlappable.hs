{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}

module Graphics.Gudni.Layout.Overlappable
  ( Overlappable(..)
  , overlap
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Empty
import Graphics.Gudni.Raster.Fabric.Type

import Control.Applicative

class Overlappable a where
  combine :: a -> a -> a

instance (HasDefault (FBinaryType i), FChildType i ~ Fabric i) => Overlappable (Fabric i) where
  combine = FBinary defaultValue

instance {-# Overlappable #-} (Applicative f, Overlappable a) => Overlappable (f a) where
  combine = liftA2 (combine :: a -> a -> a)

overlap :: (Overlappable a, HasEmpty a, Foldable f) => f a -> a
overlap = foldl combine emptyItem
