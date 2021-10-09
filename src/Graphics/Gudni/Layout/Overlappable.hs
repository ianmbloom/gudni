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
import Graphics.Gudni.Layout.Class
import Graphics.Gudni.Layout.Type

import Control.Applicative

class Overlappable a where
    combine :: a -> a -> a

instance (IsLayout (Layout x style)) => Overlappable (Layout x style) where
    combine = onTopOf defaultValue Nothing Nothing defaultValue

-- instance {-# Overlappable #-} (Applicative f, Overlappable a) => Overlappable (f a) where
--     combine = liftA2 (combine :: a -> a -> a)

overlap :: (Overlappable a, HasEmpty a, Foldable f) => f a -> a
overlap = foldl combine emptyItem
