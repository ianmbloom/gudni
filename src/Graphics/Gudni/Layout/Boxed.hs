{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Boxed
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for bounding box based layouts.

module Graphics.Gudni.Layout.Boxed
  ( Boxed(..)
  , boxAround
  , unBoxed
  , mapBoxed
  , showBoxed
  , showBoxPair
  )
where

import Graphics.Gudni.Figure

import Control.DeepSeq
import Data.Hashable
import Control.Lens

data Boxed a = Boxed
  { _boxAround :: Box (SpaceOf a)
  , _unBoxed :: Maybe a
  }
makeLenses ''Boxed

deriving instance (Show (SpaceOf a), Show a) => Show (Boxed a)
deriving instance (Eq   (SpaceOf a), Eq   a) => Eq   (Boxed a)
deriving instance (Ord  (SpaceOf a), Ord  a) => Ord  (Boxed a)

mapBoxed :: forall a b . (SpaceOf a ~ SpaceOf b) => (a->b) -> Boxed a -> Boxed b
mapBoxed f (Boxed box a) = Boxed box (fmap f a)

instance SimpleTransformable a => SimpleTransformable (Boxed a) where
  tTranslate p (Boxed box a) = Boxed (tTranslate p box) (fmap (tTranslate p) a)
  tScale f (Boxed box a) = Boxed (tScale f box) (fmap (tScale f) a)

instance HasSpace a => HasSpace (Boxed a) where
  type SpaceOf (Boxed a) = SpaceOf a

instance (Hashable a, Hashable (SpaceOf a)) => Hashable (Boxed a) where
    hashWithSalt s (Boxed a b) = s `hashWithSalt` a `hashWithSalt` b

instance (NFData a, NFData (SpaceOf a)) => NFData (Boxed a) where
  rnf (Boxed a b) = a `deepseq` b `deepseq` ()

showBoxed :: (HasSpace t) => Boxed t -> [Char]
showBoxed = showBox . view boxAround
showBox (Box tl br) = "Box:" ++ show tl ++ " sz:" ++ show (br - tl)
showBoxPair (a, b) = "(" ++ showBox (view boxAround a) ++ ", " ++  showBox (view boxAround b) ++ ")"
