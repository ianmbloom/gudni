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
  , combineGlyph
  , HasEmpty(..)
  , glyphWrapShape
  )
where

import Graphics.Gudni.Figure

import Control.DeepSeq
import Data.Hashable
import Control.Lens
import Control.Monad

data Glyph a = Glyph
  { _glyphBox :: Box (SpaceOf a)
  , _unGlyph :: a
  } |
  EmptyGlyph
makeLenses ''Glyph

deriving instance (Show (SpaceOf a), Show a) => Show (Glyph a)
deriving instance (Eq   (SpaceOf a), Eq   a) => Eq   (Glyph a)
deriving instance (Ord  (SpaceOf a), Ord  a) => Ord  (Glyph a)

class HasEmpty a where
  emptyItem :: a
  isEmpty   :: a -> Bool

instance HasEmpty (Glyph rep) where
  emptyItem = EmptyGlyph
  isEmpty EmptyGlyph = True
  isEmpty _ = False

mapGlyph :: forall a b . (SpaceOf a ~ SpaceOf b) => (a->b) -> Glyph a -> Glyph b
mapGlyph f (Glyph box a) = Glyph box (f a)
mapGlyph f EmptyGlyph    = EmptyGlyph

instance SimpleTransformable a => SimpleTransformable (Glyph a) where
  translateBy p (Glyph box a) = Glyph (translateBy p box) (translateBy p a)
  translateBy p EmptyGlyph    = EmptyGlyph
  stretchBy   p (Glyph box a) = Glyph (stretchBy p box) (stretchBy p a)
  stretchBy   p EmptyGlyph    = EmptyGlyph
  scaleBy     f (Glyph box a) = Glyph (scaleBy f box) (scaleBy f a)
  scaleBy     f EmptyGlyph    = EmptyGlyph

instance HasSpace a => HasSpace (Glyph a) where
  type SpaceOf (Glyph a) = SpaceOf a

instance (Hashable a, Hashable (SpaceOf a)) => Hashable (Glyph a) where
    hashWithSalt s (Glyph a b) = s `hashWithSalt` a `hashWithSalt` b

instance (NFData a, NFData (SpaceOf a)) => NFData (Glyph a) where
  rnf (Glyph a b) = a `deepseq` b `deepseq` ()

showGlyph :: (HasSpace t) => Glyph t -> [Char]
showGlyph EmptyGlyph = "EmptyGlyph"
showGlyph (Glyph box _) = show box

maxPoint :: Ord s => Point2 s -> Point2 s -> Point2 s
minPoint :: Ord s => Point2 s -> Point2 s -> Point2 s
maxPoint (Point2 x0 y0) (Point2 x1 y1) = Point2 (max x0 x1) (max y0 y1)
minPoint (Point2 x0 y0) (Point2 x1 y1) = Point2 (min x0 x1) (min y0 y1)

combineGlyph :: (HasSpace rep)
             => (rep -> rep -> rep)
             -> Glyph rep
             -> Glyph rep
             -> Glyph rep
combineGlyph op a b =
  if isEmpty a
  then b
  else
  if isEmpty b
  then a
  else
  let tl = minPoint (a ^?! glyphBox . topLeftBox    ) (b ^?! glyphBox . topLeftBox    )
      br = maxPoint (a ^?! glyphBox . bottomRightBox) (b ^?! glyphBox . bottomRightBox)
  in  Glyph (Box tl br) (op (a ^?! unGlyph) (b ^?! unGlyph))

glyphWrapShape :: Space s => Shape s -> Glyph (CompoundTree s)
glyphWrapShape outlines =
   let box = foldl1 minMaxBox $ map boxOf $ outlines
   in  Glyph box (SLeaf $ outlines)
