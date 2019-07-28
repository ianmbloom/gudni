{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Draw
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic functions for constructing drawings.

module Graphics.Gudni.Layout.Draw
  ( lPath
  , rectangle
  , unitSquare
  , openRectangle
  , line
  , arc
  , overlap
  , circle
  , solid
  , textureWith
  , fromSegments
  , cAdd
  , cSubtract
  , cContinue
  , boxedOutline
  )
where

import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Plot
import Graphics.Gudni.Layout.Glyph
import Graphics.Gudni.Layout.Scaffolding

import Data.Char (ord)
import Control.Lens

class Compoundable a where
  cAdd      :: a -> a -> a
  cSubtract :: a -> a -> a
  cContinue :: a -> a -> a

instance Compoundable (STree Compound leaf) where
  cAdd      = SMeld CompoundAdd
  cSubtract = flip (SMeld CompoundSubtract) -- the subtracted shape must be above what is being subtracted in the stack.
  cContinue = SMeld CompoundContinue

instance HasSpace leaf => Compoundable (Glyph (STree Compound leaf)) where
  cAdd      = combineGlyph cAdd
  cSubtract = combineGlyph cSubtract
  cContinue = combineGlyph cContinue

class HasSpace a => HasRectangle a where
  rectangle :: Point2 (SpaceOf a) -> a

rectangleCurve :: Space s => Point2 s -> [Segment s]
rectangleCurve v =
    [ straight 0      0
    , straight (v ^. pX) 0
    , straight (v ^. pX) (v ^. pY)
    , straight 0         (v ^. pY)
    ]

instance Space s => HasRectangle (Glyph (CompoundTree s)) where
    rectangle v = Glyph (Box zeroPoint v) . Just . rectangle $ v

instance Space s => HasRectangle (CompoundTree s) where
    rectangle v = SLeaf . segmentsToOutline . pure . rectangleCurve $ v

unitSquare :: HasRectangle a => a
unitSquare = rectangle (Point2 1 1)

openRectangle :: Space s
              => s
              -> Point2 s
              -> Glyph (CompoundTree s)
openRectangle s p = let strokeDelta = Point2 s s in
                    cSubtract (rectangle p)
                              (mapGlyph (tTranslate strokeDelta) $ rectangle (p ^-^ (strokeDelta ^* 2)))

class HasLine a where
  line :: (SpaceOf a ~ s)
       => s
       -> Point2 s
       -> Point2 s
       -> a

lineCurve :: (Space s) => s -> Point2 s -> Point2 s -> [Segment s]
lineCurve stroke p0 p1 =
  let vector = p0 ^-^ p1
      normal = vector ^/ norm vector
      leftNormal = rotate90 normal ^* stroke
      rightNormal = rotate270 normal ^* stroke
  in  [ Seg (p0 ^+^ rightNormal) Nothing
      , Seg (p0 ^+^ leftNormal ) Nothing
      , Seg (p1 ^+^ leftNormal ) Nothing
      , Seg (p1 ^+^ rightNormal) Nothing
      ]

instance (Space s) => HasLine (Glyph (CompoundTree s)) where
  line stroke p0 p1 = boxedOutline . segmentsToOutline . pure $ lineCurve stroke p0 p1

instance (Space s) => HasLine (CompoundTree s) where
  line stroke p0 p1 = SLeaf . segmentsToOutline . pure $ lineCurve stroke p0 p1

class HasArc a where
  arc :: Angle (SpaceOf a) -> a

instance Space s => HasArc (Glyph (CompoundTree s)) where
  arc angle = let curve = segmentsToOutline . pure . closeOpenCurve . makeArc $ angle
              in  Glyph (boxOf curve) (Just . SLeaf $ curve)

instance Space s => HasArc (CompoundTree s) where
  arc = SLeaf . segmentsToOutline . pure . closeOpenCurve . makeArc

class HasArc a => HasCircle a where
  circle :: a

instance Space s => HasCircle (CompoundTree s) where
  circle = tTranslateXY 0.5 0.5 $ tScale 0.5 $ arc fullTurn

instance Space s => HasCircle (Glyph (CompoundTree s)) where
  circle = overlap [ openRectangle 0.025 (Point2 1 1)
                   , circle
                   ]

boxedOutline :: Space s => [Outline s] -> Glyph (CompoundTree s)
boxedOutline outlines =
   let box = foldl1 minMaxBox $ map boxOf $ outlines
   in  Glyph box (Just . SLeaf $ outlines)

class HasFromSegments a where
  fromSegments :: [Segment (SpaceOf a)] -> a

instance Space s => HasFromSegments (CompoundTree s) where
  fromSegments = SLeaf . segmentsToOutline . pure

instance Space s => HasFromSegments (Glyph (CompoundTree s)) where
  fromSegments = boxedOutline . segmentsToOutline . pure

class CanFill a b | b -> a where
    solid :: Color -> a -> b
    textureWith :: NamedTexture -> a -> b

instance CanFill (CompoundTree s) (ShapeTree Int s) where
    solid color         = SLeaf . SRep 0 (Solid color)
    textureWith texture = SLeaf . SRep 0 (Texture texture)

instance CanFill (Glyph (CompoundTree s)) (Glyph (ShapeTree Int s)) where
    solid color         = mapGlyph (solid color)
    textureWith texture = mapGlyph (textureWith texture)

instance {-# Overlappable #-} (Functor f, SpaceOf (f (CompoundTree s)) ~ s) => CanFill (f (CompoundTree s)) (f (ShapeTree Int s)) where
    solid color         = fmap (solid color)
    textureWith texture = fmap (textureWith texture)

instance {-# Overlappable #-} (Functor f, SpaceOf (f (Glyph (CompoundTree s))) ~ s) => CanFill (f (Glyph (CompoundTree s))) (f (Glyph (ShapeTree Int s))) where
    solid color      = fmap (solid color)
    textureWith pict = fmap (textureWith pict)

lPath :: String
      -> Glyph (CompoundTree SubSpace)
lPath name =
  case curveLibrary name of
    Just path -> boxedOutline . segmentsToOutline . pure . closeOpenCurve $ path
    Nothing -> unitSquare
