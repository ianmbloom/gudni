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
  ( rectangle
  , unitSquare
  , openRectangle
  , line
  , arc
  , overlap
  , circle
  , solid
  , textureWith
  , cAdd
  , cSubtract
  , glyphWrapOutline
  , testPlot
  )
where

import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Plot
import Graphics.Gudni.Layout.Glyph
import Graphics.Gudni.Layout.Adjacent
import Graphics.Gudni.Util.Debug

import Data.Char (ord)
import Control.Lens
import qualified Data.Vector as V


glyphWrapOutline :: Space s => [Outline s] -> Glyph (CompoundTree s)
glyphWrapOutline outlines =
   let box = foldl1 minMaxBox $ map boxOf $ outlines
   in  Glyph box (SLeaf $ outlines)

-- | Typeclass of shape representations that can be combined with other shapes.
class Compoundable a where
  cAdd      :: a -> a -> a
  cSubtract :: a -> a -> a

-- | Instance for combining simple compound shapes.
instance Compoundable (STree Compound leaf) where
  cAdd      = SMeld CompoundAdd
  cSubtract = flip (SMeld CompoundSubtract) -- the subtracted shape must be above what is being subtracted in the stack.

-- | Instance for combining with glyph wrapped compound shapes.
instance HasSpace leaf => Compoundable (Glyph (STree Compound leaf)) where
  cAdd      = combineGlyph cAdd
  cSubtract = combineGlyph cSubtract

-- | Create a series of segments based on the size of a rectangle.
rectangleCurve :: Space s => Point2 s -> [Bezier s]
rectangleCurve v =
    [ straight (makePoint 0         0        ) (makePoint (v ^. pX) 0        )
    , straight (makePoint (v ^. pX) 0        ) (makePoint (v ^. pX) (v ^. pY))
    , straight (makePoint (v ^. pX) (v ^. pY)) (makePoint 0         (v ^. pY))
    , straight (makePoint 0         (v ^. pY)) (makePoint 0         0        )
    ]

-- | Typeclass of shape representations that can create a rectangle.
class HasSpace a => HasRectangle a where
  rectangle :: Point2 (SpaceOf a) -> a

-- | Basic instance of a rectangle.
instance Space s => HasRectangle (CompoundTree s) where
    rectangle v = SLeaf . pure . Outline . V.fromList . rectangleCurve $ v

-- | Glyph wrapper instance around a rectangle.
instance Space s => HasRectangle (Glyph (CompoundTree s)) where
    rectangle v = Glyph (Box zeroPoint v) . rectangle $ v

-- | Unit square rectangle.
unitSquare :: HasRectangle a => a
unitSquare = rectangle (Point2 1 1)

-- | Open rectangle (Temporary until stroke implemented)
openRectangle :: Space s
              => s
              -> Point2 s
              -> Glyph (CompoundTree s)
openRectangle s p = let strokeDelta = Point2 s s in
                    cSubtract (rectangle p)
                              (mapGlyph (translateBy strokeDelta) $ rectangle (p ^-^ (strokeDelta ^* 2)))

-- | Typeclass of shape representations that can create a line between two points.
class HasLine a where
  line :: (SpaceOf a ~ s)
       => s
       -> Point2 s
       -> Point2 s
       -> a

-- | Basic curve definition for a simple line (Temporary until stroke implemented.)
lineCurve :: (Space s) => s -> Point2 s -> Point2 s -> [Bezier s]
lineCurve stroke p0 p1 =
  let vector = p0 ^-^ p1
      normal = vector ^/ norm vector
      leftNormal = rotate90 normal ^* stroke
      rightNormal = rotate270 normal ^* stroke
  in  [ straight (p0 ^+^ rightNormal) (p0 ^+^ leftNormal )
      , straight (p0 ^+^ leftNormal ) (p1 ^+^ leftNormal )
      , straight (p1 ^+^ leftNormal ) (p1 ^+^ rightNormal)
      , straight (p1 ^+^ rightNormal) (p0 ^+^ rightNormal)
      ]

-- | Basic instance of a simple line.
instance (Space s) => HasLine (CompoundTree s) where
  line stroke p0 p1 = SLeaf . pure $ line stroke p0 p1

instance (Space s) => HasLine (Outline s) where
  line stroke p0 p1 = Outline $ V.fromList $ lineCurve stroke p0 p1
-- | Glyph wrapper instance around a basic line.
instance (Space s) => HasLine (Glyph (CompoundTree s)) where
  line stroke p0 p1 = glyphWrapOutline . pure $ line stroke p0 p1

arcCurve angle = pure . closeOpenCurve . makeArc $ angle

-- | Typeclass of shape representations that implement an arc.
class HasArc a where
  arc :: Angle (SpaceOf a) -> a

-- | Basic instance of an arc.
instance Space s => HasArc (CompoundTree s) where
  arc = SLeaf . arcCurve

-- | Glyph wrapper instance around and arc.
instance Space s => HasArc (Glyph (CompoundTree s)) where
  arc = glyphWrapOutline . arcCurve

-- | Typeclass of shape representations that implement a circle.
class HasArc a => HasCircle a where
  circle :: a

-- | Basic circleCurve
circleCurve :: Space s => [Outline s]
circleCurve = arcCurve fullTurn
centerCircle :: (Fractional (SpaceOf a), SimpleTransformable a) => a -> a
centerCircle = id -- scaleBy 0.5 . translateByXY 0.5 0.5 -- . scaleBy 0.5


-- | Basic instance of a circle.
instance Space s => HasCircle (CompoundTree s) where
  circle = centerCircle . SLeaf $ circleCurve

-- | Glyph wrapper around a circle.
instance Space s => HasCircle (Glyph (CompoundTree s)) where
  circle = centerCircle . glyphWrapOutline $ circleCurve

-- | Typeclass of shape representations that can be filled with a color or texture.
class CanFill a b | b -> a where
    solid :: Color -> a -> b
    textureWith :: NamedTexture -> a -> b

-- | Instance for filling a compound shape and creating a normal shapeTree leaf.
instance CanFill (CompoundTree s) (ShapeTree Int s) where
    solid color         = SLeaf . SRep 0 (Solid color)
    textureWith texture = SLeaf . SRep 0 (Texture texture)

-- | Instance for filling a compound shape and creating a glyph wrapped shapeTree leaf.
instance CanFill (Glyph (CompoundTree s)) (Glyph (ShapeTree Int s)) where
    solid color         = mapGlyph (solid color)
    textureWith texture = mapGlyph (textureWith texture)

-- | Instance for filling a functor of a compound shapetrees such as a list.
instance {-# Overlappable #-} (Functor f, SpaceOf (f (CompoundTree s)) ~ s) => CanFill (f (CompoundTree s)) (f (ShapeTree Int s)) where
    solid color         = fmap (solid color)
    textureWith texture = fmap (textureWith texture)

-- | Instance for filling a functor of a glyph-wrapped compound shapetrees such as a list.
instance {-# Overlappable #-} (Functor f, SpaceOf (f (Glyph (CompoundTree s))) ~ s) => CanFill (f (Glyph (CompoundTree s))) (f (Glyph (ShapeTree Int s))) where
    solid color      = fmap (solid color)
    textureWith pict = fmap (textureWith pict)

-- | Placeholder function to take a plot out of the curveLibrary and substitute a square on Nothing.
testPlot :: String
      -> Glyph (CompoundTree SubSpace)
testPlot name =
  case curveLibrary name of
    Just path -> glyphWrapOutline . pure . closeOpenCurve $ path
    Nothing -> unitSquare
