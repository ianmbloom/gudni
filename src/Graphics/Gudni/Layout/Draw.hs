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
  ( CanMask(..)
  , CanFill(..)
  , shapeFrom
  , withColor
  , withTexture
  , withRadialGradient
  , withLinearGradient
  , Compoundable(..)
  , HasCircle(..)
  , HasRectangle(..)
  )
where

import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Interface.Token
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Plot
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Layout.Glyph
import Graphics.Gudni.Layout.Adjacent
import Graphics.Gudni.Util.Debug

import Data.Char (ord)
import Control.Lens
import qualified Data.Vector as V
import Control.Applicative

-- | Typeclass of objects that can represent a mask (a filled in area) or a combination of filled
-- in areas that are repesented by CompoundTree.
class CanMask a b | b -> a where
    mask :: a -> b

instance CanMask (Shape s) (CompoundTree s) where
    mask = SLeaf

instance CanMask (Glyph (Shape s)) (Glyph (CompoundTree s)) where
    mask = mapGlyph mask

instance (Functor f) => CanMask (f (Shape s)) (f (CompoundTree s)) where
    mask = fmap mask

shapeFrom :: Outline s -> Shape s
shapeFrom = Shape . pure
-- | Typeclass of shape representations that can be filled with a color or texture.
class (HasSpace a, HasToken b) => CanFill a b | b -> a where
    withFill    :: Substance NamedTexture (SpaceOf a) -> a -> b
    assignToken :: (TokenOf b) -> b -> b

withColor :: (HasToken b, CanFill a b) => Color -> a -> b
withColor color = withFill (Solid color)

withTexture :: (HasToken b, CanFill a b) =>  NamedTexture -> a -> b
withTexture texture = withFill (Texture texture)

withRadialGradient :: (HasSpace a, HasToken b, CanFill a b) => Point2 (SpaceOf a) -> SpaceOf a -> Color -> SpaceOf a -> Color -> a -> b
withRadialGradient center innerRadius innerColor outerRadius outerColor =
  withFill (Radial $ RadialGradient center innerRadius outerRadius innerColor outerColor)

withLinearGradient :: (HasSpace a, HasToken b, CanFill a b) => Point2 (SpaceOf a) -> Color -> Point2 (SpaceOf a) -> Color -> a -> b
withLinearGradient start startColor end endColor =
  withFill (Linear $ LinearGradient start end startColor endColor)

instance HasToken (ShapeTree token s) where
    type TokenOf (ShapeTree token s) = token

instance HasToken a => HasToken (Glyph a) where
    type TokenOf (Glyph a) = TokenOf a

-- | Instance for filling a compound shape and creating a normal shapeTree leaf.
instance (Space s) => CanFill (CompoundTree s) (ShapeTree token s) where
    withFill substance  = SLeaf . SRep Nothing substance
    assignToken token   = mapSTree (set sRepToken (Just token))

-- | Instance for filling a compound shape and creating a glyph wrapped shapeTree leaf.
instance (Space s) => CanFill (Glyph (CompoundTree s)) (Glyph (ShapeTree token s)) where
    withFill substance  = mapGlyph (withFill substance)
    assignToken token   = mapGlyph (assignToken token)


-- | Instance for filling a functor of a compound shapetrees such as a list.
instance {-# Overlappable #-} ( HasSpace (f (CompoundTree s))
                              , Space s
                              , Functor f, SpaceOf (f (CompoundTree s)) ~ s
                              , HasToken (f (ShapeTree token s))
                              , TokenOf (f (ShapeTree token s)) ~ token)
    => CanFill (f (CompoundTree s)) (f (ShapeTree token s)) where
    withFill substance = fmap (withFill substance)
    assignToken token  = fmap (assignToken token)

-- | Instance for filling a functor of a glyph-wrapped compound shapetrees such as a list.
instance {-# Overlappable #-} ( HasSpace (f (Glyph (CompoundTree s)))
                              , Space s
                              , Functor f, SpaceOf (f (Glyph (CompoundTree s))) ~ s
                              , HasToken (f (Glyph (ShapeTree token s)))
                              , TokenOf (f (Glyph (ShapeTree token s))) ~ token)
         => CanFill (f (Glyph (CompoundTree s))) (f (Glyph (ShapeTree token s))) where
    withFill substance = fmap (mapGlyph (withFill substance))
    assignToken token  = fmap (mapGlyph (assignToken token))

-- | Typeclass of shape representations that can be combined with other shapes.
class Compoundable a where
  addOver      :: a -> a -> a
  subtractFrom :: a -> a -> a

-- | Instance for combining simple compound shapes.
instance Compoundable (STree Compound leaf) where
  addOver      = SMeld CompoundAdd
  subtractFrom = SMeld CompoundSubtract -- the subtracted shape must be above what is being subtracted in the stack.

-- | Instance for combining with glyph wrapped compound shapes.
instance HasSpace leaf => Compoundable (Glyph (STree Compound leaf)) where
  addOver      = combineGlyph addOver
  subtractFrom = combineGlyph subtractFrom

-- | Instance for functors of compound shapes.
instance (Applicative f, HasSpace leaf) => Compoundable (f (STree Compound leaf)) where
  addOver      = liftA2 addOver
  subtractFrom = liftA2 subtractFrom

-- | Create a series of segments based on the size of a rectangle.
rectangleCurve :: (Alternative f, Space s) => Point2 s -> f (Bezier s)
rectangleCurve v =
        ( pure $ line (makePoint 0         0        ) (makePoint (v ^. pX) 0        ) )
    <|> ( pure $ line (makePoint (v ^. pX) 0        ) (makePoint (v ^. pX) (v ^. pY)) )
    <|> ( pure $ line (makePoint (v ^. pX) (v ^. pY)) (makePoint 0         (v ^. pY)) )
    <|> ( pure $ line (makePoint 0         (v ^. pY)) (makePoint 0         0        ) )

-- | Typeclass of shape representations that can create a rectangle.
class HasSpace a => HasRectangle a where
  rectangle :: Point2 (SpaceOf a) -> a

-- | Basic instances of a rectangle.
instance (Alternative f, Space s) => HasRectangle (Outline_ f s) where
    rectangle v = Outline . rectangleCurve $ v

instance (Space s) => HasRectangle (Shape s) where
    rectangle v = Shape . pure . rectangle $ v

instance Space s => HasRectangle (CompoundTree s) where
    rectangle v = mask . rectangle $ v

-- | Glyph wrapper instance around a rectangle.
instance Space s => HasRectangle (Glyph (CompoundTree s)) where
    rectangle v = Glyph (Box zeroPoint v) . rectangle $ v

-- | Basic circleCurve
circleCurve :: (Space s, Chain f, Show (f (Bezier s))) => OpenCurve_ f s
circleCurve = makeArc fullTurn

-- | Typeclass of shape representations that implement a circle.
class HasCircle a where
  circle :: a

-- | Basic instances of a circle.
instance (Chain f, Space s, Show (f (Bezier s))) => HasCircle (Outline_ f s) where
    circle = closeOpenCurve circleCurve

instance (Space s) => HasCircle (Shape s) where
    circle = Shape . pure $ circle

instance Space s => HasCircle (CompoundTree s) where
    circle = mask circle

-- | Glyph wrapper instance around a rectangle.
instance Space s => HasCircle (Glyph (CompoundTree s)) where
    circle = glyphWrapShape $ circle
