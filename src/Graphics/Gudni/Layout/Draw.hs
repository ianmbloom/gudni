{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

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
import Graphics.Gudni.Layout.Boxed
import Graphics.Gudni.Layout.Scaffolding

import Data.Char (ord)
import Control.Lens

class Compoundable a where
  cAdd      :: a -> a -> a
  cSubtract :: a -> a -> a
  cContinue :: a -> a -> a

instance Compoundable (STree Compound leaf) where
  cAdd      = SMeld CompoundAdd
  cSubtract = SMeld CompoundSubtract
  cContinue = SMeld CompoundContinue

instance HasSpace leaf => Compoundable (Boxed (STree Compound leaf)) where
  cAdd      = combineBoxed cAdd
  cSubtract = combineBoxed cSubtract
  cContinue = combineBoxed cContinue

class HasSpace a => HasRectangle a where
  rectangle :: Point2 (SpaceOf a) -> a

rectangleCurve :: Space s => Point2 s -> [Segment s]
rectangleCurve v =
    [ straight 0      0
    , straight (v ^. pX) 0
    , straight (v ^. pX) (v ^. pY)
    , straight 0         (v ^. pY)
    ]

instance Space s => HasRectangle (Boxed (CompoundTree s)) where
    rectangle v = Boxed (Box zeroPoint v) . Just . rectangle $ v

instance Space s => HasRectangle (CompoundTree s) where
    rectangle v = SLeaf . segmentsToOutline . pure . rectangleCurve $ v

unitSquare :: HasRectangle a => a
unitSquare = rectangle (Point2 1 1)

openRectangle :: Space s
              => s
              -> Point2 s
              -> Boxed (CompoundTree s)
openRectangle s p = let strokeDelta = Point2 s s in
                    cSubtract (rectangle p)
                              (mapBoxed (tTranslate strokeDelta) $ rectangle (p ^-^ (strokeDelta ^* 2)))

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

instance (Space s) => HasLine (Boxed (CompoundTree s)) where
  line stroke p0 p1 = boxedOutline . segmentsToOutline . pure $ lineCurve stroke p0 p1

instance (Space s) => HasLine (CompoundTree s) where
  line stroke p0 p1 = SLeaf . segmentsToOutline . pure $ lineCurve stroke p0 p1

class HasArc a where
  arc :: Angle (SpaceOf a) -> a

instance Space s => HasArc (Boxed (CompoundTree s)) where
  arc angle = let curve = segmentsToOutline . pure . closeOpenCurve . makeArc $ angle
              in  Boxed (boxOf curve) (Just . SLeaf $ curve)

instance Space s => HasArc (CompoundTree s) where
  arc = SLeaf . segmentsToOutline . pure . closeOpenCurve . makeArc

class HasArc a => HasCircle a where
  circle :: a

instance Space s => HasCircle (CompoundTree s) where
  circle = tTranslateXY 0.5 0.5 $ tScale 0.5 $ arc fullTurn

instance Space s => HasCircle (Boxed (CompoundTree s)) where
  circle = overlap [ openRectangle 0.025 (Point2 1 1)
                   , circle
                   ]

boxedOutline :: Space s => [Outline s] -> Boxed (CompoundTree s)
boxedOutline outlines =
   let box = foldl1 minMaxBox $ map boxOf $ outlines
   in  Boxed box (Just . SLeaf $ outlines)

class HasFromSegments a where
  fromSegments :: [Segment (SpaceOf a)] -> a

instance Space s => HasFromSegments (CompoundTree s) where
  fromSegments = SLeaf . segmentsToOutline . pure

instance Space s => HasFromSegments (Boxed (CompoundTree s)) where
  fromSegments = boxedOutline . segmentsToOutline . pure

class CanFill a b | b -> a where
    solid :: Color -> a -> b
    textureWith :: PictureUsage PictId (SpaceOf a) -> a -> b

instance CanFill (CompoundTree s) (ShapeTree Int s) where
    solid color      = SLeaf . SRep 0 (Solid color)
    textureWith pict = SLeaf . SRep 0 (Texture pict)

instance CanFill (Boxed (CompoundTree s)) (Boxed (ShapeTree Int s)) where
    solid color      = mapBoxed (solid color)
    textureWith pict = mapBoxed (textureWith pict)

instance {-# Overlappable #-} (Functor f, SpaceOf (f (CompoundTree s)) ~ s) => CanFill (f (CompoundTree s)) (f (ShapeTree Int s)) where
    solid color      = fmap (solid color)
    textureWith pict = fmap (textureWith pict)

instance {-# Overlappable #-} (Functor f, SpaceOf (f (Boxed (CompoundTree s))) ~ s) => CanFill (f (Boxed (CompoundTree s))) (f (Boxed (ShapeTree Int s))) where
    solid color      = fmap (solid color)
    textureWith pict = fmap (textureWith pict)

lPath :: String
      -> Boxed (CompoundTree SubSpace)
lPath name =
  case curveLibrary name of
    Just path -> boxedOutline . segmentsToOutline . pure . closeOpenCurve $ path
    Nothing -> unitSquare
