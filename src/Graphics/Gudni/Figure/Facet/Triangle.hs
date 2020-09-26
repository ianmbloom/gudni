{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.Triangle
  ( Tri(..)
  , reverseParts
  , rotateTri1
  , rotateTri2
  , sideTris
  , centerTri
  )
where

import Graphics.Gudni.Figure.Primitive

import Graphics.Gudni.Util.Debug

import Linear.V2
import Linear.V3
import Linear.Vector
import Control.Applicative
import Control.Lens

type Tri s = V3 (Point2 s)

u0 :: V3 a -> a
u0 = view _x
u1 :: V3 a -> a
u1 = view _y
u2 :: V3 a -> a
u2 = view _z

rotateTri1 :: V3 a -> V3 a
rotateTri1 (V3 a0 a1 a2) = (V3 a1 a2 a0)

rotateTri2 :: V3 a -> V3 a
rotateTri2 = rotateTri1 . rotateTri1

reverseParts :: V3 a -> V3 a
reverseParts (V3 a b c) = V3 a c b

sideTri :: Space s => V3 (Point2 s) -> V3 (Point2 s)
sideTri i =
   fmap (0.5 *^) (pure (u0 i) + i)

sideTris :: Space s => V3 (Point2 s) -> V3 (V3 (Point2 s))
sideTris tri =
  let a = sideTri                tri
      b = sideTri . rotateTri2 $ tri
      c = sideTri . rotateTri1 $ tri
  in  V3 a b c

centerTri :: Space s => V3 (Point2 s) -> V3 (Point2 s)
centerTri i = fmap (0.5 *^) (rotateTri1 i + rotateTri2 i)
