{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.BezierTriangle
  ( BezTri(..)
  , sideBezTris
  , centerBezTri
  , bezTriToBeziers
  , shouldSubdivideBezTri
  , p0
  , c0
  , p1
  , c1
  , p2
  , c2
  , triToBezTri
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Facet.Triangle

import Graphics.Gudni.Util.Debug

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Control.Applicative
--import Control.Monad
import Control.Lens
--import Data.Foldable
--import qualified Data.Vector as V

type BezTri s = V3 (V2 (Point2 s))

p0 :: BezTri s -> Point2 s
p0 = view (_x . _x)
c0 :: BezTri s -> Point2 s
c0 = view (_x . _y)
p1 :: BezTri s -> Point2 s
p1 = view (_y . _x)
c1 :: BezTri s -> Point2 s
c1 = view (_y . _y)
p2 :: BezTri s -> Point2 s
p2 = view (_z . _x)
c2 :: BezTri s -> Point2 s
c2 = view (_z . _y)

side0 :: Space s => BezTri s -> V2 (Point2 s)
side0 i =
  let aP0 = 0.5 *^ (p0 i + p0 i)
      aC0 = 0.5 *^ (c0 i + p0 i)
 in   V2 aP0 aC0

side1 :: Space s => BezTri s -> V2 (Point2 s)
side1 i =
  let aP1 = 0.25 *^ (p1 i + p0 i + c0 i + c0 i)
      aC1 = 0.25 *^ (c1 i + p0 i + c0 i + c2 i)
  in  V2 aP1 aC1

side2 :: Space s => BezTri s -> V2 (Point2 s)
side2 i =
  let aP2 = 0.25 *^ (p2 i + p0 i + c2 i + c2 i)
      aC2 = 0.5  *^ (c2 i + p0 i)
  in  V2 aP2 aC2

sideTriangle :: Space s => BezTri s -> BezTri s
sideTriangle bezTri =
      V3 (side0 bezTri)
         (side1 bezTri)
         (side2 bezTri)

reverseBezTri :: BezTri s -> BezTri s
reverseBezTri i = V3 (V2 (p0 i) (c2 i))
                     (V2 (p2 i) (c1 i))
                     (V2 (p1 i) (c0 i))

flipSidesBezTri :: BezTri s -> BezTri s
flipSidesBezTri i = V3 (V2 (p0 i) (c1 i))
                       (V2 (p1 i) (c2 i))
                       (V2 (p2 i) (c0 i))

sideBezTris :: Space s => BezTri s -> V3 (BezTri s)
sideBezTris bezTri =
  let aOut = sideTriangle                bezTri
      bOut = sideTriangle . rotateTri1 $ bezTri
      cOut = sideTriangle . rotateTri2 $ bezTri
  in  V3 aOut bOut cOut

innerSide :: BezTri s -> V2 (Point2 s)
innerSide = view _y

centerBezTri :: Space s => V3 (BezTri s) -> BezTri s
centerBezTri = flipSidesBezTri . fmap innerSide

{-
centerBezTri bezTri =
  V3 (side1                          bezTri)
     (side1 . rotateTri2 $ bezTri)
     (side1 . rotateTri1            $ bezTri)
-}

bezTriStartPoints :: BezTri s -> V3 (Point2 s)
bezTriStartPoints = fmap (view _x)

bezTriControls :: BezTri s -> V3 (Point2 s)
bezTriControls  = fmap (view _y)

bezTriEndPoints :: BezTri s -> V3 (Point2 s)
bezTriEndPoints = fmap (view _x) . rotateTri1

bezTriToBeziers :: BezTri s -> V3 (Bezier s)
bezTriToBeziers bezTri =
  let starts   = bezTriStartPoints bezTri
      controls = bezTriControls    bezTri
      ends     = bezTriEndPoints   bezTri
  in  liftA3 Bez starts controls ends

shouldSubdivideBezTri :: (Space s) => s -> BezTri s -> Bool
shouldSubdivideBezTri tolerance = or . fmap (shouldSubdivideBezier tolerance) . bezTriToBeziers

triToBezTri :: Space s => Tri s -> BezTri s
triToBezTri (V3 a b c) = V3 (V2 a (mid a b)) (V2 b (mid b c)) (V2 c (mid c a))
