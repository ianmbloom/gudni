{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.Subdivide
  ( subdivideFacetSteps
  , tesselateFacet
  , subdivideFacet
  )
where

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Figure.Facet.Facet

import Graphics.Gudni.Base.Chain
import Graphics.Gudni.Util.Debug

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Foldable
import qualified Data.Vector as V


import Text.PrettyPrint.GenericPretty
import Control.Lens

subdivideFacetSteps :: forall s
                    .  (Space s)
                    => Int
                    -> Facet_ s
                    -> [Facet_ s]
subdivideFacetSteps steps =
  go steps
  where
  go :: Int -> Facet_ s -> [Facet_ s]
  go steps facet =
     if steps > 0
     then join . fmap (go (steps - 1)) . toList . subdivideFacet $ facet
     else pure facet

tesselateFacet :: forall s
               .  (Space s)
               => s
               -> Facet_ s
               -> [Facet_ s]
tesselateFacet tolerance =
  go
  where
  go :: Facet_ s -> [Facet_ s]
  go facet =
      let subFacets = subdivideFacetSteps 1 facet
          (continued, done) = segregate (shouldSubdivideFacet tolerance) subFacets
      in  done <|> if null continued then []  else join . fmap go $ continued

instance (Out a) => Out (V4 a)

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

u0 :: V3 a -> a
u0 = view _x
u1 :: V3 a -> a
u1 = view _y
u2 :: V3 a -> a
u2 = view _z

sideTexture :: Space s => V3 (Point2 s) -> V3 (Point2 s)
sideTexture i =
   fmap (0.5 *^) (pure (u0 i) + i)

centerTexture :: Space s => V3 (Point2 s) -> V3 (Point2 s)
centerTexture i = fmap (0.5 *^) (rotateV3 i + (rotateV3 . rotateV3) i)


reverseTri i = V3 (V2 (p0 i) (c2 i))
                  (V2 (p2 i) (c1 i))
                  (V2 (p1 i) (c0 i))

centerTriangle bezTri =
  V3 (side1                         bezTri)
     (side1 . rotateV3 . rotateV3 $ bezTri)
     (side1 . rotateV3            $ bezTri)

subdivideFacet :: (Space s) => Facet_ s -> V4 (Facet_ s)
subdivideFacet facet =
  let bezTri = facet ^. sceneSide
      aI = sideTriangle                           bezTri
      bI = sideTriangle   . rotateV3            $ bezTri
      cI = sideTriangle   . rotateV3 . rotateV3 $ bezTri
      dI = centerTriangle                         bezTri

      texTri = facet ^. textureSide
      aT = sideTexture                         texTri
      bT = sideTexture . rotateV3 . rotateV3 $ texTri
      cT = sideTexture . rotateV3            $ texTri
      dT = centerTexture                       texTri
  in  V4 (Facet aI aT)
         (Facet bI bT)
         (Facet cI cT)
         (Facet dI dT)
