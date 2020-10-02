{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.Traverse
  ( traverseFacet
  , traverseFacetUntil
  )
where

import Graphics.Gudni.Base.Chain

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Figure.Facet.Triangle
import Graphics.Gudni.Figure.Facet.BezierTriangle
import Graphics.Gudni.Figure.Facet.Type
import Graphics.Gudni.Figure.Facet.Subdivide

import Linear.V3
import Data.Foldable
import Control.Lens

insideBox :: Space s
          => Point2 s
          -> Box s
          -> Bool
insideBox p box =
       box ^. leftSide   <= p ^. pX
    && box ^. topSide    <= p ^. pY
    && box ^. rightSide  >  p ^. pX
    && box ^. bottomSide >  p ^. pY

couldContain :: Space s => Point2 s -> Facet_ s -> Bool
couldContain p facet = insideBox p . boxOf $ facet

limit :: Space s => s
limit = 1 / 16

sizeLimit :: Space s => Facet_ s -> Bool
sizeLimit facet =
  let box = boxOf facet
  in (box ^. heightBox > toAlong Vertical limit) || (box ^. widthBox > toAlong Horizontal limit)

insideBezierTri :: Space s => Point2 s -> BezTri s -> Bool
insideBezierTri point =
  foldl1 (/=) .
  fmap (crossesAlong Vertical minBound (point ^. pX) (point ^. pY)) .
  bezTriToBeziers

traverseFacetUntil :: forall s . (Space s) => s -> Point2 s -> Facet_ s -> Facet_ s
traverseFacetUntil threshold point =
  go
  where
  go :: Facet_ s -> Facet_ s
  go facet =
    let potential = traverseFacet point facet
    in  if shouldSubdivideFacet threshold potential
        then go potential
        else facet

traverseFacet :: (Space s) => Point2 s -> Facet_ s -> Facet_ s
traverseFacet point facet =
  let output = facet ^. facetOutput
      sideOut = sideBezTris output
      centerOut = centerBezTri  sideOut

      input = facet ^. facetInput
      sideIn = sideTris input
      centerIn = centerTri input
      isInside = fmap (insideBezierTri point) sideOut
      next
         | isInside ^. _x = (Facet (sideOut ^. _x) (sideIn ^. _x))
         | isInside ^. _y = (Facet (sideOut ^. _y) (sideIn ^. _y))
         | isInside ^. _z = (Facet (sideOut ^. _z) (sideIn ^. _z))
         | otherwise      = (Facet  centerOut       centerIn     )
  in  next
