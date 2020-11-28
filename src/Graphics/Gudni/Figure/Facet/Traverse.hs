{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.Traverse
  ( traverseFacet
  , traverseFacetUntil
  )
where

import Graphics.Gudni.Base.Chain

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
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

limit :: Space s => s
limit = 1 / 16

insideBezierTri :: Space s => s -> Point2 s -> BezTri s -> Bool
insideBezierTri limit point =
  foldl1 (/=) .
  fmap (crossesBezierAlong False limit Vertical minBound (point ^. pX) (point ^. pY)) .
  bezTriToBeziers

traverseFacetUntil :: forall s . (Space s) => s -> s -> Point2 s -> Facet s -> Facet s
traverseFacetUntil limit flatness point =
  go
  where
  go :: Facet s -> Facet s
  go facet =
      if shouldSubdivideFacet flatness facet
      then go (traverseFacet limit point facet)
      else facet

traverseFacet :: (Space s) => s -> Point2 s -> Facet s -> Facet s
traverseFacet limit point facet =
  let output = facet ^. facetOutput
      sideOut = sideBezTris output
      centerOut = centerBezTri  sideOut

      input = facet ^. facetInput
      sideIn = sideTris input
      centerIn = centerTri input
      isInside = fmap (insideBezierTri limit point) sideOut
      next
         | isInside ^. _x = (Facet (sideOut ^. _x) (sideIn ^. _x))
         | isInside ^. _y = (Facet (sideOut ^. _y) (sideIn ^. _y))
         | isInside ^. _z = (Facet (sideOut ^. _z) (sideIn ^. _z))
         | otherwise      = (Facet  centerOut       centerIn     )
  in  next
