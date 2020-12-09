{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.Cross
  ( crossesFacetAlong
  , crossesFacet
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Facet.Type
import Graphics.Gudni.Figure.Facet.BezierTriangle

import Control.Lens

crossesFacetAlong :: forall axis s
                  . (Axis axis, Space s)
                  => s
                  -> axis
                  -> Along axis s
                  -> Athwart axis s
                  -> Along axis s
                  -> Facet s
                  -> Bool
crossesFacetAlong limit axis start baseline end facet =
   foldl1 (/=) .
   fmap (crossesBezierAlong limit axis start baseline end) .
   bezTriToBeziers .
   view facetOutput $
   facet

crossesFacet :: (Space s) => s -> Point2 s -> Point2 s -> Facet s -> Bool
crossesFacet limit start end facet =
    let iP = interimPoint start end
    in
    crossesFacetAlong limit Vertical   (start ^. pY) (start ^. pX) (iP  ^. pY) facet /=
    crossesFacetAlong limit Horizontal (iP    ^. pX) (iP    ^. pY) (end ^. pX) facet
