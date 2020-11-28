{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.WithinBox
  ( facetIsWithinBox
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier.WithinBox
import Graphics.Gudni.Figure.Facet.Type
import Graphics.Gudni.Figure.Facet.BezierTriangle
import Graphics.Gudni.Util.Debug

import Control.Lens

facetIsWithinBox :: Space s
                 => s
                 -> Box s
                 -> Facet s
                 -> Bool
facetIsWithinBox limit boundary facet =
   or .
   fmap (bezierIsWithinBox limit boundary) .
   bezTriToBeziers .
   view facetOutput $
   facet
