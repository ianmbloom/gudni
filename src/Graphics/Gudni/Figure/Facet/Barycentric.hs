{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Facet.Barycentric
  ( inverseFacet
  , inverseTriangle
  , inverseBarycentric
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Facet.Type
import qualified Graphics.Gudni.Figure.Facet.BezierTriangle as BT
import qualified Graphics.Gudni.Figure.Facet.Triangle as TR
import Graphics.Gudni.Figure.Facet.Traverse

import Graphics.Gudni.Util.Debug

import Linear.V2
import Linear.V3
import Linear.Vector
import Linear.Metric

import Control.Applicative
import Control.Lens

inverseFacet :: (Space s) => Facet s -> Point2 s -> Point2 s
inverseFacet facet p = flip inverseTriangle p . traverseFacet p $ facet

inverseTriangle :: (Space s) => Facet s -> Point2 s -> Point2 s
inverseTriangle (Facet output input) p =
  let o  = BT.p0 output
      ou = BT.p1 output - o
      ov = BT.p2 output - o
      p' = p - o
      pu = project p' ou
      pv = project p' ov
      to = TR.t0 input
      tu = TR.t1 input - to
      tv = TR.t2 input - to
  in  to + (pu * tu + pv * tv)

inverseBarycentric :: (Space s) => TR.Tri s -> Point2 s -> Point2 s
inverseBarycentric output p =
  let o  = TR.t0 output
      ou = TR.t1 output
      ov = TR.t2 output
      p' = p - o
      pu = project p' ou
      pv = project p' ov
  in  pu + pv
