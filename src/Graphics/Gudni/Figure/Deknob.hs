{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Constants
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for removing "knobs" which are vertical curves that cannot be easily
-- rasterized.

module Graphics.Gudni.Figure.Deknob
  ( CanDeKnob(..)
  )
where

import Linear
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Util.Debug
import qualified Data.Vector as V

import Control.Lens
import Control.Applicative

-- * Remove Knobs
-- Knobs are segments of a curve where the curve point is outside of the horizontal range of the anchor points.
-- In other words the curve bulges out in the x direction.
-- It must be split into two curves that do not bulge out by finding the on curve point that is as close as possible
-- to the verticle tangent point.

-- | Constant for bifercating exactly in half.
sPLIT :: (Fractional s, Num s) => s
sPLIT = 1 / 2

-- | Given two onCurve points and a controlPoint. Find two control points and an on-curve point between them
-- by bifercating according to the parameter t.
curvePoint :: Num s => s -> Bezier s -> Bezier s
curvePoint t (Bez v0 control v1) =
  let mid0     = lerp (1-t) v0      control
      mid1     = lerp (1-t) control v1
      onCurve  = lerp (1-t) mid0    mid1
  in  (Bez mid0 onCurve mid1)

-- | Given two onCurve points and a controlPoint. Find two control points and an on-curve point between them
-- by bifercating according to the parameter t.
-- curvePoint :: Num s => s -> Bezier s -> Bezier s
-- curvePoint t (Bez v0 control v1) =
--   let mid0     = between t v0      control
--       mid1     = between t control v1
--       onCurve  = between t mid0    mid1
--   in  (Bez mid0 onCurve mid1)


class HasSpace t => CanDeKnob t where
  deKnob :: (Alternative f) => t -> f t

axis = (^. pX)

instance (Space s) => CanDeKnob (Bezier s) where
   -- | If a curve is a knob, split it.
   deKnob bz@(Bez v0 control v1) =
       -- If both sides are convex in the left direction.
       if ((abs (axis v0 - axis control) + abs (axis control - axis v1)) - abs (axis v0 - axis v1)) /= 0
       then-- This is a bulging knob. Split it while one part maintains that bulge.
           let f t = let (Bez mid0 onCurve mid1) = curvePoint t bz
                     in  (abs (axis onCurve - axis v0) - (abs (axis onCurve - axis mid0) + abs (axis mid0 - axis v0))) -
                         (abs (axis onCurve - axis v1) - (abs (axis onCurve - axis mid1) + abs (axis mid1 - axis v1)))
               t = findSplit f
               (left, right) = splitBezier t bz
           -- And return the two resulting curves.
           in pure left <|> pure right
       else -- Otherwise return the curve unharmed.
            pure (Bez v0 control v1)
