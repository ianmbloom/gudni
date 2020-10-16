{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE RankNTypes          #-}

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

module Graphics.Gudni.Figure.Deknob.Class
  ( CanDeknob(..)
  )
where

import Linear
import Graphics.Gudni.Figure.Principle.Space
import Graphics.Gudni.Figure.Principle.Axis
import Graphics.Gudni.Figure.Principle.Point
import Graphics.Gudni.Util.Debug
import qualified Data.Vector as V
import Data.Maybe

import Control.Lens
import Control.Applicative

-- * Remove Knobs
-- Knobs are segments of a curve where the curve point is outside of the horizontal range of the anchor points.
-- In other words the curve bulges out in the x direction.
-- It must be split into two curves that do not bulge out by finding the on curve point that is as close as possible
-- to the verticle tangent point.

-- | Given two onCurve points and a controlPoint. Find two control points and an on-curve point between them
-- by bifercating according to the parameter t.
-- curvePoint :: Num s => s -> Bezier s -> Bezier s
-- curvePoint t (Bez v0 control v1) =
--   let mid0     = between t v0      control
--       mid1     = between t control v1
--       onCurve  = between t mid0    mid1
--   in  (Bez mid0 onCurve mid1)

class HasSpace t => CanDeknob t where
    deKnob :: (Alternative f, Axis axis) => axis -> t -> Maybe (f t)
    replaceKnob :: (Alternative f, Axis axis) => axis -> t -> f t
    default replaceKnob :: (Alternative f, Axis axis) => axis -> t -> f t
    replaceKnob axis b = fromMaybe (pure b) (deKnob axis b)
