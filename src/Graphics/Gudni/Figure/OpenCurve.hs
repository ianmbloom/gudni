{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.OpenCurve
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for defining and combining open curves.

module Graphics.Gudni.Figure.OpenCurve
  ( OpenCurve(..)
  , curveSegments
  , terminator
  , outset
  , (<^>)
  , reverseCurve
  , overCurve
  , bezierArcLength
  , arcLength
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Segment
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Transformer

import Graphics.Gudni.Util.Debug

import Linear
import Data.Maybe
import Data.Fixed
import Data.Hashable
import qualified Data.Map as M

import Control.Monad.State
import Control.DeepSeq
import Control.Applicative
import Control.Lens

-- | An (almost) typesafe representation of an open bezier curve.
data OpenCurve s = OpenCurve
    { _curveSegments :: [Segment s]
    , _terminator    :: (Point2 s)
    } deriving (Show, Eq, Ord)
makeLenses ''OpenCurve

-- | The first point on the curve of an open curve or the terminator if it only has one point.
outset :: Lens' (OpenCurve s) (Point2 s)
outset elt_fn (OpenCurve segments terminator) =
  let (Seg p mc) = head segments
  in  (\p' -> OpenCurve (Seg p' mc:tail segments) terminator) <$> elt_fn p

-- | Map over every point in an OpenCurve
overCurve :: (Point2 s -> Point2 z) -> OpenCurve s -> OpenCurve z
overCurve f (OpenCurve segs term) = OpenCurve (map (overSegment f) segs) (f term)


pullSegments :: Segment s -> Segment s -> Segment s
pullSegments (Seg o0 c0) (Seg o1 c1) = Seg o1 c0

-- | Map a function f over every consecutive pair in a list of one or more elements.
acrossPairs f (a:b:cs) = f a b : acrossPairs f (b:cs)
acrossPairs f (a:[]) = []
acrossPairs f [] = error "list must have on or more elements"

-- | Return the same curve in the opposite order.
reverseCurve :: OpenCurve s -> OpenCurve s
reverseCurve (OpenCurve segments terminator) =
  let terminal = head segments ^. anchor
      extendSegments = segments ++ [Seg terminator Nothing]
      revCurve = reverse . acrossPairs pullSegments $ segments
  in  OpenCurve revCurve terminal

-- | Connect two curves end to end by translating c1 so that the starting point of 'c1' is equal to the terminator of 'c0'
(<^>) :: Space s => OpenCurve s -> OpenCurve s -> OpenCurve s
(<^>) c0 c1 = let delta = c0 ^. terminator ^-^ c1 ^. outset
                  transC1 = overCurve (tTranslate delta) c1
              in  over curveSegments (c0 ^. curveSegments ++) transC1

instance Hashable s => Hashable (OpenCurve s) where
    hashWithSalt s (OpenCurve segs term) = s `hashWithSalt` segs `hashWithSalt` term

instance NFData s => NFData (OpenCurve s) where
    rnf (OpenCurve a b) = a `deepseq` b `deepseq` ()

-- | Arc length of a single quadratic Bézier segment.
-- From https://github.com/linebender/kurbo
-- This computation is based on an analytical formula. Since that formula suffers
-- from numerical instability when the curve is very close to a straight line, we
-- detect that case and fall back to Legendre-Gauss quadrature.
bezierArcLength :: (Floating s, Ord s) => Point2 s -> Point2 s -> Point2 s -> s
bezierArcLength p0 p1 p2 = let
    d2 = p0 - 2.0 * p1 + p2
    a = quadrance d2
    d1 = p1 - p0
    c = quadrance d1
    in if a < 5e-4 * c
    then
        -- This case happens for nearly straight Béziers.
        --
        -- Calculate arclength using Legendre-Gauss quadrature using formula from Behdad
        -- in https:--github.com/Pomax/BezierInfo-2/issues/77
        let
            v0 = norm
                (-0.492943519233745 *^ p0
                + 0.430331482911935 *^ p1
                + 0.0626120363218102 *^ p2)
            v1 = norm ((p2 - p0) ^* 0.4444444444444444)
            v2 = norm
                (-0.0626120363218102 *^ p0
                - 0.430331482911935 *^ p1
                + 0.492943519233745 *^ p2)
        in v0 + v1 + v2
    else
        let
            b = 2.0 * dot d2 d1

            sabc = sqrt (a + b + c)
            a2 = a ** (-0.5)
            a32 = a2 ^ 3
            c2 = 2.0 * sqrt c
            ba_c2 = b * a2 + c2

            v0 = 0.25 * a2 * a2 * b * (2.0 * sabc - c2) + sabc
        in if ba_c2 < 1e-13 -- TODO: justify and fine-tune this exact constant.
            then v0 -- This case happens for Béziers with a sharp kink.
            else
            v0 + 0.25
                * a32
                * (4.0 * c * a - b * b)
                * log (((2.0 * a + b) * a2 + 2.0 * sabc) / ba_c2)

arcLength :: (Floating s, Ord s) => OpenCurve s -> s
arcLength (OpenCurve [] _) = 0
arcLength (OpenCurve (s0 : ss) terminator) =
    length0 + arcLength (OpenCurve ss terminator) where
        length0 = case s0 ^. control of
            Nothing -> distance (s0 ^. anchor) p2
            Just c -> bezierArcLength (s0 ^. anchor) c p2
        p2 = case ss of
            (Seg p2 _ : _) -> p2
            [] -> terminator
