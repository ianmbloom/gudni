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
  , arcLength
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Segment
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Transformer
import qualified Graphics.Gudni.Figure.Bezier as BZ

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

-- | Returns a Bezier for each Segment in the input
segmentLengths :: (Floating s, Ord s) => OpenCurve s -> [s]
segmentLengths (OpenCurve [] _) = []
segmentLengths (OpenCurve (s0 : ss) terminator) =
    length0 : segmentLengths (OpenCurve ss terminator) where
        length0 = case s0 ^. control of
            Nothing -> distance (s0 ^. anchor) p2
            Just c -> BZ.arcLength (V3 (s0 ^. anchor) c p2)
        p2 = case ss of
            (Seg p2 _ : _) -> p2
            [] -> terminator

{-# SPECIALIZE arcLength :: OpenCurve Float -> Float #-}
{-# SPECIALIZE arcLength :: OpenCurve Double -> Double #-}
arcLength :: (Floating s, Ord s) => OpenCurve s -> s
arcLength = sum . segmentLengths

-- TODO inverseArcLength
    -- find the segment containing the point we want
    -- find the endpoint of the segment
    -- if segment is a line, exact calculation
    -- if if segment is a curve, call inverseBezierArcLength
