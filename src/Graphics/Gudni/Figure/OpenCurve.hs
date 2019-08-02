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
import Linear.Affine
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

{-# INLINE terminated #-}
terminated :: OpenCurve s -> [(Segment s, Point2 s)]
terminated (OpenCurve [] _) = []
terminated (OpenCurve [s0] p2) = [(s0, p2)]
terminated (OpenCurve (s0 : ss@(Seg p2 _ : _)) terminator) =
    (s0, p2) : terminated (OpenCurve ss terminator)

-- | Returns a Bezier for each Segment in the input
{-# INLINE segmentLength #-}
segmentLength :: (Floating s, Ord s) => (Segment s, Point2 s) -> s
segmentLength (seg, p2) = case seg ^. control of
    Nothing -> distance (seg ^. anchor) p2
    Just c -> BZ.arcLength (V3 (seg ^. anchor) c p2)

{-# SPECIALIZE arcLength :: OpenCurve Float -> Float #-}
{-# SPECIALIZE arcLength :: OpenCurve Double -> Double #-}
arcLength :: (Floating s, Ord s) => OpenCurve s -> s
arcLength = sum . map segmentLength . terminated

-- | returns Nothing if the parameter exceeds the length of @curve@.
projectPoint :: (Floating s, RealFrac s, Ord s, Epsilon s) =>
    s -> OpenCurve s -> Point2 s -> Maybe (Point2 s)
projectPoint accuracy curve (P (V2 overall_length offset)) =
    let
        -- find the segment containing the point we want
        segments = terminated curve
        m_seg = pickSeg overall_length segments
        pickSeg remaining [] = Nothing
        pickSeg remaining (s : ss) =
            let l = segmentLength s in
                if l < remaining
                then pickSeg (remaining - l) ss
                else Just (remaining, s)
    in case m_seg of
        Nothing -> Nothing
        Just (remaining_length, (Seg anchor m_control, endpoint)) -> case m_control of
            -- if segment is a line, exact calculation
            Nothing -> Just (onCurve .+^ (offset *^ normal)) where
              t = remaining_length / distance anchor endpoint
              onCurve = lerp t endpoint anchor
              normal = normalize (perp (endpoint .-. anchor))
            -- if if segment is a curve, call inverseBezierArcLength
            Just c -> Just (onCurve .+^ (offset *^ normal)) where
              bz = V3 anchor c endpoint
              (_, V3 onCurve tangent _) = BZ.split bz
                  (BZ.inverseArcLength accuracy bz remaining_length)
              normal = normalize (perp (tangent .-. onCurve))
