{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
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
  , CanProject(..)
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
import Data.Traversable

-- | An (almost) typesafe representation of an open bezier curve.
data OpenCurve s = OpenCurve
    { _curveSegments :: [Segment s]
    , _terminator    :: (Point2 s)
    } deriving (Show, Eq, Ord)
makeLenses ''OpenCurve

instance (SimpleSpace s) => HasSpace (OpenCurve s) where
    type SpaceOf (OpenCurve s) = s

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

projectPoint :: (Floating s, RealFrac s, Ord s, Epsilon s) =>
    Int -> Maybe s -> OpenCurve s -> Point2 s -> Point2 s
projectPoint max_steps m_accuracy curve (P (V2 overall_length offset)) =
    let
        -- find the segment containing the point we want
        segments = terminated curve
        pickSeg remaining [] = error "unreachable empty list in projectPoint"
        pickSeg remaining (s : ss) =
            let l = segmentLength s in
                case (l < remaining, ss) of
                    (False, _) -> (remaining, s)
                    (True, []) -> let
                        (Seg p1 m_control, p2) = s
                        v = case m_control of
                            Just control -> p2 .-. control
                            Nothing -> p2 .-. p1
                      in (remaining - l, (Seg p2 Nothing, p2 .+^ negated v))
                    (True, _) -> pickSeg (remaining - l) ss
        (remaining_length, (Seg anchor m_control, endpoint)) = pickSeg overall_length segments
    in case m_control of
        -- if segment is a line, exact calculation
        Nothing -> (onCurve .+^ (offset *^ normal)) where
            t = remaining_length / distance anchor endpoint
            onCurve = lerp t endpoint anchor
            normal = normalize (perp (endpoint .-. anchor))
        -- if if segment is a curve, call inverseBezierArcLength
        Just c -> (onCurve .+^ (offset *^ normal)) where
            bz = V3 anchor c endpoint
            (_, V3 onCurve tangent _) = BZ.split bz
                (BZ.inverseArcLength max_steps m_accuracy bz remaining_length)
            normal = normalize (perp (tangent .-. onCurve))

-- | In most cases, it is sufficient to define
-- @projectWithStepsAccuracy@, and use default implementations for the
-- remaining functions.  You may also want to define a default
-- accuracy by overriding @project@.
class CanProject t where
  project :: OpenCurve (SpaceOf t) -> t -> t
  default project ::
      (SpaceOf t ~ s, Floating s, RealFrac s) => OpenCurve (SpaceOf t) -> t -> t
  project = projectWithAccuracy 1e-3

  projectWithAccuracy :: SpaceOf t -> OpenCurve (SpaceOf t) -> t -> t
  default projectWithAccuracy ::
      (SpaceOf t ~ s, Floating s, RealFrac s) => SpaceOf t -> OpenCurve (SpaceOf t) -> t -> t
  projectWithAccuracy accuracy =
      projectWithStepsAccuracy (BZ.maxStepsFromAccuracy accuracy) (Just accuracy)

  projectWithSteps :: Int -> OpenCurve (SpaceOf t) -> t -> t
  projectWithSteps max_steps = projectWithStepsAccuracy max_steps Nothing

  projectWithStepsAccuracy :: s ~ SpaceOf t => Int -> Maybe s -> OpenCurve s -> t -> t

instance (SpaceOf (OpenCurve s) ~ s, Floating s, RealFrac s, Ord s, Epsilon s) =>
    CanProject (OpenCurve s) where
    projectWithStepsAccuracy max_steps m_accuracy path (OpenCurve ss terminator)  =
        OpenCurve segments (proj terminator) where
      segments = ss <&> \(Seg p1 c) -> Seg (proj p1) (proj <$> c)
      proj = projectPoint max_steps m_accuracy path
