{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Segment
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Segments are a typesafe way to describe the components of a bezier curve sequence.
-- This module is utilities because segments are just regarded as a convenient way to describe raw outlines
-- for debugging etc. There are better ways to describe shapes.

module Graphics.Gudni.Util.Segment
  ( Segment (..)
  , anchor
  , control
  , pattern Straight
  , straightXY
  , pattern Curved
  , curvedXY
  , hasControl
  , overSegment
  , randomSegmentFromPoints
  , fromSegments
  , oldLine
  , segmentsToShape
  , segmentsToOutline
  , segmentsToOpenCurve
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Layout

import Graphics.Gudni.Util.CurvePair

import Control.Lens

import System.Random
import Control.Monad.Random
import Control.DeepSeq
import Data.Hashable
import qualified Data.Vector as V
import qualified Data.Foldable as F

-- | A segment is a typesafe way to describe on link in a quadradic bezier curve.
-- multiple on curves points can occur in sequence but multiple control points cannot.
data Segment s = Seg
  { _anchor  :: Point2 s
  , _control :: Maybe (Point2 s) }
  deriving (Eq, Ord)
makeLenses ''Segment

-- | Pattern synonym for a segment with no control point.
pattern Straight p = Seg p Nothing
-- | Make a straiight segment from the component dimensions
straightXY :: s -> s -> Segment s
straightXY x y = Seg (makePoint x y) Nothing

-- | Pattern synonym for a segment with a control point.
pattern Curved p c = Seg p (Just c)

-- | Make a curved segment from the component dimensions.
curvedXY :: s -> s -> s -> s -> Segment s
curvedXY x y cx cy = Seg (makePoint x y) (Just (makePoint cx cy))

-- | Map over the points of a segment.
overSegment :: (Point2 s -> Point2 z) -> Segment s -> Segment z
overSegment f (Seg o c) = Seg (f o) (fmap f c)

-- | Return true if the segment has a control point.
hasControl :: Segment s -> Bool
hasControl (Seg _ (Just _)) = True
hasControl (Seg _ Nothing ) = False

instance Random s => Random (Segment s) where
  random = runRand $ do o <- getRandom
                        v <- getRandom
                        if o
                        then do c <- getRandom
                                return $ Seg v (Just c)
                        else    return $ Seg v Nothing
  randomR (Seg p0 _, Seg p1 _) = runRand $ do o <- getRandom
                                              v <- getRandomR (p0, p1)
                                              if o
                                              then do c <- getRandomR (p0, p1)
                                                      return $ Seg v (Just c)
                                              else    return $ Seg v Nothing

-- | Generate a random segment from inside the boundaries of a range of points.
randomSegmentFromPoints :: (RandomGen g, Random s) => (Point2 s, Point2 s) -> g -> (Segment s, g)
randomSegmentFromPoints (p0, p1) = randomR (Seg p0 Nothing, Seg p1 Nothing)

-- | Convert a loop of Segments to a loop of CurvePairs.
segmentsToCurvePairs :: (Fractional s) => [Segment s] -> [CurvePair s]
segmentsToCurvePairs segments = segmentsToCurvePairs' (head segments ^. anchor) segments

segmentsToCurvePairs' :: (Fractional s) => Point2 s -> [Segment s] -> [CurvePair s]
segmentsToCurvePairs' first segs = case segs of
      (Seg v0 Nothing:[])             -> CurvePair v0 (mid v0 first):[]
      (Seg v0 Nothing:Seg v1 mC:rest) -> CurvePair v0 (mid v0 v1):segmentsToCurvePairs' first (Seg v1 mC:rest)
      (Seg v0 (Just c):rest)          -> CurvePair v0 c:segmentsToCurvePairs' first rest
      []                              -> []

segmentsToOpenCurve :: (Chain f, Fractional s) => [Segment s] -> Point2 s -> OpenCurve_ f s
segmentsToOpenCurve segments endPoint = makeOpenCurve . chainFromList $ go segments
   where
   go segments =
       if null segments
       then []
       else let (start, mControl, end) =
                    case segments of
                      (Seg v0 c:[]) -> (v0, c, endPoint)
                      (Seg v0 c:Seg v1 _:rest) -> (v0, c, v1)
                control =
                    case mControl of
                       Just c -> c
                       Nothing -> mid start end
            in Bez start control end : go (tail segments)

-- | Convert a list of lists of segments to a list of outlines.
segmentsToShape :: (Space s) => [[Segment s]] -> Shape s
segmentsToShape = Shape . map segmentsToOutline

-- | Convert a list of
segmentsToOutline :: (Loop f, Space s) => [Segment s] -> Outline_ f s
segmentsToOutline = Outline . pairsToBeziers . chainFromList . segmentsToCurvePairs

-- | Basic curve definition for a simple line (Temporary until stroke implemented.)
lineCurve :: (Space s) => s -> Point2 s -> Point2 s -> [Segment s]
lineCurve stroke p0 p1 =
  let vector = p0 ^-^ p1
      normal = vector ^/ norm vector
      leftNormal = rotate90 normal ^* stroke
      rightNormal = rotate270 normal ^* stroke
  in  [ Seg (p0 ^+^ rightNormal) Nothing
      , Seg (p0 ^+^ leftNormal ) Nothing
      , Seg (p1 ^+^ leftNormal ) Nothing
      , Seg (p1 ^+^ rightNormal) Nothing
      ]

oldLine thickness p0 p1 = segmentsToShape . pure $ lineCurve thickness p0 p1

-- | Typeclass for shape representations that can be created from a list of segments.
class HasFromSegments a where
  fromSegments :: [Segment (SpaceOf a)] -> a

instance (Loop f, Space s) => HasFromSegments (Outline_ f s) where
  fromSegments = segmentsToOutline

-- | Instance for creating a simple shape from a list of segments.
instance Space s => HasFromSegments (CompoundTree s) where
  fromSegments = mask . segmentsToShape . pure

instance NFData s => NFData (Segment s) where
  rnf (Seg o c) = o `deepseq` c `deepseq` ()
instance Hashable s => Hashable (Segment s) where
  hashWithSalt s (Seg o c) = s `hashWithSalt` o `hashWithSalt` c
instance Show s => Show (Segment s) where
  show (Seg o mc) = "V-" ++ show o ++ (case mc of Nothing -> ""; Just c -> "C-" ++ show c)
