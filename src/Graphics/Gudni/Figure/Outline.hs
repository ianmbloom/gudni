{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Angle
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for defining Outlines which are closed bezier curves that are the component
-- parts of shapes.

module Graphics.Gudni.Figure.Outline
  ( CurvePair
  , pattern CurvePair
  , onCurve
  , offCurve
  , pairPoints
  , Outline(..)
  , mapOutline
  , segmentsToOutline
  , closeOpenCurve
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Segment
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Util.Util
import Control.Lens
import Linear.V2
import qualified Data.Vector as V

import Data.Hashable
import Control.DeepSeq

-- | A CurvePair is a representation of two points along an outline∘
-- The control point is called offCurve and in the case of a straight segment the point just colinear with
-- the onCurve points before and after it. This is just internal, user defined outlines should be specified as
-- sequences of Segments.
newtype CurvePair s = Cp {_unCp :: V2 (Point2 s)} deriving (Eq, Ord, Num)
makeLenses ''CurvePair
pattern CurvePair a b = Cp (V2 a b)

instance Show s => Show (CurvePair s) where
  show (CurvePair a b) = " O(" ++ show (a ^. pX) ++ "," ++ show (a ^. pY) ++ ")" -- ++" X(" ++ show (b ^. pX) ++ "," ++ show (b ^. pY) ++ ")"

-- | Lens for the anchor or on-curve point.
onCurve :: Lens' (CurvePair s) (Point2 s)
onCurve = unCp . _x
-- | Lens for the control or off-curve point.
offCurve :: Lens' (CurvePair s) (Point2 s)
offCurve = unCp . _y

-- | Map over both points in a CurvePair.
mapCurvePair :: (Point2 s -> Point2 z) -> CurvePair s -> CurvePair z
mapCurvePair f (Cp v2) = Cp (fmap f v2)

-- | Make every two points in a list of points into a CurvePair. Useful for raw glyph data.
pairPoints :: [Point2 s] -> [CurvePair s]
pairPoints (v0:v1:rest) = (CurvePair v0 v1):pairPoints rest
pairPoints [] = []
pairPoints [v0] = []

-- | An outline is just a wrapper for a list of CurvePairs. It represents one curve loop∘
-- A shape is defined by a list of outlines.
newtype Outline s = Outline (V.Vector (CurvePair s))
               deriving (Eq, Ord)

instance Show s => Show (Outline s) where
  show (Outline vs) = "Outline" ++ show vs

-- | Map over every point in an outline.
mapOutline :: (Point2 s -> Point2 z) -> Outline s -> Outline z
mapOutline f (Outline ps) = Outline (V.map (mapCurvePair f) ps)

-- | Make a mid point from two points.
mid :: (Fractional s, Num s) => Point2 s -> Point2 s -> Point2 s
mid v0 v1 = lerp 0.5 v0 v1

-- | Convert a loop of Segments to a loop of CurvePairs.
segmentsToCurvePairs :: (Fractional s) => [Segment s] -> [CurvePair s]
segmentsToCurvePairs segments = segmentsToCurvePairs' (head segments ^. anchor) segments

segmentsToCurvePairs' :: (Fractional s) => Point2 s -> [Segment s] -> [CurvePair s]
segmentsToCurvePairs' first segs = case segs of
      (Seg v0 Nothing:[])             -> CurvePair v0 (mid v0 first):[]
      (Seg v0 Nothing:Seg v1 mC:rest) -> CurvePair v0 (mid v0 v1):segmentsToCurvePairs' first (Seg v1 mC:rest)
      (Seg v0 (Just c):rest)          -> CurvePair v0 c:segmentsToCurvePairs' first rest
      []                              -> []

-- | Convert a list of lists of segments to a list of outlines.
segmentsToOutline :: (Fractional s) => [[Segment s]] -> [Outline s]
segmentsToOutline = map (Outline . V.fromList . segmentsToCurvePairs)

-- | Close an open curve and convert it to an outline. An additional straight segment is added if the outset and the terminator of
-- the curve are not the same.
closeOpenCurve :: (Fractional s, Eq s) => OpenCurve s -> [Segment s]
closeOpenCurve curve =
  let segments = if curve ^. terminator == curve ^. outset
                 then curve ^. curveSegments -- if the beggining of the curve is the same as the end, ignore the end
                 else Straight (curve ^. terminator) : curve ^. curveSegments -- else insert a straight segment from the end to the beggining.
  in  segments

-- * Instances

instance (SimpleSpace s) => HasSpace (CurvePair s) where
  type SpaceOf (CurvePair s) = s

instance (SimpleSpace s) => HasBox (CurvePair s) where
  boxOf (CurvePair c o) = minMaxBox (boxOf c) (boxOf o)

instance (SimpleSpace s) => HasSpace (Outline s) where
  type SpaceOf (Outline s) = s

--instance (Bounded s, Ord s, Num s) => HasSpace (V.Vector (CurvePair s)) where
--  type SpaceOf (V.Vector (CurvePair s)) = s

instance (SimpleSpace s) => HasBox (Outline s) where
  boxOf (Outline vs) = minMaxBoxes . fmap boxOf $ vs

instance (Space s) => SimpleTransformable (Outline s) where
  tTranslate p = mapOutline (tTranslate p)
  tScale     s = mapOutline (tScale s)
instance (Space s) => Transformable (Outline s) where
  tRotate    a = mapOutline (tRotate a)

instance NFData s => NFData (CurvePair s) where
  rnf (Cp v2) = v2 `deepseq` ()

instance NFData s => NFData (Outline s) where
  rnf (Outline ps) = ps `deepseq` ()

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt s vector = V.foldl hashWithSalt s vector
instance Hashable p => Hashable (CurvePair p) where
  hashWithSalt s (Cp v2) = s `hashWithSalt` v2
instance Hashable s => Hashable (Outline s) where
  hashWithSalt s (Outline ps) = s `hashWithSalt` ps
