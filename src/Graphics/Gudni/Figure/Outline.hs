{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Graphics.Gudni.Figure.Outline
  ( CurvePair
  , pattern CurvePair
  , onCurve
  , offCurve
  , pairPoints
  , Outline(..)
  , mapOutline
  , segmentsToOutline
  , openCurveToOutline
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

import Data.Hashable
import Control.DeepSeq

newtype CurvePair s = Cp {_unCp :: V2 (Point2 s)} deriving (Eq, Ord, Show, Num)
makeLenses ''CurvePair
pattern CurvePair a b = Cp (V2 a b)

onCurve :: Lens' (CurvePair s) (Point2 s)
onCurve = unCp . _x
offCurve :: Lens' (CurvePair s) (Point2 s)
offCurve = unCp . _y

mapCurvePair :: (Point2 s -> Point2 z) -> CurvePair s -> CurvePair z
mapCurvePair f (Cp v2) = Cp (fmap f v2)

pairPoints :: [Point2 s] -> [CurvePair s]
pairPoints (v0:v1:rest) = (CurvePair v0 v1):pairPoints rest
pairPoints [] = []
pairPoints [v0] = []

data Outline s = Outline [CurvePair s]
               deriving (Eq, Ord, Show)

mapOutline :: (Point2 s -> Point2 z) -> Outline s -> Outline z
mapOutline f (Outline ps) = Outline (map (mapCurvePair f) ps)

mid :: (Fractional s, Num s) => Point2 s -> Point2 s -> Point2 s
mid v0 v1 = lerp 0.5 v0 v1

segmentsToCurvePairs segments = segmentsToCurvePairs' (head segments ^. anchor) segments

segmentsToCurvePairs' :: (Fractional s) => (Point2 s) -> [Segment s] -> [CurvePair s]
segmentsToCurvePairs' first segs = case segs of
      (Seg v0 Nothing:[])             -> CurvePair v0 (mid v0 first):[]
      (Seg v0 Nothing:Seg v1 mC:rest) -> CurvePair v0 (mid v0 v1):segmentsToCurvePairs' first (Seg v1 mC:rest)
      (Seg v0 (Just c):rest)          -> CurvePair v0 c:segmentsToCurvePairs' first rest
      []                              -> []


segmentsToOutline :: (Fractional s) => [[Segment s]] -> [Outline s]
segmentsToOutline = map (Outline . segmentsToCurvePairs)

openCurveToOutline :: (Fractional s, Eq s) => OpenCurve s -> [Outline s]
openCurveToOutline curve =
  let segments = if curve ^. terminator == curve ^. outset
                 then curve ^. curveSegments -- if the beggining of the curve is the same as the end, ignore the end
                 else Straight (curve ^. terminator) : curve ^. curveSegments -- else insert a straight segment from the end to the beggining.
  in segmentsToOutline [segments]


instance Boxable (CurvePair DisplaySpace) where
  getBoundingBox (CurvePair a b) =
      let left   = min (a ^. pX) (b ^. pX)
          top    = min (a ^. pY) (b ^. pY)
          right  = max (a ^. pX) (b ^. pX)
          bottom = max (a ^. pY) (b ^. pY)
      in  makeBox left top right bottom

instance Boxable (Outline DisplaySpace) where
  getBoundingBox (Outline vs) = getBoundingBox . map getBoundingBox $ vs

instance (Num s) => SimpleTransformable Outline s where
  tTranslate p = mapOutline (tTranslate p)
  tScale     s = mapOutline (tScale s)
instance (Floating s, Num s) => Transformable Outline s where
  tRotate    a = mapOutline (tRotate a)

-- * Instances

instance NFData s => NFData (CurvePair s) where
  rnf (Cp v2) = v2 `deepseq` ()

instance NFData s => NFData (Outline s) where
  rnf (Outline ps) = ps `deepseq` ()

instance Hashable p => Hashable (CurvePair p) where
  hashWithSalt s (Cp v2) = s `hashWithSalt` v2
instance Hashable s => Hashable (Outline s) where
  hashWithSalt s (Outline ps) = s `hashWithSalt` ps
