{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Graphics.Gudni.Figure.CurvePair
  ( CurvePair(..)
  , pattern CurvePair
  , onCurve
  , offCurve
  , pairPoints
  , pairsToBeziers
  , beziersToPairs
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Util.Loop

import Control.Lens
import Linear.V2
import Control.DeepSeq
import Data.Hashable
import qualified Data.Vector as V

-- | A CurvePair is a representation of two points along a shape or path.∘
-- The control point is called offCurve and in the case of a line segment the point just colinear with
-- the onCurve points before and after it. This is just internal, user defined shape should be specified as
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

-- Make a bezier section from two adjacent curve pairs.
makeBezier :: CurvePair s -> CurvePair s -> Bezier s
makeBezier a b = Bez (a ^. onCurve) (a ^. offCurve) (b ^. onCurve)

-- | Turn a sequence of curve pairs into a sequence of curve sections (called beziers)
pairsToBeziers :: Loop t => t (CurvePair s) -> t (Bezier s)
pairsToBeziers  = overLoopNeighbors makeBezier

bezierToCurvePair :: Bezier s -> CurvePair s
bezierToCurvePair (Bez v0 c _) = CurvePair v0 c

beziersToPairs :: Functor t => t (Bezier s) -> t (CurvePair s)
beziersToPairs = fmap bezierToCurvePair

instance (SimpleSpace s) => HasSpace (CurvePair s) where
  type SpaceOf (CurvePair s) = s

instance (SimpleSpace s) => HasBox (CurvePair s) where
  boxOf (CurvePair c o) = minMaxBox (boxOf c) (boxOf o)

instance NFData s => NFData (CurvePair s) where
  rnf (Cp v2) = v2 `deepseq` ()
instance Hashable p => Hashable (CurvePair p) where
  hashWithSalt s (Cp v2) = s `hashWithSalt` v2
