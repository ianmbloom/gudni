{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  ( OpenCurve_(..)
  , OpenCurve(..)
  , makeOpenCurve
  , curveSegments
  , terminator
  , outset
  , (>*<)
  , reverseCurve
  , overCurvePoints
  )
where

import Prelude hiding (reverse)

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Projection
import qualified Graphics.Gudni.Figure.Bezier as BZ
--import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Debug

import Linear
import Linear.Affine
import Data.Maybe
import Data.Fixed
import Data.Hashable
import qualified Data.Map as M
import qualified Data.Vector as V

import Control.Monad.State
import Control.DeepSeq
import Control.Applicative
import Control.Lens
import Data.Traversable
import Data.Foldable

-- | An (almost) typesafe representation of an open bezier curve.
data OpenCurve_ t s = OpenCurve
    { _curveSegments :: t (Bezier s)
    }
makeLenses ''OpenCurve_

makeOpenCurve :: Foldable t => t (Bezier s) -> OpenCurve s
makeOpenCurve = OpenCurve . V.fromList . toList

deriving instance (Show (t (Bezier s))) => Show (OpenCurve_ t s)
deriving instance (Eq   (t (Bezier s))) => Eq (OpenCurve_ t s)
deriving instance (Ord  (t (Bezier s))) => Ord (OpenCurve_ t s)

type OpenCurve s = OpenCurve_ V.Vector s

instance (Space s) => HasSpace (OpenCurve_ t s) where
    type SpaceOf (OpenCurve_ t s) = s

-- {-# SPECIALIZE arcLength :: OpenCurve Float  -> Float  #-}
-- {-# SPECIALIZE arcLength :: OpenCurve Double -> Double #-}
instance (Functor t, Foldable t, Space s) => HasArcLength (OpenCurve_ t s) where
    arcLength = sum . fmap arcLength . view curveSegments

-- | The first point on the curve of an open curve.
outset :: (Chain t) => Lens' (OpenCurve_ t s) (Point2 s)
outset elt_fn (OpenCurve segments) =
    let (Bez v0 control v1) = firstLink segments
    in  (\v0' -> OpenCurve $ pure (Bez v0' control v1) <|> rest segments) <$> elt_fn v0

 -- | The last poinr on the curve of an open curve.
terminator :: (Chain t) => Lens' (OpenCurve_ t s) (Point2 s)
terminator elt_fn (OpenCurve segments) =
    let (Bez v0 control v1) = lastLink segments
    in  (\v1' -> OpenCurve $ notLast segments <|> pure (Bez v0 control v1')) <$> elt_fn v1

-- | Map over every point in an OpenCurve
overCurvePoints :: Functor t => (Point2 s -> Point2 s) -> OpenCurve_ t s -> OpenCurve_ t s
overCurvePoints f = over curveSegments (fmap (over bzPoints $ fmap f))

-- | Return the same curve in the opposite order.
reverseCurve :: (Chain t) => OpenCurve_ t s -> OpenCurve_ t s
reverseCurve = over curveSegments (reverseChain . fmap reverseBezier)

-- | Connect two curves end to end by translating c1 so that the starting point of 'c1' is equal to the terminator of 'c0'
(>*<) :: (Chain t, Space s) => OpenCurve_ t s -> OpenCurve_ t s -> OpenCurve_ t s
(>*<) c0 c1 = let delta = c0 ^. terminator ^-^ c1 ^. outset
                  transC1 = overCurvePoints (translateBy delta) c1
              in  over curveSegments (c0 ^. curveSegments <|>) transC1

instance (Hashable s, Hashable (t (Bezier s))) => Hashable (OpenCurve_ t s) where
    hashWithSalt s (OpenCurve segs) = s `hashWithSalt` segs

instance (NFData s, NFData (t (Bezier s))) => NFData (OpenCurve_ t s) where
    rnf (OpenCurve a ) = a `deepseq` ()

instance (s ~ (SpaceOf (f (Bezier s))), Space s, Monad f, Alternative f, Show (f (Bezier s)), Chain f) => CanProject (BezierSpace s) (OpenCurve_ f s) where
    projectionWithStepsAccuracy max_steps m_accuracy bSpace curve =
         OpenCurve . overChainNeighbors fixBezierNeighbor . projectionWithStepsAccuracy max_steps m_accuracy bSpace . view curveSegments $ curve

instance (Chain f, Space s, CanProject (BezierSpace s) t, Show (f (Bezier s)), Chain f) => CanProject (OpenCurve_ f s) t where
    projectionWithStepsAccuracy max_steps m_accuracy path t =
      let bSpace = makeBezierSpace arcLength (view curveSegments path)
      in  projectionWithStepsAccuracy max_steps m_accuracy bSpace t
