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
  ( OpenCurve_(..)
  , OpenCurve(..)
  , UnLoop(..)
  , curveSegments
  , terminator
  , outset
  , (<^>)
  , reverseCurve
  , overCurvePoints
  )
where
import Prelude hiding (last, reverse)

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Transformable
import qualified Graphics.Gudni.Figure.Bezier as BZ
--import Graphics.Gudni.Util.Util
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

-- | An (almost) typesafe representation of an open bezier curve.
data OpenCurve_ t s = OpenCurve
    { _curveSegments :: t (Bezier s)
    }
makeLenses ''OpenCurve_

deriving instance (Show (t (Bezier s))) => Show (OpenCurve_ t s)
deriving instance (Eq   (t (Bezier s))) => Eq (OpenCurve_ t s)
deriving instance (Ord  (t (Bezier s))) => Ord (OpenCurve_ t s)

class UnLoop t where
  first   :: t a -> a
  rest    :: t a -> t a
  last    :: t a -> a
  notLast :: t a -> t a
  reverse :: t a -> t a
  halfSplit :: t a -> (t a, t a)

instance UnLoop V.Vector where
  first   = V.head
  rest    = V.tail
  last    = V.last
  notLast vector = V.take (V.length vector - 1) vector
  reverse = V.reverse
  halfSplit vector = if (V.null vector)
                     then (V.empty, V.empty)
                     else let half = V.length vector `div` 2
                          in (V.take half vector, V.drop half vector)

type OpenCurve s = OpenCurve_ V.Vector s

instance (SimpleSpace s) => HasSpace (OpenCurve_ t s) where
    type SpaceOf (OpenCurve_ t s) = s

-- {-# SPECIALIZE arcLength :: OpenCurve Float  -> Float  #-}
-- {-# SPECIALIZE arcLength :: OpenCurve Double -> Double #-}
instance (Functor t, Foldable t, Space s) => HasArcLength (OpenCurve_ t s) where
    arcLength = sum . fmap arcLength . view curveSegments

-- | The first point on the curve of an open curve.
outset :: (Alternative t, UnLoop t) => Lens' (OpenCurve_ t s) (Point2 s)
outset elt_fn (OpenCurve segments) =
    let (Bez v0 control v1) = first segments
    in  (\v0' -> OpenCurve $ pure (Bez v0' control v1) <|> rest segments) <$> elt_fn v0

 -- | The last poinr on the curve of an open curve.
terminator :: (Alternative t, UnLoop t) => Lens' (OpenCurve_ t s) (Point2 s)
terminator elt_fn (OpenCurve segments) =
    let (Bez v0 control v1) = last segments
    in  (\v1' -> OpenCurve $ notLast segments <|> pure (Bez v0 control v1')) <$> elt_fn v1

-- | Map over every point in an OpenCurve
overCurvePoints :: Functor t => (Point2 s -> Point2 s) -> OpenCurve_ t s -> OpenCurve_ t s
overCurvePoints f = over curveSegments (fmap (over bzPoints $ fmap f))

-- | Return the same curve in the opposite order.
reverseCurve :: (Functor t, UnLoop t) => OpenCurve_ t s -> OpenCurve_ t s
reverseCurve = over curveSegments (reverse . fmap reverseBezier)

-- | Connect two curves end to end by translating c1 so that the starting point of 'c1' is equal to the terminator of 'c0'
(<^>) :: (UnLoop t, Alternative t, Space s) => OpenCurve_ t s -> OpenCurve_ t s -> OpenCurve_ t s
(<^>) c0 c1 = let delta = c0 ^. terminator ^-^ c1 ^. outset
                  transC1 = overCurvePoints (translateBy delta) c1
              in  over curveSegments (c0 ^. curveSegments <|>) transC1

instance (Hashable s, Hashable (t (Bezier s))) => Hashable (OpenCurve_ t s) where
    hashWithSalt s (OpenCurve segs) = s `hashWithSalt` segs

instance (NFData s, NFData (t (Bezier s))) => NFData (OpenCurve_ t s) where
    rnf (OpenCurve a ) = a `deepseq` ()
