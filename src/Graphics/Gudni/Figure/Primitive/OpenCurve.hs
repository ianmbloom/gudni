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
-- Module      :  Graphics.Gudni.Figure.Primitive.OpenCurve
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for defining and combining open curves.

module Graphics.Gudni.Figure.Primitive.OpenCurve
  ( OpenCurve_(..)
  , curveSegments
  , terminator
  , outset
  , OpenCurve(..)
  , makeOpenCurve
  )
where

import Prelude hiding (reverse)

import Graphics.Gudni.Base.Chain
import Graphics.Gudni.Base.Reversible

import Graphics.Gudni.Figure.Primitive.Space
import Graphics.Gudni.Figure.Primitive.Point
import Graphics.Gudni.Figure.Primitive.Bezier
import Graphics.Gudni.Figure.Primitive.ArcLength

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

deriving instance (Show (t (Bezier s))) => Show (OpenCurve_ t s)
deriving instance (Eq   (t (Bezier s))) => Eq (OpenCurve_ t s)
deriving instance (Ord  (t (Bezier s))) => Ord (OpenCurve_ t s)

instance Space s => HasSpace (OpenCurve_ t s) where
    type SpaceOf (OpenCurve_ t s) = s

instance ( Chain f
         , Space s) => PointContainer (OpenCurve_ f s) where
   mapOverPoints f = over curveSegments (fmap (over bzPoints (fmap f)))

instance ( Chain f
         , Space s
         )
         => BezierContainer (OpenCurve_ f s)
  where
  type BezFunctor (OpenCurve_ f s) = f
  joinOverBeziers f = OpenCurve .  join . fmap f . view curveSegments


type OpenCurve s = OpenCurve_ V.Vector s

makeOpenCurve :: f (Bezier s) -> OpenCurve_ f s
makeOpenCurve = OpenCurve

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

-- | Return the same curve in the opposite order.
instance (Chain t) => Reversible (OpenCurve_ t s) where
  reverseItem = over curveSegments (reverseChain . fmap reverseBezier)

instance (Hashable s, Hashable (t (Bezier s))) => Hashable (OpenCurve_ t s) where
    hashWithSalt s (OpenCurve segs) = s `hashWithSalt` segs

instance (NFData s, NFData (t (Bezier s))) => NFData (OpenCurve_ t s) where
    rnf (OpenCurve a ) = a `deepseq` ()
