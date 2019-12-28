{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Outline
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
  ( Outline(..)
  , Outline_(..)
  , outlineSegments
  , mapOutlinePoints
  , closeOpenCurve
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Util.Util
import Control.Lens
import Linear.V2
import qualified Data.Vector as V

import Control.Applicative
import Data.Hashable
import Control.DeepSeq
import Control.Monad

-- | An outline is just a wrapper for a list of CurvePairs. It represents one curve loop∘
-- A shape is defined by a list of outlines.
newtype Outline_ t s = Outline
  { _outlineSegments :: t (Bezier s)
  }
makeLenses ''Outline_

type Outline s = Outline_ V.Vector s

instance (Show (t (Bezier s))) => Show (Outline_ t s) where
  show (Outline vs) = "Outline" ++ show vs

-- | Map over every point in an outline.
mapOutlinePoints :: Functor t => (Point2 s -> Point2 s) -> Outline_ t s -> Outline_ t s
mapOutlinePoints f ps = over outlineSegments (fmap (over bzPoints (fmap f))) ps

-- | Close an open curve and convert it to an outline. An additional straight segment is added if the outset and the terminator of
-- the curve are not the same.
closeOpenCurve :: forall t s . (UnLoop t, Alternative t, Space s) => OpenCurve_ t s -> Outline_ t s
closeOpenCurve curve =
  let connect :: t (Bezier s) -> t (Bezier s)
      connect = if curve ^. terminator == curve ^. outset
                 then id  -- if the beggining of the curve is the same as the end, ignore the end
                 else (pure (straight (curve ^. terminator) (curve ^. outset)) <|>)
                     -- else insert a straight segment from the end to the beggining.
  in  Outline . connect . view curveSegments $ curve

instance (Monad f, Alternative f, Space s) => CanProject (BezierSpace s) f (Outline_ f s) where
    projectionWithStepsAccuracy max_steps m_accuracy bSpace curve =
         pure . Outline . join . fmap (projectionWithStepsAccuracy max_steps m_accuracy bSpace) . view outlineSegments $ curve


-- * Instances


instance (SimpleSpace s) => HasSpace (Outline_ t s) where
  type SpaceOf (Outline_ t s) = s

--instance (Bounded s, Ord s, Num s) => HasSpace (V.Vector (CurvePair s)) where
--  type SpaceOf (V.Vector (CurvePair s)) = s

instance (Foldable t, Functor t, Space s) => HasBox (Outline_ t s) where
  boxOf (Outline vs) = minMaxBoxes . fmap boxOf $ vs

instance (Functor t, Space s) => SimpleTransformable (Outline_ t s) where
  translateBy p = mapOutlinePoints (translateBy p)
  scaleBy     s = mapOutlinePoints (scaleBy s)
  stretchBy   p = mapOutlinePoints (stretchBy p)
instance (Functor t, Space s) => Transformable (Outline_ t s) where
  rotateBy    a = mapOutlinePoints (rotateBy a)

instance (NFData s, NFData (t (Bezier s))) => NFData (Outline_ t s) where
  rnf (Outline ps) = ps `deepseq` ()

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt s vector = V.foldl hashWithSalt s vector

instance (Hashable (t (Bezier s))) => Hashable (Outline_ t s) where
  hashWithSalt s (Outline ps) = s `hashWithSalt` ps
