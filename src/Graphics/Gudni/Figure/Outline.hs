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
{-# LANGUAGE StandaloneDeriving         #-}

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
-- Functions for defining Outlines which are closed bezier curves.

module Graphics.Gudni.Figure.Outline
  ( Outline(..)
  , Outline_(..)
  , outlineSegments
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
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Util
import Control.Lens
import Linear.V2
import Linear.V3
import qualified Data.Vector as V

import Control.Applicative
import Data.Hashable
import Control.DeepSeq
import Control.Monad

-- | An shape is just a wrapper for a list of beziers. It represents one curve loop∘
newtype Outline_ f s = Outline
  { _outlineSegments :: f (Bezier s)
  }
makeLenses ''Outline_

deriving instance (Eq   (f (Bezier s))) => Eq (Outline_ f s)
deriving instance (Ord  (f (Bezier s))) => Ord (Outline_ f s)
instance (Show (f (Bezier s))) => Show (Outline_ f s) where
  show (Outline vs) = "Outline" ++ show vs

type Outline s = Outline_ V.Vector s

unfoldV3 :: Alternative f => V3 a -> f a
unfoldV3 (V3 a b c) = pure a <|> pure b <|> pure c

instance ( Chain f
         , Space s) => PointContainer (Outline_ f s) where
   type ContainerFunctor (Outline_ f s) = f
   containedPoints = join . fmap (unfoldBezier) . view outlineSegments
   mapOverPoints f = over outlineSegments (fmap (over bzPoints (fmap f)))

-- | Close an open curve and convert it to an shape. An additional line segment is added if the outset and the terminator of
-- the curve are not the same.
closeOpenCurve :: forall f s . (Chain f, Space s) => OpenCurve_ f s -> Outline_ f s
closeOpenCurve curve =
  let connect :: f (Bezier s) -> f (Bezier s)
      connect = if curve ^. terminator == curve ^. outset
                 then id  -- if the beggining of the curve is the same as the end, ignore the end
                 else (pure (line (curve ^. terminator) (curve ^. outset)) <|>)
                     -- else insert a line segment from the end to the beggining.
  in  Outline . connect . view curveSegments $ curve

instance (s ~ SpaceOf (f (Bezier s)), Monad f, Alternative f, Space s, Show (f (Bezier s)), Loop f) => CanProject (BezierSpace s) (Outline_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy bSpace curve =
         Outline . {-overLoopNeighbors fixBezierNeighbor . -} projectionWithStepsAccuracy debug max_steps m_accuracy bSpace . view outlineSegments $ curve

-- * Instances
instance (Space s) => HasSpace (Outline_ t s) where
  type SpaceOf (Outline_ t s) = s

--instance (Bounded s, Ord s, Num s) => HasSpace (V.Vector (CurvePair s)) where
--  type SpaceOf (V.Vector (CurvePair s)) = s

instance (Foldable t, Functor t, Space s) => HasBox (Outline_ t s) where
  boxOf (Outline vs) = minMaxBoxes . fmap boxOf $ vs

instance (NFData s, NFData (t (Bezier s))) => NFData (Outline_ t s) where
  rnf (Outline ps) = ps `deepseq` ()

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt s vector = V.foldl hashWithSalt s vector

instance (Hashable (t (Bezier s))) => Hashable (Outline_ t s) where
  hashWithSalt s (Outline ps) = s `hashWithSalt` ps
