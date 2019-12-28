{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Projection
  ( CanProject(..)
  , BezierSpace(..)
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Deknob

import Control.Lens
import Linear
import Linear.Affine
import Linear.V2
import Linear.V3
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad
import Data.Functor.Classes

projectPoint :: Space s => Int -> Maybe s -> Bezier s -> Point2 s -> Point2 s
projectPoint max_steps m_accuracy bz@(Bez anchor control endpoint) (Point2 x y) =
  (onCurve .+^ (y *^ normal)) where
      len = (inverseArcLength max_steps m_accuracy bz x)
      (_, Bez onCurve tangent _) = splitBezier x bz
      normal = normalize (perp (tangent .-. onCurve))

-- | In most cases, it is sufficient to define
-- @projectionWithStepsAccuracy@, and use default implementations for the
-- remaining functions.  You may also want to define a default
-- accuracy by overriding @project@.
class (SpaceOf u ~ SpaceOf t, Space (SpaceOf t), Monad f, Alternative f) => CanProject u f t where
  projection :: u -> t -> f t
  default projection :: u -> t -> f t
  projection = projectionWithAccuracy 1e-3

  projectionWithAccuracy :: SpaceOf t -> u -> t -> f t
  default projectionWithAccuracy :: SpaceOf t -> u -> t -> f t
  projectionWithAccuracy accuracy =
      projectionWithStepsAccuracy (maxStepsFromAccuracy accuracy) (Just accuracy)

  projectionWithSteps :: Int -> u -> t -> f t
  projectionWithSteps max_steps = projectionWithStepsAccuracy max_steps Nothing

  projectionWithStepsAccuracy :: Int -> Maybe (SpaceOf t) -> u -> t -> f t

instance (Space s, Monad f, Alternative f) => CanProject (Bezier s) f (Bezier s) where
    projectionWithStepsAccuracy max_steps m_accuracy path =
       pure . over bzPoints (fmap (projectPoint max_steps m_accuracy path))


instance (Space s, Monad f, Alternative f) => CanProject (BezierSpace s) f (Bezier s) where
    projectionWithStepsAccuracy max_steps m_accuracy bSpace =
      let stretchProject :: (Monad f, Alternative f) => (s -> Bezier s -> Bezier s -> f (Bezier s))
          stretchProject len path = projectionWithStepsAccuracy max_steps m_accuracy path . stretchBy (Point2 (1/len) 1)
      in  join . (fmap (traverseBezierSpace stretchProject bSpace)) . fixKnob

instance (Space s, Monad f, Alternative f) => CanProject (BezierSpace s) f (OpenCurve_ f s) where
    projectionWithStepsAccuracy max_steps m_accuracy bSpace curve =
         pure . OpenCurve . join . fmap (projectionWithStepsAccuracy max_steps m_accuracy bSpace) . view curveSegments $ curve

instance (Eq1 f, Alternative f, UnLoop f, Space s, CanProject (BezierSpace s) f t) => CanProject (OpenCurve_ f s) f t where
    projectionWithStepsAccuracy max_steps m_accuracy path t =
      let bSpace = makeBezierSpace arcLength (view curveSegments $ path)
      in  projectionWithStepsAccuracy max_steps m_accuracy bSpace t

data BezierSpace s = BezierSpace
  { bsTree   :: RangeTree (Bezier s)
  , bsLength :: s
  }

instance Space s => HasSpace (BezierSpace s) where
  type SpaceOf (BezierSpace s) = s

data HasSpace t => RangeTree t
  = RangeSplit (SpaceOf t) (RangeTree t) (RangeTree t)
  | RangeLeaf { rangeLength :: (SpaceOf t)
              , rangeItem :: t
              }
  | RangeEmpty

makeBezierSpace :: forall f s . (Eq1 f, Alternative f, UnLoop f, Space s) => (Bezier s -> s) -> f (Bezier s) -> BezierSpace s
makeBezierSpace lengthFun = uncurry BezierSpace . go 0
  where
  go :: s -> f (Bezier s) -> (RangeTree (Bezier s), s)
  go start vector =
    let (left, right) = halfSplit vector
    in if right `eq1` empty
       then if left `eq1` empty
            then (RangeEmpty, start)
            else let curve = first left
                     curveLength = lengthFun curve
                 in  (RangeLeaf curveLength curve, start + curveLength)
        else
           let (leftBranch,  leftSplit ) = go start     left
               (rightBranch, rightSplit) = go leftSplit right
           in  (RangeSplit leftSplit leftBranch rightBranch, rightSplit)

traverseBezierSpace :: (Space s, Alternative f) => (s -> Bezier s -> Bezier s -> f a) -> BezierSpace s -> Bezier s -> f a
traverseBezierSpace f (BezierSpace tree totalLen) bez =
  go bez tree
  where
  go bez tree = case tree of
    RangeSplit splitPoint left right ->
      if (unOrtho (bez ^. bzStart . pX) < splitPoint)
      then if (unOrtho (bez ^. bzEnd . pX) > splitPoint)
           then let [leftBez, rightBez] = splitBezierX splitPoint bez
                in  go leftBez left <|> go rightBez right
           else go bez left
      else go bez right
    RangeEmpty -> empty
    RangeLeaf len controlCurve -> f len controlCurve bez
