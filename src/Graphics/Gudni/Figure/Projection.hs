{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Projection
  ( CanProject(..)
  , CanFit(..)
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Lens
import Linear
import Linear.Affine
import Linear.V2
import Linear.V3
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad
import Data.Functor.Classes
import Data.Maybe (fromMaybe, fromJust)

-- | In most cases, it is sufficient to define
-- @projectionWithStepsAccuracy@, and use default implementations for the
-- remaining functions.  You may also want to define a default
-- accuracy by overriding @project@.
class (Space (SpaceOf t)) => CanProject u t where
    projectOnto :: Bool -> u -> t -> t
    default projectOnto :: Bool -> u -> t -> t
    projectOnto debug = projectionWithAccuracy debug 1e-3

    projectionWithAccuracy :: Bool -> SpaceOf t -> u -> t -> t
    default projectionWithAccuracy :: Bool -> SpaceOf t -> u -> t -> t
    projectionWithAccuracy debug accuracy =
        projectionWithStepsAccuracy debug (maxStepsFromAccuracy accuracy) (Just accuracy)

    projectionWithSteps :: Bool -> Int -> u -> t -> t
    projectionWithSteps debug max_steps = projectionWithStepsAccuracy debug max_steps Nothing

    projectionWithStepsAccuracy :: Bool -> Int -> Maybe (SpaceOf t) -> u -> t -> t

class Reversible t where
    reverseItem :: t -> t

class (Space (SpaceOf t)) => CanFit t where
    isForward :: t -> Bool
    projectTangent :: SpaceOf t -> Point2 (SpaceOf t) -> Diff Point2 (SpaceOf t) -> t -> t
    fillGap :: (Chain f) => f t -> f t -> f t
    projectOntoCurve :: Bool -> Int -> Maybe (SpaceOf t) -> SpaceOf t -> Bezier (SpaceOf t) -> t -> t
