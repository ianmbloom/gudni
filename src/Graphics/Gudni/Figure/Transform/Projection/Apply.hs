{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Transform.Projection.Apply
  ( CanApplyProjection(..)
  )
where

import Graphics.Gudni.Base.Chain
import Graphics.Gudni.Base.Loop

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Transform.Projection.BezierSpace
import Graphics.Gudni.Figure.Bezier.Split
import Graphics.Gudni.Figure.Bezier.Cut
import Graphics.Gudni.Figure.Deknob

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
class (HasSpace t) => CanApplyProjection t where
    projectDefault :: Bool -> BezierSpace (SpaceOf t) -> t -> t
    default projectDefault :: Bool -> BezierSpace (SpaceOf t) -> t -> t
    projectDefault debug = projectionWithAccuracy debug 1e-3

    projectionWithAccuracy :: Bool -> SpaceOf t -> BezierSpace (SpaceOf t) -> t -> t
    default projectionWithAccuracy :: Bool -> SpaceOf t -> BezierSpace (SpaceOf t) -> t -> t
    projectionWithAccuracy debug accuracy =
        projectionWithStepsAccuracy debug (maxStepsFromAccuracy accuracy) (Just accuracy)

    projectionWithSteps :: Bool -> Int -> BezierSpace (SpaceOf t) -> t -> t
    projectionWithSteps debug max_steps = projectionWithStepsAccuracy debug max_steps Nothing

    projectionWithStepsAccuracy :: Bool -> Int -> Maybe (SpaceOf t) -> BezierSpace (SpaceOf t) -> t -> t
