{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Transform.Apply
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Directly apply simple tranformations to point containers.

module Graphics.Gudni.Figure.Transform.Apply
 ( applyTranslation
 , applyStretch
 , applyScale
 , applyRotation
 , translateBox
 , stretchBox
 )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Shape
import Graphics.Gudni.Figure.Fit
import Graphics.Gudni.Figure.Transform.Projection

import Graphics.Gudni.Util.Debug

import Control.Monad.Random
import Control.Applicative
import Control.Lens

translate_ :: Space s => Point2 s -> Point2 s -> Point2 s
translate_ = (^+^)

stretch_ :: Space s => Point2 s -> Point2 s -> Point2 s
stretch_ = liftA2 (*)

rotate_ :: Space s => Angle s -> Point2 s -> Point2 s
rotate_ = rotate

translateBox p = mapBox (translate_ p)
stretchBox s = mapBox (stretch_ s)

applyTranslation :: PointContainer t => Point2 (SpaceOf t) -> t -> t
applyTranslation p = mapOverPoints (translate_ p)

applyStretch :: PointContainer t => Point2 (SpaceOf t) -> t -> t
applyStretch p = mapOverPoints (stretch_ p)

applyScale :: PointContainer t => SpaceOf t -> t -> t
applyScale s = mapOverPoints (stretch_ . pure $ s)

applyRotation :: PointContainer t => Angle (SpaceOf t) -> t -> t
applyRotation angle = mapOverPoints (rotate_ angle)

instance (Chain f, Space s)
         => CanApplyProjection (Shape_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path shape =
        joinOverBeziers (projectBezierWithStepsAccuracy debug max_steps m_accuracy path) shape

instance ( Space s
         , Chain f
         ) => CanApplyProjection (OpenCurve_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path =
        joinOverBeziers (projectBezierWithStepsAccuracy debug max_steps m_accuracy path)

instance ( Space s
         , Chain f
         )
         => CanApplyProjection (Outline_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path =
         joinOverBeziers (projectBezierWithStepsAccuracy debug max_steps m_accuracy path)
