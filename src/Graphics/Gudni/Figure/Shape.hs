{-# LANGUAGE UndecidableInstances #-} -- Show (SpaceOf leaf)
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Shape
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A ShapeTree is the main input data structure for the Gudni Rasterizer. A client program
-- generates a Scene which contains a ShapeTree for each frame that they wish to render.

module Graphics.Gudni.Figure.Shape
  ( Shape_(..)
  , shapeOutlines
  , Shape(..)
  , ShapeFunctor(..)
  )
where

import Graphics.Gudni.Figure.HasDefault
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Figure.Outline
import Graphics.Gudni.Util.Chain
import Control.Lens
import Data.Vector as V

newtype Shape_ f s = Shape {_shapeOutlines :: [Outline_ f s]}
makeLenses ''Shape_

deriving instance (Show (Outline_ f s)) => Show (Shape_ f s)
deriving instance (Eq   (Outline_ f s)) => Eq (Shape_ f s)
deriving instance (Ord  (Outline_ f s)) => Ord (Shape_ f s)

instance (Chain f, Space s) => PointContainer (Shape_ f s) where
    type ContainerFunctor (Shape_ f s) = f
    containedPoints = concatChains . fmap containedPoints . view shapeOutlines
    mapOverPoints f = over shapeOutlines (fmap (mapOverPoints f))

instance (Chain f, Space s) => HasBox (Shape_ f s) where
    boxOf = minMaxBoxes . fmap boxOf . view shapeOutlines

instance Space s => HasSpace (Shape_ f s) where
    type SpaceOf (Shape_ f s) = s

instance (Chain f, Space s) => SimpleTransformable (Shape_ f s) where
    translateBy p = over shapeOutlines $ fmap $ mapOverPoints (translateBy p)
    scaleBy     s = over shapeOutlines $ fmap $ mapOverPoints (scaleBy s)
    stretchBy   p = over shapeOutlines $ fmap $ mapOverPoints (stretchBy p)
instance (Chain f, Space s) => Transformable (Shape_ f s) where
    rotateBy    a = over shapeOutlines $ fmap $ mapOverPoints (rotateBy a)
instance (Chain f, Space s)
         => CanProject (BezierSpace s) (Shape_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path shape =
        over shapeOutlines (fmap (projectionWithStepsAccuracy debug max_steps m_accuracy path)) shape

type ShapeFunctor = V.Vector
type Shape s = Shape_ ShapeFunctor s
