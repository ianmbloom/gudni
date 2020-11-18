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
{-# LANGUAGE DeriveGeneric         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Shape.Type
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A ShapeTree is the main input data structure for the Gudni RasterState. A client program
-- generates a Scene which contains a ShapeTree for each frame that they wish to render.

module Graphics.Gudni.Figure.Shape.Type
  ( Shape_(..)
  , shapeOutlines
  , Shape(..)
  , ShapeFunctor(..)
  )
where

import Graphics.Gudni.Figure.Principle.Space
import Graphics.Gudni.Figure.Principle.Point
import Graphics.Gudni.Figure.Principle.Box
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Figure.Shape.Outline
import Graphics.Gudni.Base.Chain
import Control.Lens
import Data.Vector as V

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

newtype Shape_ f s = Shape {_shapeOutlines :: [Outline_ f s]} deriving (Generic)
makeLenses ''Shape_

deriving instance (Show (Outline_ f s)) => Show (Shape_ f s)
deriving instance (Eq   (Outline_ f s)) => Eq (Shape_ f s)
deriving instance (Ord  (Outline_ f s)) => Ord (Shape_ f s)

instance (Chain f, Space s) => PointContainer (Shape_ f s) where
    mapOverPoints f = over shapeOutlines (fmap (mapOverPoints f))

instance ( Chain f
         , Space s
         )
         => BezierContainer (Shape_ f s)
    where
    type BezFunctor (Shape_ f s) = f
    joinOverBeziers f = Shape . fmap (joinOverBeziers f) . view shapeOutlines

instance (Chain f, Space s) => CanBox (Shape_ f s) where
    boxOf = minMaxBoxes . fmap boxOf . view shapeOutlines

instance Space s => HasSpace (Shape_ f s) where
    type SpaceOf (Shape_ f s) = s

type ShapeFunctor = V.Vector

instance HasSpace a => HasSpace (ShapeFunctor a) where
    type SpaceOf (ShapeFunctor a) = SpaceOf a

type Shape s = Shape_ ShapeFunctor s
