{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Graphics.Gudni.Draw.ArrowHead
  ( ArrowDirection(..)
  , CanArrow(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Layout

import Graphics.Gudni.Util.Segment
import Graphics.Gudni.Util.Debug
import qualified Data.Vector as V

import Control.Lens
import Control.Applicative
import Control.Monad

data ArrowDirection = PointingForward | PointingBackward deriving (Eq, Show)

defaultArrowHead :: (Loop f, Space s) => Point2 s -> ArrowDirection -> Shape_ f s
defaultArrowHead size direction = Shape . pure . applyStretch size $ fromSegments [straightXY (-1) (-0.5), straightXY 0 0, straightXY (-1) 0.5]

class (Space (SpaceOf t), HasSpace t, HasArcLength t, Reversible t) => CanArrow t where
  withArrowHeadOffsetShape :: Shape (SpaceOf t) -> Point2 (SpaceOf t) -> Point2 (SpaceOf t) -> ArrowDirection -> t -> CompoundTree (SpaceOf t)

  withArrowHeadOffset :: Point2 (SpaceOf t) -> Point2 (SpaceOf t) -> ArrowDirection -> t -> CompoundTree (SpaceOf t)
  withArrowHeadOffset offset size direction = withArrowHeadOffsetShape (defaultArrowHead size direction) offset size direction

  withArrowHead :: Point2 (SpaceOf t) -> ArrowDirection -> t -> CompoundTree (SpaceOf t)
  withArrowHead size direction t =
    let len = arcLength t
        t' = if direction == PointingForward then t else reverseItem t
        offset = Point2 len 0
    in  withArrowHeadOffset offset size direction t'

instance (Space s) => CanArrow (OpenCurve s) where
  withArrowHeadOffsetShape shape offset size direction path =
    projectOnto path . translateBy offset $ mask shape

instance (Space s) => CanArrow (Bezier s) where
  withArrowHeadOffsetShape shape offset size direction bz =
      withArrowHeadOffsetShape shape offset size direction (makeOpenCurve (pure bz :: ShapeFunctor (Bezier s)))
