{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Graphics.Gudni.Layout.ArrowHead
  ( ArrowDirection(..)
  , CanArrow(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Util.Segment
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Subdividable
import qualified Data.Vector as V

import Control.Lens
import Control.Applicative
import Control.Monad

data ArrowDirection = PointingForward | PointingBackward deriving (Eq, Show)

defaultArrowHead :: Space s => Point2 s -> ArrowDirection -> Outline s
defaultArrowHead size direction = stretchBy size $ fromSegments [straightXY (-1) (-0.5), straightXY 0 0, straightXY (-1) 0.5]

class (Space (SpaceOf t), HasSpace t, HasArcLength t, Reversible t) => CanArrow t where
  withArrowHeadOffsetShape :: Outline (SpaceOf t) -> Point2 (SpaceOf t) -> Point2 (SpaceOf t) -> ArrowDirection -> t -> Outline (SpaceOf t)

  withArrowHeadOffset :: Point2 (SpaceOf t) -> Point2 (SpaceOf t) -> ArrowDirection -> t -> Outline (SpaceOf t)
  withArrowHeadOffset offset size direction = withArrowHeadOffsetShape (defaultArrowHead size direction) offset size direction

  withArrowHead :: Point2 (SpaceOf t) -> ArrowDirection -> t -> Outline (SpaceOf t)
  withArrowHead size direction t =
    let len = arcLength t
        t' = if direction == PointingForward then t else reverseItem t
        offset = Point2 len 0
    in  withArrowHeadOffset offset size direction t'

instance (Space s) => CanArrow (OpenCurve s) where
  withArrowHeadOffsetShape shape offset size direction path =
    projectOnto False path . translateBy offset $ shape

instance (Space s) => CanArrow (Bezier s) where
  withArrowHeadOffsetShape shape offset size direction bz =
      withArrowHeadOffsetShape shape offset size direction (makeOpenCurve [bz])
