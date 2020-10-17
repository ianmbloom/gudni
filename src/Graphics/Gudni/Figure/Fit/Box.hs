{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Fit.Box
  (
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Figure.Transform.Projection
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
import Text.PrettyPrint.GenericPretty


instance Reversible (Box s) where
  reverseItem = id

instance Reversible (V2 (Facet s)) where
  reverseItem = id

projectBoxWithStepsAccuracy :: (Space s, Chain f, CanCut (Box s)) => Bool -> Int -> Maybe s -> BezierSpace s -> Box s -> f (V2 (Facet s))
projectBoxWithStepsAccuracy debug max_steps m_accuracy bSpace box =
    join . fmap (traverseBezierSpace debug max_steps m_accuracy bSpace) $ pure box

instance GoesForward (Box s) where
    isForward _ = True

instance CanFillGap (V2 (Facet s)) where
    fillGap leftResult rightResult =
    {-
      let output = V4 (rightResult ^. facetOutput . _x . _x)
                      (rightResult ^. facetOutput . _x . _z)
                      (leftResult  ^. facetOutput . _y . _x)
                      (leftResult  ^. facetOutput . _y . _y)
      in
     -}
       leftResult <|> {-filler <|>-} rightResult


instance (Space s) => CanFit (Box s) (V2 (Facet s)) where
    projectTangent = projectTangentBox

    -- Given a target curve and a source curve, return a new target curve that approximates projecting every point in the target curve
    -- onto the source curve, such that the original x-axis corresponds to arclength along the source curve and the y-axis corresponds
    -- to distance from source curve along a normal.
    -- We can assume that the target curve has already been split and ordered so that the start x is less than or equal to the conrol x
    -- and the control x is less than or equal to the end x. In other words the curve is roughly horizontal.
    -- We can also assume that all the x coordinates will be within the range of 0 to the source curves arcLength after correction.
    projectDefaultCurve debugFlag max_steps m_accuracy start sourceCurve box =
       undefined $ projectCurveBox sourceCurve box

v4PointsToTris :: V4 (Point2 s) -> V2 (Tri s)
v4PointsToTris (V4 tl tr br bl) = V2 (V3 tl tr bl) (V3 tr br bl)

v4PointsToBezierTris :: Space s => V4 (Point2 s) -> V2 (BezTri s)
v4PointsToBezierTris = fmap triToBezTri . v4PointsToTris

projectTangentBox :: Space s => Ax Horizontal s -> Point2 s -> Diff Point2 s -> Box s -> V2 (Facet s)
projectTangentBox offset v0 normal box =
  let boxPoints = boxToV4Points box
      bezTris = v4PointsToBezierTris $ fmap (projectTangentPoint offset v0 normal) $ boxPoints
      tris = v4PointsToTris boxPoints
  in  liftA2 Facet bezTris tris

buildBezTris sourceCurve (V4 tl tr br bl) = undefined

projectCurveBox :: Space s => Bezier s -> Box s -> V2 (Facet s)
projectCurveBox sourceCurve box =
  let boxPoints = boxToV4Points box
      bezTris = buildBezTris sourceCurve boxPoints
      tris    = v4PointsToTris boxPoints
  in  liftA2 Facet bezTris tris
