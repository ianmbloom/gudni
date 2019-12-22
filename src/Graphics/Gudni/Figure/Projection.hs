{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Graphics.Gudni.Figure.Projection
  ( segmentLength
  , arcLength
  , CanProject(..)
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.ArcLength
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Segment
import Graphics.Gudni.Figure.OpenCurve

import Control.Lens
import Linear
import Linear.Affine
import Linear.V2
import Linear.V3

-- | Returns a Bezier for each Segment in the input
{-# INLINE segmentLength #-}
segmentLength :: (Space s) => (Segment s, Point2 s) -> s
segmentLength (seg, p2) = case seg ^. control of
    Nothing -> distance (seg ^. anchor) p2
    Just c -> arcLength (Bez (seg ^. anchor) c p2)

-- {-# SPECIALIZE arcLength :: OpenCurve Float  -> Float  #-}
-- {-# SPECIALIZE arcLength :: OpenCurve Double -> Double #-}
instance Space s => HasArcLength (OpenCurve s) where
    arcLength = sum . map segmentLength . terminated

-- | In most cases, it is sufficient to define
-- @projectWithStepsAccuracy@, and use default implementations for the
-- remaining functions.  You may also want to define a default
-- accuracy by overriding @project@.
class CanProject t where
  project :: OpenCurve (SpaceOf t) -> t -> t
  default project ::
      (SpaceOf t ~ s, Floating s, RealFrac s) => OpenCurve (SpaceOf t) -> t -> t
  project = projectWithAccuracy 1e-3

  projectWithAccuracy :: SpaceOf t -> OpenCurve (SpaceOf t) -> t -> t
  default projectWithAccuracy ::
      (SpaceOf t ~ s, Floating s, RealFrac s) => SpaceOf t -> OpenCurve (SpaceOf t) -> t -> t
  projectWithAccuracy accuracy =
      projectWithStepsAccuracy (maxStepsFromAccuracy accuracy) (Just accuracy)

  projectWithSteps :: Int -> OpenCurve (SpaceOf t) -> t -> t
  projectWithSteps max_steps = projectWithStepsAccuracy max_steps Nothing

  projectWithStepsAccuracy :: s ~ SpaceOf t => Int -> Maybe s -> OpenCurve s -> t -> t

instance (Epsilon s, RealFrac s, Space s) => CanProject (Point2 s) where
  projectWithStepsAccuracy max_steps m_accuracy curve (P (V2 overall_length offset)) =
      let -- find the segment containing the point we want
          segments = terminated curve
          pickSeg remaining [] = error "unreachable empty list in projectPoint"
          pickSeg remaining (s : ss) =
              let l = segmentLength s in
                  case (l < remaining, ss) of
                      (False, _) -> (remaining, s)
                      (True, []) -> let
                          (Seg p1 m_control, p2) = s
                          v = case m_control of
                              Just control -> p2 .-. control
                              Nothing -> p2 .-. p1
                        in (remaining - l, (Seg p2 Nothing, p2 .+^ negated v))
                      (True, _) -> pickSeg (remaining - l) ss
          (remaining_length, (Seg anchor m_control, endpoint)) = pickSeg overall_length segments
      in case m_control of
          -- if segment is a line, exact calculation
          Nothing -> (onCurve .+^ (offset *^ normal)) where
              t = remaining_length / distance anchor endpoint
              onCurve = lerp t endpoint anchor
              normal = normalize (perp (endpoint .-. anchor))
          -- if if segment is a curve, call inverseBezierArcLength
          Just c -> (onCurve .+^ (offset *^ normal)) where
              bz = Bez anchor c endpoint
              (_, Bez onCurve tangent _) = splitBezier bz
                  (inverseArcLength max_steps m_accuracy bz remaining_length)
              normal = normalize (perp (tangent .-. onCurve))

instance (SpaceOf (OpenCurve s) ~ s, Space s) => CanProject (OpenCurve s) where
    projectWithStepsAccuracy max_steps m_accuracy path (OpenCurve ss terminator)  =
        OpenCurve segments (proj terminator) where
      segments = ss <&> \(Seg p1 c) -> Seg (proj p1) (proj <$> c)
      proj = projectWithStepsAccuracy max_steps m_accuracy path
