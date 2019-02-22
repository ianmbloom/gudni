module Graphics.Gudni.Figure.Curve
  ( Outline(..)
  , mapOutline
  , outlineBox
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Outline
import Graphics.Gudni.Util.Util


data Segment s = Segment
  { onCurvePoint  :: Point2 s
  , offCurvePoint :: Point2 s
  , arcLength     :: s
  }

data Curve s = Curve
  { curveSegments    :: [Segment s]
  , curveTermination :: Maybe (Point2 s)
}

curveLength = length . curveSegments

getCurveSections :: Curve s -> [(Point2 s, Point2 s, Point2 s)]
getCurveSections (Curve (x:xs) (Just end)) = getCurveSections' end              (x:xs)
getCurveSections (Curve (x:xs) Nothing   ) = getCurveSections' (onCurvePoint x) (x:xs)

getCurveSections' :: Point2 s -> [Segment s] -> [(Point2 s, Point2 s, Point2 s)]
getCurveSections' end (x : xs) =
  case xs of
    (y : ys) -> (onCurvePoint x, offCurvePoint x, onCurvePoint y):getCurveSections' end xs
    []       -> [(onCurvePoint x, offCurvePoint x, end)]


segmentToOutline (Segment on off _) = CurvePair on off

curveToOutline (Curve segments Nothing) = Outline $ map segmentToOutline segments
