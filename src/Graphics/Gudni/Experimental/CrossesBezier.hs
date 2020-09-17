{-# LANGUAGE TypeFamilies #-}
module Graphics.Gudni.Experimental.CrossesBezier
  ( crossesAlong
  , crosses
  , interimPoint
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Axis
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Figure.Split

import Control.Lens

limit :: (Space s) => s
limit = 1 / 32

foldBez :: (Point2 s -> a) -> (a -> a -> a) -> Bezier s -> a
foldBez f g = foldl1 g . fmap f . unBezier

bezAlong :: Axis axis => axis -> (s -> s -> s) -> Bezier s -> s
bezAlong axis = foldBez (view (along axis))

bezAthwart :: Axis axis => axis -> (s -> s -> s) -> Bezier s -> s
bezAthwart axis = foldBez (view (athwart axis))

bezierSlopeLTEZero :: (Axis axis, Space s) => axis -> Bezier s -> Bool
bezierSlopeLTEZero axis bez =
  let alo = bez ^. bzEnd . along   axis - bez ^. bzStart . along   axis
      ath = bez ^. bzEnd . athwart axis - bez ^. bzStart . athwart axis
  in  ((alo > 0) /= (ath > 0)) || (ath == 0 && alo /= 0)

crossesAlong :: (Axis axis, Space s) => axis -> s -> s -> s -> Bezier s -> Bool
crossesAlong axis baseline start end bez =
  if start == end
  then False
  else
  if start > end
  then crossesAlong axis baseline end start bez
  else go bez
  where
  go bez =
    let minAthwart = bezAthwart axis min bez
        maxAthwart = bezAthwart axis max bez
        minAlong   = bezAlong   axis min bez
        maxAlong   = bezAlong   axis max bez
        (lessBez, moreBez) = splitBezier 0.5 bez
    in  if baseline <  minAthwart ||
           baseline >= maxAthwart ||
           start >= maxAlong ||
           end   <  minAlong
        then -- segment is totally outside the range of curve
             False
        else let sizeAlong = maxAlong - minAlong
                 barrier = if isHorizontal axis || bezierSlopeLTEZero axis bez then minAlong else maxAlong
                 --oppose  = if isHorizontal axis || bezierSlopeLTEZero axis bez then maxAlong else minAlong
             in
             if {- tr "mustSplit" $ -}
                 (sizeAlong > limit && -- curve size remains greater than the limit
                 --(baseline /= oppose) &&
                 (start >= minAlong || end < maxAlong) -- and the start or end points are somewhere inside curve limits
                )
                || isKnob (along axis) bez -- or the curve creates a knob, meaning there could be more than one cross point
             then -- must split
                  go lessBez /= go moreBez
             else start < barrier && end >= barrier

interimPoint :: Point2 s -> Point2 s -> Point2 s
interimPoint start end = Point2 (start ^. pX) (end ^. pY)

crosses :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crosses start end bez =
    let iP = interimPoint start end
    in
    crossesAlong Vertical   (start ^. pX) (start ^. pY) (iP  ^. pY) bez /=
    crossesAlong Horizontal (iP    ^. pY) (iP    ^. pX) (end ^. pX) bez
