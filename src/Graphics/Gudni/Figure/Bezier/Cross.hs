{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}

module Graphics.Gudni.Figure.Bezier.Cross
  ( crossesAlong
  , crosses
  , interimPoint
  )
where

import Graphics.Gudni.Figure.Primitive.Space
import Graphics.Gudni.Figure.Primitive.Axis
import Graphics.Gudni.Figure.Primitive.Point
import Graphics.Gudni.Figure.Primitive.Box
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Util.Debug

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
  in  ((alo > 0) /= (ath > 0)) || ({-ath /= 0 && -} alo == 0)

trDP depth = trDepth (depth + 1)

crossesAlong :: (Axis axis, Space s) => axis -> s -> s -> s -> Bezier s -> Bool
crossesAlong axis baseline start end bez =
  --tc ("crossesAlong " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
  if start == end
  then False
  else
  if start > end
  then crossesAlong axis baseline end start bez
  else go {-1-} bez
  where
  go {-depth-} bez =
    --tcDepth depth ("go " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
    let minAthwart = {-trDP depth "minAthwart" $-} bezAthwart axis min bez
        maxAthwart = {-trDP depth "maxAthwart" $-} bezAthwart axis max bez
        minAlong   = {-trDP depth "minAlong  " $-} bezAlong   axis min bez
        maxAlong   = {-trDP depth "maxAlong  " $-} bezAlong   axis max bez
        (lessBez, moreBez) = splitBezier 0.5 bez
    in  if --trDP depth "totally outside" $
           ({-trDP depth "baseline >  maxAthwart" $-} baseline >  maxAthwart) ||
           ({-trDP depth "baseline <= minAthwart" $-} baseline <= minAthwart) ||
           ({-trDP depth "     start >  maxAlong" $-} start >  maxAlong     ) ||
           ({-trDP depth "     end   <= minAlong" $-} end   <= minAlong     )
        then -- segment is totally outside the range of curve
             False
        else let size = (maxAlong - minAlong) -- `max` (maxAthwart - minAthwart)
                 slopeLTEZero = --trDP depth "slopeLTEZero" $
                                bezierSlopeLTEZero axis bez
                 -- barSide = isHorizontal axis || bezierSlopeLTEZero axis bez
                 offBaseline = --trDP depth "offBaseLine" $
                               baseline /= maxAthwart
                 --oppose  = if isHorizontal axis || bezierSlopeLTEZero axis bez then maxAlong else minAlong
                 isK = --trDP depth "ISK" $
                       isKnobAbsolute axis bez
             in
             if  --trDP depth "mustSplit" $
                 (size > limit &&
                 -- curve size remains greater than the limit
                 offBaseline &&
                 {-trDP depth "||"-} ({-trDP depth "start > minAlong"-} (start > minAlong)|| {-trDP depth "end <= maxAlong"-} (end <= maxAlong)) -- and the start or end points are somewhere inside curve limits
                )
                || isK -- or the curve creates a knob, meaning there could be more than one cross point
             then -- must split
                  go {-(depth + 2)-} lessBez /= go {-(depth + 2)-} moreBez
             else let barrierMin = {-trDP depth "barrierMin" -} slopeLTEZero || (not slopeLTEZero && (offBaseline && not (isHorizontal axis)))
                      minAlongNoC = min (bez ^. bzStart . along axis) (bez ^. bzEnd . along axis)
                      maxAlongNoC = max (bez ^. bzStart . along axis) (bez ^. bzEnd . along axis)
                      barrier    = {-trDP depth "barrier"    $-} if barrierMin then minAlongNoC else maxAlongNoC
                      startLTE   = {-trDP depth "startLTE"   $-} slopeLTEZero || (not slopeLTEZero && (not (isHorizontal axis)))
                  in
                  if startLTE
                  then start <= barrier && end > barrier
                  else start <  barrier && end >= barrier

interimPoint :: Point2 s -> Point2 s -> Point2 s
interimPoint start end = Point2 (start ^. pX) (end ^. pY)

crosses :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crosses start end bez =
    let iP = interimPoint start end
    in
    crossesAlong Vertical   (start ^. pX) (start ^. pY) (iP  ^. pY) bez /=
    crossesAlong Horizontal (iP    ^. pY) (iP    ^. pX) (end ^. pX) bez
