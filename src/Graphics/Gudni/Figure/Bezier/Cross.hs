{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Bezier.Cross
  ( crossesAlong
  , crosses
  , crossesHorizontal
  , crossesVertical
  , interimPoint
  , bezierSlopeLTEZero
  , crossSplitLimit
  , bezAlong
  , bezAthwart
  )
where

import Graphics.Gudni.Figure.Principle.Space
import Graphics.Gudni.Figure.Principle.Axis
import Graphics.Gudni.Figure.Principle.Point
import Graphics.Gudni.Figure.Principle.Box
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Util.Debug

import Control.Lens

crossSplitLimit :: (Space s) => s
crossSplitLimit = 1 / 32

foldBez :: (Point2 s -> a) -> (a -> a -> a) -> Bezier s -> a
foldBez f g = foldl1 g . fmap f . unBezier

bezAlong :: Axis axis => axis -> (Along axis s -> Along axis s -> Along axis s) -> Bezier s -> Along axis s
bezAlong axis = foldBez (view (along axis))

bezAthwart :: Axis axis => axis -> (Athwart axis s -> Athwart axis s -> Athwart axis s) -> Bezier s -> Athwart axis s
bezAthwart axis = foldBez (view (athwart axis))

bezierSlopeLTEZero :: (Axis axis, Space s) => axis -> Bezier s -> Bool
bezierSlopeLTEZero axis bez =
  let alo = bez ^. bzEnd . along   axis - bez ^. bzStart . along   axis
      ath = bez ^. bzEnd . athwart axis - bez ^. bzStart . athwart axis
  in  ((alo > 0) /= (ath > 0)) || ({-ath /= 0 &&-} alo == 0)

trDP depth message = id -- trDepth (depth + 1) message
tcDP depth message = id -- tcDepth (depth + 1) message

crossesAlong :: forall axis s
             . (Axis axis, Space s)
             => axis
             -> Along axis s
             -> Athwart axis s
             -> Along axis s
             -> Bezier s
             -> Bool
crossesAlong axis start baseline end bez =
  --tc ("crossesAlong " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
  if start == end
  then False
  else
  if start > end
  then crossesAlong axis end baseline start bez
  else go bez
  where
  go :: Bezier s -> Bool
  go bez =
    --tcDepth depth ("go " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
    let minAthwart = bezAthwart axis min bez
        maxAthwart = bezAthwart axis max bez
        minAlong   = bezAlong   axis min bez
        maxAlong   = bezAlong   axis max bez
        (lessBez, moreBez) = splitBezier 0.5 bez
    in  if baseline >  maxAthwart ||
           baseline <= minAthwart ||
           start >  maxAlong      ||
           end   <= minAlong
        then -- segment is totally outside the range of curve
             False
        else let size = fromAlong axis (maxAlong - minAlong) `max` fromAthwart axis (maxAthwart - minAthwart)
                 slopeLTEZero = bezierSlopeLTEZero axis bez
                 offBaseline = baseline /= maxAthwart
                 isK = isKnobAbsolute axis bez || isKnobAbsolute (perpendicularTo axis) bez
             in
             if  size >= crossSplitLimit &&
                (
                 -- curve size remains greater than the limit
                 offBaseline &&
                 (start > minAlong || end <= maxAlong) -- and the start or end points are somewhere inside curve limits
                )
                || isK -- or the curve creates a knob, meaning there could be more than one cross point
             then -- must split
                  go lessBez /= go moreBez
             else
                  let barrierMin = slopeLTEZero || (not slopeLTEZero && (offBaseline && isVertical axis))
                      barrier    = if barrierMin then minAlong else maxAlong
                      startLTE   = slopeLTEZero || (not slopeLTEZero && (offBaseline || isHorizontal axis))
                  in  if startLTE
                      then start <= barrier && end > barrier
                      else start < barrier  && end >= barrier

-- This is an implementation of crosses along that doesn't short circuit if the maximum side of the curve is on the baseline.
crossesAlongNoShort :: forall axis s
                    . (Axis axis, Space s)
                    => axis
                    -> Along axis s
                    -> Athwart axis s
                    -> Along axis s
                    -> Bezier s
                    -> Bool
crossesAlongNoShort axis start baseline end bez =
  --tc ("crossesAlong " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
  if start == end
  then False
  else
  if start > end
  then crossesAlong axis end baseline start bez
  else go bez
  where
  go :: Bezier s -> Bool
  go bez =
    --tcDepth depth ("go " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
    let minAthwart = bezAthwart axis min bez
        maxAthwart = bezAthwart axis max bez
        minAlong   = bezAlong   axis min bez
        maxAlong   = bezAlong   axis max bez
        (lessBez, moreBez) = splitBezier 0.5 bez
    in  if baseline >  maxAthwart ||
           baseline <= minAthwart ||
           start >  maxAlong      ||
           end   <= minAlong
        then -- segment is totally outside the range of curve
             False
        else let size = fromAlong axis (maxAlong - minAlong) `max` fromAthwart axis (maxAthwart - minAthwart)
                 slopeLTEZero = bezierSlopeLTEZero axis bez
                 isK = isKnobAbsolute axis bez || isKnobAbsolute (perpendicularTo axis) bez
             in
             if  size >= crossSplitLimit &&
                (
                 -- curve size remains greater than the limit
                 (start > minAlong || end <= maxAlong) -- and the start or end points are somewhere inside curve limits
                )
                || isK -- or the curve creates a knob, meaning there could be more than one cross point
             then -- must split
                  go lessBez /= go moreBez
             else
                  let barrierMin = slopeLTEZero || (not slopeLTEZero && isVertical axis)
                      barrier    = if barrierMin then minAlong else maxAlong
                  in  start <= barrier && end > barrier


interimPoint :: Point2 s -> Point2 s -> Point2 s
interimPoint start end = makePoint (start ^. pX) (end ^. pY)

crosses :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crosses start end bez =
    let iP = interimPoint start end
    in
    crossesAlong Vertical   (start ^. pY) (start ^. pX) (iP  ^. pY) bez /=
    crossesAlong Horizontal (iP    ^. pX) (iP    ^. pY) (end ^. pX) bez

crossesHorizontal :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crossesHorizontal start end bez =
    crossesAlong Horizontal (start ^. pX) (end ^. pY) (end ^. pX) bez

crossesVertical :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crossesVertical start end bez =
    crossesAlong Vertical   (start ^. pY) (start ^. pX) (end  ^. pY) bez
