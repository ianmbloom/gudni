{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Bezier.Cross
  ( crossesBezierAlong
  , crossesBezier
  , crossesBezierHorizontal
  , crossesBezierVertical
  , interimPoint
  , bezierSlopeLTEZero
  , crossSplitLimit
  , bezAlong
  , bezAthwart
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Util.Debug

import Control.Lens

crossSplitLimit :: (Space s) => s
crossSplitLimit = 1 / 1024

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

outsideOfRange :: ( Axis axis
                  , Space s
                  )
               => axis
               -> Along   axis s
               -> Athwart axis s
               -> Along   axis s
               -> Box s
               -> Bool
outsideOfRange axis start baseline end box =
    baseline >  box ^. maxBox . athwart axis ||
    baseline <= box ^. minBox . athwart axis ||
    start    >  box ^. maxBox . along   axis ||
    end      <= box ^. minBox . along   axis

-- This is written with a little odd recursion using checkGo so that it matches the finite stack based GPU code better.
crossesBezierAlong :: forall axis s
                   . (Axis axis, Space s)
                   => axis
                   -> Along axis s
                   -> Athwart axis s
                   -> Along axis s
                   -> Bezier s
                   -> Bool
crossesBezierAlong axis start baseline end initBez =
  --tc ("crossesBezierAlong " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
  if start == end
  then False
  else
  if start > end
  then crossesBezierAlong axis end baseline start initBez
  else checkGo initBez
  where
  checkGo :: Bezier s -> Bool
  checkGo bez =
    let box = boxOf bez
    in  if outsideOfRange axis start baseline end box
        then False
        else go bez box
  go :: Bezier s -> Box s -> Bool
  go bez box =
     --tcDepth depth ("go " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
     let minAthwart = box ^. minBox . athwart axis
         maxAthwart = box ^. maxBox . athwart axis
         minAlong   = box ^. minBox . along   axis
         maxAlong   = box ^. maxBox . along   axis
         size = fromAlong axis (maxAlong - minAlong) `max` fromAthwart axis (maxAthwart - minAthwart)
         slopeLTEZero = bezierSlopeLTEZero axis bez
         offBaseline = baseline /= maxAthwart
         isK = isKnobAbsolute axis bez || isKnobAbsolute (perpendicularTo axis) bez
     in
     if  size >= crossSplitLimit &&
        (
         -- curve size remains greater than the limit
         offBaseline &&
         (start > box ^. minBox . along axis || end <= box ^. maxBox . along axis) -- and the start or end points are somewhere inside curve limits
        )
        || isK -- or the curve creates a knob, meaning there could be more than one cross point
     then -- must split
          let (lessBez, moreBez) = splitBezier 0.5 bez
         in   checkGo lessBez
              /=
              checkGo moreBez
     else
          let barrierMin = slopeLTEZero || (not slopeLTEZero && (offBaseline && isVertical axis))
              barrier    = if barrierMin then minAlong else maxAlong
              startLTE   = slopeLTEZero || (not slopeLTEZero && (offBaseline || isHorizontal axis))
          in  if startLTE
              then start <= barrier && end >  barrier
              else start <  barrier && end >= barrier

-- This is an implementation of crossesBezierAlong that doesn't short circuit if the maximum side of the curve is on the baseline.
crossesBezierAlongNoShort :: forall axis s
                          . (Axis axis, Space s)
                          => axis
                          -> Along axis s
                          -> Athwart axis s
                          -> Along axis s
                          -> Bezier s
                          -> Bool
crossesBezierAlongNoShort axis start baseline end bez =
  --tc ("crossesBezierAlong " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
  if start == end
  then False
  else
  if start > end
  then crossesBezierAlong axis end baseline start bez
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

crossesBezier :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crossesBezier start end bez =
    let iP = interimPoint start end
    in
    crossesBezierAlong Vertical   (start ^. pY) (start ^. pX) (iP  ^. pY) bez /=
    crossesBezierAlong Horizontal (iP    ^. pX) (iP    ^. pY) (end ^. pX) bez

crossesBezierHorizontal :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crossesBezierHorizontal start end bez =
    crossesBezierAlong Horizontal (start ^. pX) (end ^. pY) (end ^. pX) bez

crossesBezierVertical :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crossesBezierVertical start end bez =
    crossesBezierAlong Vertical   (start ^. pY) (start ^. pX) (end  ^. pY) bez
