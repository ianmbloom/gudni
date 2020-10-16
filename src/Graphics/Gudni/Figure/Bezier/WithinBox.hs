{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Figure.Bezier.WithinBox
  ( curveWithinBox
  )
where

import Graphics.Gudni.Figure.Principle.Space
import Graphics.Gudni.Figure.Principle.Axis
import Graphics.Gudni.Figure.Principle.Point
import Graphics.Gudni.Figure.Principle.Box
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Figure.Bezier.Cross
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Applicative

trDP depth message = id -- trDepth (depth + 1) message
tcDP depth message = id -- tcDepth (depth + 1) message

pointIsInsideBox :: ( Ord s
                    )
                 => Box s
                 -> Point2 s
                 -> Bool
pointIsInsideBox box point =
    point ^. pX >  box ^. minBox . pX &&
    point ^. pY >  box ^. minBox . pY &&
    point ^. pX <= box ^. maxBox . pX &&
    point ^. pY <= box ^. maxBox . pY

curveWithinBox :: forall s
               . ( Space s
                 )
               => Box s
               -> Bezier s
               -> [Bezier s]
curveWithinBox box bez =
  --tc ("crossesAlong " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
  go bez
  where
  axis :: Vertical
  axis = Vertical
  go :: Bezier s -> [Bezier s]
  go bez =
    --tcDepth depth ("go " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
    let minAthwart = bezAthwart axis min bez
        maxAthwart = bezAthwart axis max bez
        minAlong   = bezAlong   axis min bez
        maxAlong   = bezAlong   axis max bez
        (lessBez, moreBez) = splitBezier 0.5 bez
    in  if box ^. minBox . athwart axis >  maxAthwart ||
           box ^. maxBox . athwart axis <= minAthwart ||
           box ^. minBox . along   axis >  maxAlong      ||
           box ^. maxBox . along   axis <= minAlong
        then -- segment is totally outside the range of curve
             []
        else let size = fromAlong axis (maxAlong - minAlong) `max` fromAthwart axis (maxAthwart - minAthwart)
                 slopeLTEZero = bezierSlopeLTEZero axis bez
                 isK = isKnobAbsolute axis bez || isKnobAbsolute (perpendicularTo axis) bez
             in
             if  -- curve size remains greater than the limit
                 size >= crossSplitLimit &&
                 not (pointIsInsideBox box (bez ^. bzStart) || pointIsInsideBox box (bez ^. bzEnd))
                 || isK -- or the curve creates a knob, meaning there could be more than one cross point
             then -- must split
                  go lessBez <|> go moreBez
             else pure bez
