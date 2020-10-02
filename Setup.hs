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
  else
    go 2 bez
  where
  go :: Int -> Bezier s -> Bool
  go depth bez =
    --tcDepth depth ("go " ++ show axis ++ " baseline "++ show baseline ++ " start " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez ) $
    let minAthwart = trDP depth "minAthwart" $ bezAthwart axis min bez
        maxAthwart = trDP depth "maxAthwart" $ bezAthwart axis max bez
        minAlong   = trDP depth "minAlong  " $ bezAlong   axis min bez
        maxAlong   = trDP depth "maxAlong  " $ bezAlong   axis max bez
        (lessBez, moreBez) = splitBezier 0.5 bez
    in  if trDP depth "totally outside" $
           ( trDP depth "baseline >  maxAthwart" $ baseline >  maxAthwart) ||
           ( trDP depth "baseline <= minAthwart" $ baseline <= minAthwart) ||
           ( trDP depth "     start >  maxAlong" $ start >  maxAlong     ) ||
           ( trDP depth "     end   <= minAlong" $ end   <= minAlong     )
        then -- segment is totally outside the range of curve
             False
        else let --size :: s
                 size = fromAlong axis (maxAlong - minAlong) `max` fromAthwart axis (maxAthwart - minAthwart)
                 slopeLTEZero = trDP depth "slopeLTEZero" $
                                bezierSlopeLTEZero axis bez
                 -- barSide = isHorizontal axis || bezierSlopeLTEZero axis bez
                 offBaseline = trDP depth "offBaseLine" $
                               baseline /= maxAthwart
                 --oppose  = if isHorizontal axis || bezierSlopeLTEZero axis bez then maxAlong else minAlong
                 isK = trDP depth "ISK" $
                       isKnobAbsolute axis bez || isKnobAbsolute (perpendicularTo axis) bez
             in
             if  trDP depth "mustSplit" $
                 size >= limit &&
                (
                 -- curve size remains greater than the limit
                 offBaseline &&
                 trDP depth "||" (trDP depth "start > minAlong" (start > minAlong)|| trDP depth "end <= maxAlong" (end <= maxAlong)) -- and the start or end points are somewhere inside curve limits
                )
                || isK -- or the curve creates a knob, meaning there could be more than one cross point
             then -- must split
                  go (depth + 2) lessBez /= go (depth + 2) moreBez
             else
                  let barrierMin = {-trDP depth "barrierMin"-} slopeLTEZero || (not slopeLTEZero && (offBaseline && isVertical axis))
                      barrier     = trDP depth "barrier"    $  if barrierMin then minAlong else maxAlong
                      startLTE    = slopeLTEZero || (not slopeLTEZero && (offBaseline || isHorizontal axis)) -- tested
                  in  if startLTE
                      then start <= barrier && end > barrier
                      else start < barrier  && end >= barrier
