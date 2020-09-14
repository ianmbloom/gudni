

class LessPoint axis where
    lessPoint :: axis -> Bool

instance LessPoint Horizontal where
    lessPoint Horizontal = True

instance LessPoint Vertical where
    lessPoint Vertical = False

limit = 0.125

lessCurve :: (Axis axis) => axis -> Point2 s -> Bezier s -> Bool
lessCurve axis point bez =
     if start ^. along axis < box ^. minBox . along axis
     then True
     else if start ^. along axis > box ^. minBox . along axis
          then False
          else if taxiDistance start (box ^. minBox) < limit
               then lessPoint axis
               else let (lessBez, moreBez) = splitBezier 0.5 bez
                    in
                    if p ^. athwart axis <= lessBez ^. bzEnd . athwart axis
                    then go lessBez
                    else go moreBez


crossesAlong :: (Axis axis) => axis -> Point2 s ->
crossesAlong axis start end bez =
    let box = curveEndPointsBox bez
        oBez = orderedCurve bez
    if start ^. athwart axis < box ^. minBox . athwart axis || start ^. withAxis >= box ^. maxBox . athwart axis
    then False
    else lessCurve axis start bez /= lessCurve axis end bez
