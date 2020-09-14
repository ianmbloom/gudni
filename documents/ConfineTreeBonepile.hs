data Side s =
  Side
  { _cutAxis     :: Axis
  , _cutPoint    :: s
  , _rangeStart  :: s
  , _rangeEnd    :: s
  } deriving (Generic)
makeLenses ''Side
instance Out s => Out (Side s)

leftRange   box = Side Vertical   (box ^. leftSide  ) (box ^. topSide   ) (box ^. bottomSide)
rightRange  box = Side Vertical   (box ^. rightSide ) (box ^. bottomSide) (box ^. topSide   )
topRange    box = Side Horizontal (box ^. topSide   ) (box ^. leftSide  ) (box ^. rightSide )
bottomRange box = Side Horizontal (box ^. bottomSide) (box ^. rightSide ) (box ^. leftSide  )

allSides box = [leftRange box, topRange box, rightRange box, bottomRange box]


inRange start end p = (p + iota) >= min start end && (p - iota) <= max start end

breakSide :: (Out s, Space s) => Bezier s -> Side s -> Either (Side s, Side s) (Side s)
breakSide bez side =
  let rangeAxis = if isVertical side then pY else pX
      mCutT = tr "mCutT" $ maybeCutPointBezier ((trP "side" side) ^. cutAxis) (side ^. cutPoint) bez
  in
  case mCutT of
      Just cutT ->
         let p = tr "p" $ eval cutT bez ^. rangeAxis
         in  if inRange (side ^. rangeStart) (side ^. rangeEnd) p
             then Left (set rangeEnd p side, set rangeStart p side)
             else Right side
      Nothing -> Right side

breakSides :: (Out s, Space s) => Bezier s -> [Side s] -> ([Side s], [Side s])
breakSides bez (s:ss) =
    case breakSide bez s of
        Left (a, b) -> ([a], b:ss)
        Right a -> let (bs, cs) = breakSides bez ss
                   in  (a:bs, cs)
breakSides bez [] = ([],[])

isVertical side = side ^. cutAxis == Vertical

sideToLine :: (Out s, Space s) => Side s -> Bezier s
sideToLine side =
  let cutAxisLens = if isVertical side then pX else pY
      rangeAxis   = if isVertical side then pY else pX
      pStart      = set cutAxisLens (side ^. cutPoint) . set rangeAxis (side ^. rangeStart) $ zeroPoint
      pEnd        = set cutAxisLens (side ^. cutPoint) . set rangeAxis (side ^. rangeEnd  ) $ zeroPoint
  in  line pStart pEnd

breakBox :: (Out s, Space s) => Bezier s -> Box s -> ([Bezier s], [Bezier s])
breakBox bez box =
  let (firstCut, secondCut) = breakSides (trP "bez" bez) (allSides (trP "box" box))
      (secondCut', thirdCut) = breakSides bez secondCut
      firstLoop = bez:map sideToLine (thirdCut ++ firstCut)
      secondLoop = bez:map sideToLine secondCut
  in  (trP "firstLoop" firstLoop, trP "secondLoop" secondLoop)

constructCurve :: (Out s, Space s) => Box s -> Bezier s -> ShapeTree token s
constructCurve boundary bez =
  let (firstLoop, secondLoop) = breakBox bez boundary
      first  = withColor orange       . mask . shapeFrom . makeOutline $ firstLoop
      second = withColor (light blue) . mask . shapeFrom . makeOutline $ secondLoop
  in  overlap [first, second]

-----------------------------------------------

overPixelStack :: ([Layer] -> [Layer]) -> (Box s, [Layer]) -> (Box s, [Layer])
overPixelStack f (pixel, stack) = (pixel, f stack)

splitBoxAcrossPoint :: (Space s, Axis axis) => axis -> Box s -> Point2 s -> ([Box s], [Box s])
splitBoxAcrossPoint axis box point =
   if box ^. maxBox . athwart axis <= point ^. athwart axis
   then ([box], [])
   else if box ^. minBox . athwart axis >= point ^. athwart axis
   then ([], [box])
   else let (less, more) = splitBox axis (point ^. athwart axis) box
        in  ([less], [more])

splitAcrossBezier :: (Space s, Axis axis) => axis -> Box s -> Bezier s -> ([Box s], [Box s], [Box s])
splitAcrossBezier axis box bez =
  let oBez                  = orderedBezier axis bez
      (mLessEnd, mMoreEnd)  = splitBoxAcrossPoint axis box (oBez ^. bzEnd)
      (mLessStart, mMiddle) = case mLessEnd of
                                  [] -> ([], [])
                                  [lessEnd] -> splitBoxAcrossPoint axis box (oBez ^. bzStart)
                                  _ -> error "impossible"
  in  (mLessStart, mMiddle, mMoreEnd)

breakWithCurve :: (Space s) => Confine axis s -> Box s -> [Box s]
breakWithCurve tree box =
  go box (orderedBezier Vertical $ tree ^.  confineCurve)
  where
  go :: (Space s) => Box s -> Bezier s -> [Box s]
  go box bez =
      let minAxis = min (bez ^. bzStart . along Vertical) (bez ^. bzEnd . along Vertical)
          maxAxis = max (bez ^. bzStart . along Vertical) (bez ^. bzEnd . along Vertical)
      in  if box ^. minBox . along Vertical >= maxAxis
          then pure box
          else if box ^. maxBox . along Vertical <= minAxis
               then pure box
               else let (lessBez, moreBez) = splitBezier 0.5 bez
                        center = lessBez ^. bzEnd
                        (lessBox, moreBox) = splitBox Vertical (center ^. with Vertical) box
                    in
                    if   box ^. maxBox . with Vertical <= center ^. with Vertical
                    then go box lessBez
                    else if   box ^. minBox . with Vertical >= center ^. with Vertical
                         then go box moreBez
                         else go lessBox lessBez <|> go moreBox moreBez

breakPixel :: forall s . (Space s) => ConfineTree s -> Box s -> [Box s]
breakPixel mTree pixel =
    go Vertical mTree [pixel]
    where
    go :: (Axis axis, Switcher axis) => axis -> Branch axis s -> [Box s] -> [Box s]
    go axis mTree = concatMap (goPixel axis mTree)

    goPixel :: (Axis axis, Switcher axis) => axis -> Branch axis s -> Box s -> [Box s]
    goPixel axis mTree pixel =
        case mTree of
            Nothing -> [pixel]
            Just tree ->
              let (mLessStart, mMiddle, mMoreEnd) = splitAcrossBezier Vertical pixel (tree ^. confineCurve)
              in  lessSwitch axis (pixel ^. minBox) tree (go (nextAxis axis)) .
                  moreSwitch axis (pixel ^. maxBox) tree (go (nextAxis axis)) -- .
                  --concatMap (breakWithCurve tree)
                  $ mLessStart ++ mMiddle ++ mMoreEnd

  , Layer(..)
  , layItemTagId
  , layWinding
  , layTags

data Layer = Layer
   { _layItemTagId :: ItemTagId
   , _layWinding   :: WindingNumber
   , _layTags      :: [Int]
   }
makeLenses ''Layer

overLayer :: (WindingNumber -> WindingNumber) -> Confine axis s -> [Layer] -> [Layer]
overLayer f tree stack =
  let itemTagId = tree ^. confineItemTagId
      tag       = tree ^. confineCurveTag
      newLayer = Layer { _layItemTagId = itemTagId
                       , _layWinding   = f 0
                       , _layTags      = [tag]
                       }
  in  case stack of
           (x:xs) | itemTagId <  x ^. layItemTagId -> newLayer:x:xs
                  | itemTagId == x ^. layItemTagId -> (over layWinding f . over layTags (++[tag]) $ x):xs
                  | itemTagId  > x ^. layItemTagId -> x:overLayer f tree xs
           [] -> [newLayer]

class Switcher (NextAxis axis) => Switcher axis where
    lessSwitch :: (Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a

instance Switcher Vertical where
    lessSwitch axis point tree goNext =
        if point ^. athwart axis < tree ^. confineOverhang
        then goNext (tree ^. confineLessCut)
        else id

instance Switcher Horizontal where
    lessSwitch axis point tree goNext=
        goNext (tree ^. confineLessCut)

go Vertical mTree []
where
go :: (Axis axis) => axis -> Branch axis s -> [ItemTagId] -> [ItemTagId]
go axis mTree  =
  case mTree of
    Nothing -> foldl (overCurve  axis point tree (toggleCrossing (tree ^. confineItemTagId)))
    Just tree ->
        lessCut axis point tree (go (nextAxis axis)) .
        moreCut axis point tree (go (nextAxis axis)) .
        flip (foldl (flip toggleCrossing)) (tree ^. confineCrossings)

overCurve :: (Axis axis, Space s) => axis -> Point2 s -> Confine axis s -> (a -> a) -> a -> a
overCurve axis point tree f =
    let oBez = orderedBezier Vertical (tree ^. confineCurve)
    in
    if point ^. athwart axis >= tree ^. confineCut &&
       inRangeBez Vertical point oBez
    then if lessThanBezier Vertical point oBez
         then id
         else f
    else id

----------------------


-- These assumes the bezier is ordered on axis
sizeOnAxis :: (Space s, Axis axis) => axis -> Bezier s -> s
sizeOnAxis axis bez = bez ^. bzEnd . athwart axis - bez ^. bzStart . athwart axis

aboveRangeMin :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
aboveRangeMin axis p bez = p ^. athwart axis >= bez ^. bzStart . athwart axis

belowRangeMax :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
belowRangeMax axis p bez = p ^. athwart axis < bez ^. bzEnd . athwart axis

belowRangeMin :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
belowRangeMin axis p = not . aboveRangeMin axis p

aboveRangeMax :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
aboveRangeMax axis p = not . belowRangeMax axis p

-- This assumes the bezier is ordered on axis
inRangeBez :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
inRangeBez axis p bez =
    -- tr ("inRangeBez axis " ++ show axis ++ " p " ++ show p) $
    aboveRangeMin axis p bez && belowRangeMax axis p bez

-- This assumes the bezier is ordered on axis
lessThanBezier :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
lessThanBezier axis p = go
  where
  go bez =
    let maxAxis = max (bez ^. bzStart . along axis) (bez ^. bzEnd . along axis)
        minAxis = min (bez ^. bzStart . along axis) (bez ^. bzEnd . along axis)
    in
    if p ^. along axis <= minAxis
    then True
    else if p ^. along axis > maxAxis || sizeOnAxis axis bez <= iota
         then False
         else if p ^. athwart axis == bez ^. bzStart . athwart axis
              then p ^. along axis <= bez ^. bzStart . along axis
              else let (less, more) = splitBezier 0.5 bez
                   in
                   if p ^. athwart axis <= less ^. bzEnd . athwart axis
                   then go less
                   else go more

, inRangeBez

interimPointInverse :: Point2 s -> Point2 s -> Point2 s
interimPointInverse start end = Point2 (end ^. pX) (start ^. pY)
-- Using /= for xor

crossWithAxis :: (Axis axis, Space s) => axis -> Point2 s -> Point2 s -> Bezier s -> Bool
crossWithAxis axis start end bez =
    let ip = interimPoint start end
        oBez = orderedBezier axis bez
    in  inRangeBez axis ip oBez &&
        (lessThanBezier axis ip  oBez /=
         lessThanBezier axis end oBez)

crosses start end bez =
  let a = crosses' start end bez
      b = crosses' end start bez
  in
  if a == b
  then a
  else error $ "crosses " ++ show start ++ " end " ++ show end ++ " bez " ++ show bez

crosses' :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crosses' start' end' bez =
    let (start, end) = if start' ^. pX <= end' ^. pX then (start', end') else (end', start')
    in
    crossesWithAxis Horizontal start end bez /=
    crossesWithAxis Vertical   start end bez

aboveRangeMin :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
aboveRangeMin axis p bez =
  let start = bez ^. bzStart . athwart axis
      end   = bez ^. bzEnd   . athwart axis
      c     = p ^. athwart axis
  in  if start <= end
      then c >= start
      else c >  end

belowRangeMax :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
belowRangeMax axis p bez =
  let start = bez ^. bzStart . athwart axis
      end   = bez ^. bzEnd   . athwart axis
      c     = p ^. athwart axis
  in  if start <= end
      then c < end
      else c <= start


lessCurve :: (Axis axis, LessPoint axis, Space s) => axis -> Point2 s -> Bezier s -> Bool
lessCurve axis point = go
    where
    go bez =
       tc ("lessCurve " ++ show point ++ " bez " ++ show bez ) $
       let box = curveEndPointsBox bez
       in
       if point ^. athwart axis == bez ^. bzStart . athwart axis
       then if point ^. along axis == bez ^. bzStart . along axis
            then if positiveSlope axis bez
                 then lessPoint axis
                 else True
            else point ^. along axis < bez ^. bzStart . along axis
       else
             if tr "point ^. along axis <= box ^. minBox . along axis" $ point ^. along axis <= box ^. minBox . along axis
                  then True
                  else if tr "point ^. along axis > box ^. maxBox . along axis" $ point ^. along axis > box ^. maxBox . along axis
                       then False
                       else let (lessBez, moreBez) = splitBezier 0.5 bez
                            in
                            if tr "point ^. athwart axis < lessBez ^. bzEnd . athwart axis" $ point ^. athwart axis < lessBez ^. bzEnd . athwart axis
                            then go lessBez
                            else go moreBez

tracePoint :: forall m token
           .  (Monad m, Show token)
           => M.Map ItemTagId Color
           -> ConfineTree SubSpace
           -> Point2 SubSpace
           -> Int
           -> FontMonad m (ShapeTree token SubSpace)
tracePoint colorMap tree point i =
  let (stack, steps) = pointWinding tree point
      step = steps ^?! ix (clamp 0 (length steps - 1) i)
  in
  do  confine <- case step ^. stepConfine of
                   Left  vTree -> constructConfine Vertical   vTree reasonableBoundaries
                   Right hTree -> constructConfine Horizontal hTree reasonableBoundaries
      let corner = case step ^. stepConfine of
                      Left  vTree -> confineAnchorPoint vTree
                      Right hTree -> confineAnchorPoint hTree
      let string = "horiWind " ++ show (step ^. stepHoriWind) ++ "\nvertWind " ++ show (step ^. stepVertWind)
      text <- fromGlyph . withColor (dark green) . translateBy corner . translateByXY 4 0 . scaleBy 30 <$> paragraph 0.1 0.1 AlignMin AlignMin string
      label <- constructAnchorStack colorMap point (step ^. stepStack) []
      return . overlap $ [label, text, confine]

confineAnchorPoint :: (Axis axis, Space s)
                   => axis
                   -> s
                   -> Confine axis s
                   -> Point2 s
confineAnchorPoint axis parent tree = set (along axis) parent .
                                      set (athwart axis) (tree ^. confineCut) $
                                      zeroPoint

findAnchorPoint :: forall s
                .  (Space s)
                => ConfineTree s
                -> Point2 s
                -> Point2 s
findAnchorPoint mTree point = go Vertical mTree (With minBound) (With minBound)
    where
    go :: (Axis axis) => axis -> Branch axis s -> With (NextAxis axis) s -> With axis s -> Point2 s
    go axis mTree first next =
      case mTree of
        Nothing -> pointFromAxis axis next first
        Just tree ->
           let cut = tree ^. confineCut
           in
           if point ^. athwart axis >= unAxis cut
           then go (nextAxis axis) (tree ^. confineMoreCut) next cut
           else go (nextAxis axis) (tree ^. confineLessCut) next cut

{-
lessCurve :: (Axis axis, LessPoint axis, Space s)
          => axis
          -> Point2 s
          -> Box s
          -> Bezier s
          -> Bool
lessCurve axis point box = go box
    where
    go box oBez =
       if
          point ^. along axis < box ^. minBox . along axis
       then True
       else if
               point ^. along axis >= box ^. maxBox . along axis
            then False
            else if   positiveSlope axis oBez && taxiDistance point (oBez ^. bzStart) < limit
                 then lessPoint axis
                 else let (lessBez, moreBez) = splitBezier 0.5 oBez
                      in
                      if point ^. athwart axis < lessBez ^. bzEnd . athwart axis
                      then go (curveEndPointsBox lessBez) lessBez
                      else go (curveEndPointsBox moreBez) moreBez

outsideCurveRange :: (Axis axis, Ord s)
                  => axis
                  -> Point2 s
                  -> Box s
                  -> Bool
outsideCurveRange axis point box =
    point ^. athwart axis <  box ^. minBox . athwart axis ||
    point ^. athwart axis >= box ^. maxBox . athwart axis

lessCurve :: (Axis axis, LessPoint axis, Space s)
          => axis
          -> Point2 s
          -> Box s
          -> Bezier s
          -> Bool
lessCurve axis point box = go box
    where
    go box oBez =
       if point ^. along axis < box ^. minBox . along axis
       then True
       else if point ^. along axis >= box ^. maxBox . along axis
            then False
            else if   taxiDistance point (oBez ^. bzStart) < limit
                 then not (positiveSlope axis oBez) {- /= lessPoint axis -}
                 else let (lessBez, moreBez) = splitBezier 0.5 oBez
                      in
                      if point ^. athwart axis < lessBez ^. bzEnd . athwart axis
                      then go (curveEndPointsBox lessBez) lessBez
                      else go (curveEndPointsBox moreBez) moreBez
-}



outsideCurveRange :: ( Axis axis
                     , Space s
                     )
                  => axis
                  -> Point2 s
                  -> Bezier s
                  -> Bool
outsideCurveRange axis p oBez =
    let mx = max (oBez ^. bzStart . athwart axis) (oBez ^. bzEnd . athwart axis)
        mn = min (oBez ^. bzStart . athwart axis) (oBez ^. bzEnd . athwart axis)
    in  p ^. athwart axis < mn || p ^. athwart axis > mx || p ^. athwart axis == oBez ^. bzStart . athwart axis

{-
    in
    if outsideCurveRange axis start oBez || start == end
    then False
    else lessCurve axis start oBez /= lessCurve axis end oBez
-}


countCross :: ( Axis axis
              , Space s
              )
           => axis
           -> s
           -> s
           -> s
           -> Bezier s
           -> Int
countCross axis baseline start end = go
    where
    go bez =
        let mxAlong   = bezAlong max
            mnAlong   = bezAlong min
            mxAthwart = bezAthwart max
            mnAthwart = bezAthwart min
            startBez  = bez ^. bzStart
            endBez    = bez ^. bzEnd
        in
        if baseline < mnAthwart && baseline >= mxAthwart
        then 0
        else
          let needSplit = (start >= mnAlong && start < mxAlong) || (end >= mnAlong && end < mxAlong)
              (lessBez, moreBez) = splitBezier 0.5 oBez
          in  if needSplit
              then crossCount axis baseline start end + crossCount axis baseline start end
              else if (start < mnAlong && end >= mxAlong) || (start >= mxAlong && end < mnAlong)
                   then 1
                   else 0

lessCurve :: ( Axis axis
             , Space s
             )
          => axis
          -> Point2 s
          -> Bezier s
          -> Bool
lessCurve axis p = go
    where
    go oBez =
        let mx = max (oBez ^. bzStart . along axis) (oBez ^. bzEnd . along axis)
            mn = min (oBez ^. bzStart . along axis) (oBez ^. bzEnd . along axis)
            start = oBez ^. bzStart
            end   = oBez ^. bzEnd
        in
        if taxiDistance p end < limit
        then not (positiveSlope axis oBez)
        else if   p ^. along axis < mn || p ^. along axis == start ^. along axis
             then True
             else if p ^. along axis > mx || p ^. along axis == start ^. along axis
                  then False
                  else let (lessBez, moreBez) = splitBezier 0.5 oBez
                       in
                       if (lessBez ^. bzStart . athwart axis < moreBez ^. bzStart . athwart axis && p ^. athwart axis <  moreBez ^. bzStart . athwart axis) ||
                          (lessBez ^. bzStart . athwart axis > moreBez ^. bzStart . athwart axis && p ^. athwart axis >= lessBez ^. bzEnd   . athwart axis)

                       then go lessBez
                       else go moreBez

countCross :: ( Axis axis
              , Space s
              , LessPoint axis
              )
           => axis
           -> s
           -> s
           -> s
           -> Bezier s
           -> Int
countCross axis baseline start end bez = tc "countCross" $
    if start == end then 0 else
    if start > end then countCross axis baseline end start bez
    else prime bez
    where
    pSlope = positiveSlope axis bez
    prime bez =
        tc "prime" $
        let maxAthwart = bezAthwart axis max bez
            minAthwart = bezAthwart axis min bez
        in  if baseline >= minAthwart && baseline < maxAthwart
            then go bez
            else 0
    go bez =
        tc "go" $
        let maxAlong   = bezAlong axis max bez
            minAlong   = bezAlong axis min bez
            -- leftPoint = if bez ^. bzStart
            minSide = if   bez ^. bzStart . along axis <=
                           bez ^. bzEnd   . along axis
                      then bez ^. bzStart . athwart axis
                      else bez ^. bzEnd   . athwart axis
            (lessBez, moreBez) = splitBezier 0.5 bez
            shouldSplit = (maxAlong - minAlong > limit) &&
                          ( (start > minAlong && start < maxAlong) ||
                            (end   > minAlong && end   < maxAlong)
                          )
        in  if shouldSplit
            then prime lessBez + prime moreBez
            else if end >= maxAlong && (start < minAlong || (start == minAlong && comp axis baseline minSide))
                 then 1
                 else 0

countCross :: ( Axis axis
              , Space s
              , LessPoint axis
              )
           => axis
           -> s
           -> s
           -> s
           -> Bezier s
           -> Int
countCross axis baseline start end bez = tc "countCross" $
    if start == end then 0 else
    if start > end then countCross axis baseline end start bez
    else prime bez
    where
    prime bez =
        tc "prime" $
        let maxAthwart = bezAthwart axis max bez
            minAthwart = bezAthwart axis min bez
        in  if baseline >= minAthwart && baseline <= maxAthwart && baseline /= bez ^. bzEnd . athwart axis
            then go bez
            else 0
    go bez =
        tc "go" $
        let maxAlong   = bezAlong axis max bez
            minAlong   = bezAlong axis min bez
            -- leftPoint = if bez ^. bzStart
            minSide = if   bez ^. bzStart . along axis <=
                           bez ^. bzEnd   . along axis
                      then bez ^. bzStart . athwart axis
                      else bez ^. bzEnd   . athwart axis
            (lessBez, moreBez) = splitBezier 0.5 bez
            shouldSplit = (maxAlong - minAlong > limit) &&
                          ( (start > minAlong && start < maxAlong) ||
                            (end   > minAlong && end   < maxAlong)
                          )
        in  if shouldSplit
            then prime lessBez + prime moreBez
            else
            let startLess = ( start <= minAlong && start /= bez ^. bzEnd . along axis )
                            ||
                            ( start    == bez ^. bzStart . along   axis &&
                              baseline == bez ^. bzStart . athwart axis && isVertical axis
                            )
                endLess   = ( end   >= maxAlong && end   /= bez ^. bzEnd . along axis )
                            ||
                            ( end      == bez ^. bzStart . along   axis &&
                              baseline == bez ^. bzStart . athwart axis && isVertical axis
                            )


              if startLess /= endLess
              then 1
              else 0
