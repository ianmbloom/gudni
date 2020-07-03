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
