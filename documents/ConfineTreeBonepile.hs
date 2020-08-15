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
   if box ^. maxBox . with axis <= point ^. with axis
   then ([box], [])
   else if box ^. minBox . with axis >= point ^. with axis
   then ([], [box])
   else let (less, more) = splitBox axis (point ^. with axis) box
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
        if point ^. with axis < tree ^. confineOverhang
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
