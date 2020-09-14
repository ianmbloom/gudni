boxAbovePoint :: Space s => Point2 s -> Box s -> Bool
boxAbovePoint p bx =
    inHorizontalRangeBox p bx && p ^. pY >= bx ^. bottomSide

boxLeftOfPoint :: Space s => Point2 s -> Box s -> Bool
boxLeftOfPoint p bx =
    inVerticalRangeBox p bx && p ^. pX >= bx ^. rightSide


boxSectionsVertical cutBox box =
  let (top, rest)      = splitBox Horizontal (cutBox ^. topSide) box
      (middle, bottom) = splitBox Horizontal (cutBox ^. bottomSide) rest
  in  (top, middle, bottom)

boxSectionsHorizontal cutBox box =
  let (left, rest)    = splitBox Vertical (cutBox ^. leftSide ) box
      (middle, right) = splitBox Vertical (cutBox ^. rightSide) rest
  in  (left, middle, right)

trimCurve :: forall s . (Space s) => Box s -> Bezier s -> Maybe (Bezier s)
trimCurve box sourceBez = (trimVertical box >=> trimHorizontal box) sourceBez
    where
    bezBox = boxOf sourceBez
    trim :: (Space s)
         => Lens' (Box s) s
         -> Axis
         -> (s -> s -> Bool)
         -> ((Bezier s, Bezier s) -> Bezier s)
         -> Box s
         -> Bezier s
         -> Bezier s
    trim side axis comp f box bez =
        if (boxOf bez ^. side) `comp` (box ^. side)
        then f $ splitAtCut axis (box ^. side) bez
        else bez

    trimVertical :: Box s
                 -> Bezier s
                 -> Maybe (Bezier s)
    trimVertical box bez =
        if (bezBox ^. rightSide > box ^. leftSide) && (bezBox ^. leftSide < box ^. rightSide)
        then Just $ trim leftSide   Vertical   (<) snd box .
                    trim rightSide  Vertical   (>) fst box $ bez
        else Nothing

    trimHorizontal :: Box s
                   -> Bezier s
                   -> Maybe (Bezier s)
    trimHorizontal box bez =
        if (bezBox ^. bottomSide > box ^. topSide) && (bezBox ^. topSide < box ^. bottomSide)
        then Just $ trim topSide    Horizontal (<) snd box .
                    trim bottomSide Horizontal (>) fst box $ bez
        else Nothing


confineTreeBoxes :: forall s . Space s => ConfineTree s -> [Box s]
confineTreeBoxes tree = go Vertical tree reasonableBoundaries
  where
  go :: IsAxis axis => axis -> Branch axis s -> Box s -> [Box s]
  go thisAxis mTree boundary =
       if widthOf boundary > 0 && heightOf boundary > 0
       then case mTree of
                Nothing -> [boundary]
                Just tree ->
                    let cut = tree ^. confineCut
                        (lessBound, moreBound) = splitBox thisAxis cut boundary
                        lessBranch = go (nextAxis thisAxis) (tree ^.  confineLessCut) lessBound
                        moreBranch = go (nextAxis thisAxis) (tree ^.  confineMoreCut) moreBound
                    in  lessBranch ++ moreBranch
       else [boundary]


curvesInBox :: forall s . Space s => ConfineTree s -> Box s -> [Bezier s]
curvesInBox tree box = catMaybes . map (trimCurve box) . go Vertical $ tree
    where
    go :: IsAxis axis => axis -> Branch axis s -> [Bezier s]
    go thisAxis mTree =
        let axis = pick thisAxis
        in
        case mTree of
            Nothing -> []
            Just tree ->
                let cut        = tree ^. confineCut
                    overhang   = tree ^. confineOverhang
                    curve      = tree ^. confineCurve
                    lessBranch = if overhang > box ^. minBox . axis then         go (nextAxis thisAxis) (tree ^.  confineLessCut) else []
                    moreBranch = if cut      < box ^. maxBox . axis then curve : go (nextAxis thisAxis) (tree ^.  confineMoreCut) else []
                in  lessBranch ++ moreBranch

constructConfineTreeFromBoxes :: forall s token . (Out s, Space s) => ConfineTree s -> ShapeTree token s
constructConfineTreeFromBoxes tree =
    overlap . map (drawCurve) . concatMap (curvesInBox tree) . confineTreeBoxes $ tree


drawCurve :: Space s => Bezier s -> ShapeTree token s
drawCurve bez =
  let s = bez ^. bzStart
      e = bez ^. bzEnd
      i = insideBoundaryPoint bez
  in  withColor (light green) . mask . shapeFrom $ makeOutline [line e i, line i s, bez]

goesRight :: Ord s => Bezier s -> Bool
goesRight bez = bez ^. bzStart . pX <= bez ^. bzEnd . pX

goesDown  :: Ord s => Bezier s -> Bool
goesDown  bez = bez ^. bzStart . pY <= bez ^. bzEnd . pY

outsideBoundaryPoint :: Ord s => Bezier s -> Point2 s
outsideBoundaryPoint bez =
    let x = if goesDown bez /= goesRight bez then bez ^. bzStart . pX else bez ^. bzEnd   . pX
        y = if goesDown bez == goesRight bez then bez ^. bzStart . pY else bez ^. bzEnd   . pY
    in  Point2 x y

insideBoundaryPoint :: Ord s => Bezier s -> Point2 s
insideBoundaryPoint bez =
    let x = if goesDown bez == goesRight bez then bez ^. bzStart . pX else bez ^. bzEnd . pX
        y = if goesDown bez /= goesRight bez then bez ^. bzStart . pY else bez ^. bzEnd . pY
    in  Point2 x y

confineToLeft :: (Ord s) => Bezier s -> Bool
confineToLeft bez = bez ^. bzStart . pY < bez ^. bzEnd . pY

confineAbove  :: (Ord s) => Bezier s -> Bool
confineAbove  bez = bez ^. bzStart . pX > bez ^. bzEnd . pX

clampBox :: (Ord s, Axis axis) => axis -> Box s -> s -> s
clampBox axis box = clamp (box ^. topLeftBox . pick axis) (box ^. bottomRightBox . pick axis)

taxiWindingBezier :: Space s => Point2 s -> Point2 s -> Box s -> Bezier s -> WindingNumber
taxiWindingBezier start end box bez =
    let interimPoint = Point2 (end ^. pX) (start ^. pY)
        crossesH = crossesHorizontal start end box bez
        crossesV = crossesVertical   start end box bez
    in  crossesH + crossesV


if tr "p ^. pick axis == bez ^. bzStart . pick axis" $ p ^. pick axis == bez ^. bzEnd . pick axis
     then tr "p ^. pickNext axis >= bez ^. bzEnd . pickNext axis" $ p ^. pickNext axis >= bez ^. bzEnd . pickNext axis
     else if tr "p ^. pick axis == bez ^. bzEnd . pick axis" $ p ^. pick axis == bez ^. bzEnd . pick axis
          then False
          else

isLessThanCurve :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s ->  Bool
isLessThanCurve axis point bezier =
  if curveOnAxis axis bezier
  then False
  else let t = findCutBezier axis (point ^. pickNext axis) bezier
           bPoint = eval t bezier
       in  point ^. pick axis < bPoint ^. pick axis


makeSearchBox :: [Anchor s] -> Box s
makeSearchBox anchors = foldl1 minMaxBox . map (boxOf) $ point : map (view ancPoint) anchors


-------------------

V.mapM_ (\(curve, tag) -> conConfineTree %= windConfineTree itemTagId tag curve) curveTags

, Anchor(..)
, ancItemTagId
, ancTag
, ancPoint
, ancWinding
, ancCornerWinding
, ancCount
, Step(..)
, stepSearchBox
, stepHoriWind
, stepVertWind
, stepStack
, stepConfine
, windConfineTree

pointWinding :: forall s . (Reasonable s, Space s) => ConfineTree s -> Point2 s -> ([Anchor s], [Step s])
pointWinding tree point = tc ("pointWinding " ++ show point) $
  go Vertical 0 tree (tr "searchStack" searchStack, [])
  where
  searchStack = goFindSearchStack Vertical tree []
  searchBox   = makeSearchBox searchStack
  goFindSearchStack :: Axis axis => axis -> Branch axis s -> [Anchor s] -> [Anchor s]
  goFindSearchStack axis mTree =
      case mTree of
          Nothing -> trace "goFind Nothing" . id
          Just tree ->
              let cut         = tree ^. confineCut
                  overhang    = tree ^. confineOverhang
                  itemTagId   = tree ^. confineItemTagId
                  pointFromAxis = confineAnchorPoint tree
                  anchor      = Anc { _ancItemTagId     = itemTagId
                                    , _ancTag           = tree ^. confineCurveTag
                                    , _ancPoint         = pointFromAxis
                                    , _ancWinding       = 0
                                    , _ancCornerWinding = tree ^. confineCornerWinding
                                    , _ancCount         = 0
                                    }
                  overMore    = if point ^. pick axis >= cut      then trace ("goFind more " ++ show axis ++ " tag " ++ show (tree ^. confineCurveTag)) . goFindSearchStack (nextAxis axis) (tree ^. confineMoreCut)  else id
                  overLess    = if point ^. pick axis <  overhang then trace ("goFind less " ++ show axis ++ " tag " ++ show (tree ^. confineCurveTag)) . goFindSearchStack (nextAxis axis) (tree ^. confineLessCut ) else id
                  addLayer    = if point ^. pick axis >= cut
                                then trace ("insert " ++ show itemTagId ++ " tag " ++ show (tree ^. confineCurveTag)) $ insertAnchor anchor
                                else id
              in  trace ("goFind " ++ show axis ++ " tag " ++ show (tree ^. confineCurveTag)) . addLayer . overLess . overMore

  makeSearchBox :: [Anchor s] -> Box s
  makeSearchBox anchors = foldl1 minMaxBox . map (boxOf) $ point : map (view ancPoint) anchors

  insertAnchor :: Anchor s -> [Anchor s] -> [Anchor s]
  insertAnchor a (x:xs)
      | a ^. ancItemTagId <  x ^. ancItemTagId = a:x:xs
      | a ^. ancItemTagId == x ^. ancItemTagId = x:xs
      | a ^. ancItemTagId >  x ^. ancItemTagId = x:insertAnchor a xs
  insertAnchor a [] = [a]

  findAnchor :: ItemTagId -> [Anchor s] -> Int
  findAnchor itemTagId = go 0
      where
      go i (x:xs)
          | itemTagId == x ^. ancItemTagId = i
          | itemTagId >  x ^. ancItemTagId = go (i+1) xs
          | otherwise = 0 -- error ("anchor " ++ show itemTagId ++ " not found")
      go i [] = 0 -- error ("anchor " ++ show itemTagId ++ " not found in empty list")

  go :: (Axis axis, EitherConfine axis) => axis -> Depth -> Branch axis s -> ([Anchor s],[Step s]) -> ([Anchor s],[Step s])
  go axis depth mTree (stack, steps) =
      case mTree of
          Nothing -> trace "go Nothing" (stack, steps)
          Just tree ->
              let cut            = tree ^. confineCut
                  overhang       = tree ^. confineOverhang
                  curve          = tree ^. confineCurve
                  --box            = curveEndPointsBox curve
                  itemTagId      = tree ^. confineItemTagId
                  anchorIndex    = findAnchor itemTagId stack
                  anchor         = stack ^?! ix anchorIndex
                  curveWindHori  = --tc ("crossesHorizontal i" ++ show (tree ^. confineCurveTag) ++ " anchor " ++ show (anchor ^. ancPoint) ++ " point " ++ show point) $
                                   crossesHorizontal  (anchor ^. ancPoint) point curve
                  curveWindVert  = --tc ("crossesVertical i" ++ show (tree ^. confineCurveTag) ++ " anchor " ++ show (anchor ^. ancPoint) ++ " point " ++ show point) $
                                   crossesVertical    (anchor ^. ancPoint) point  curve
                  curveWind      = curveWindHori + curveWindVert
                  moreStack = if searchBox ^. maxBox . pick axis >= cut      then trace ("go more " ++ show axis ++ " tag " ++ show (tree ^. confineCurveTag)) . go (nextAxis axis) (depth + 1) (tree ^. confineMoreCut) else id
                  lessStack = if searchBox ^. minBox . pick axis <  overhang then trace ("go less " ++ show axis ++ " tag " ++ show (tree ^. confineCurveTag)) . go (nextAxis axis) (depth + 1) (tree ^. confineLessCut) else id
                  fromCurve = if searchBox ^. maxBox . pick axis >= cut      then trace ("fromCurve " ++ show itemTagId ++ " tag " ++ show (tree ^. confineCurveTag)) . over _1 (over (ix anchorIndex . ancCount) (+1) . over (ix anchorIndex . ancWinding) (+curveWind)) else id
                  addStep (ss, steps) = if searchBox ^. maxBox . pick axis >= cut
                                        then let step stepStack =
                                                      Step { _stepSearchBox = searchBox
                                                           , _stepHoriWind  = curveWindHori
                                                           , _stepVertWind  = curveWindVert
                                                           , _stepStack     = stepStack
                                                           , _stepConfine   = storeConfine axis tree
                                                           }
                                             in (ss, step ss:steps)
                                        else (ss, steps)
              in  trace ("go " ++ show axis ++ " tag " ++ show (tree ^. confineCurveTag)) . addStep . fromCurve . lessStack . moreStack $ (stack, steps)

windConfineTree :: forall s . (Space s)
                => ItemTagId
                -> Int
                -> Bezier s
                -> ConfineTree s
                -> ConfineTree s
windConfineTree itemTagId tag bez = goWindV
    where
    oBez :: Bezier s
    oBez = orderedBezier Vertical bez
    goWindV :: Branch Vertical s -> Branch Vertical s
    goWindV mVNode =
      case mVNode of
        Nothing -> Nothing
        Just vTree ->
            let cut      = vTree ^. confineCut
                overhang = vTree ^. confineOverhang
                setLess  = if oBez ^. bzStart . pX  <  overhang then set confineLessCut (goWindH (vTree ^. confineLessCut )) else id
                setMore  = if oBez ^. bzEnd   . pX  >= cut      then set confineMoreCut (goWindH (vTree ^. confineMoreCut )) else id
            in  Just . setLess . setMore . setCornerWinding itemTagId tag oBez $ vTree
    goWindH :: Branch Horizontal s -> Branch Horizontal s
    goWindH mHNode =
      case mHNode of
        Nothing -> Nothing
        Just hTree ->
            let cut      = hTree ^. confineCut
                overhang = hTree ^. confineOverhang
                oBezTop  = min (oBez ^. bzStart . pY) (oBez ^. bzEnd . pY)
                setLess  = if oBezTop < overhang then set confineLessCut (goWindV (hTree ^. confineLessCut )) else id
                setMore  =                            set confineMoreCut (goWindV (hTree ^. confineMoreCut ))
            in  Just . setLess . setMore . setCornerWinding itemTagId tag oBez $ hTree


data Anchor s = Anc
  { _ancItemTagId :: ItemTagId
  , _ancTag       :: Int
  , _ancPoint     :: Point2 s
  , _ancWinding   :: WindingNumber
  , _ancCornerWinding :: WindingNumber
  , _ancCount     :: Int
  }
makeLenses ''Anchor

instance Show s => Show (Anchor s) where
    show anchor =  "item " ++ show (unItemTagId $ anchor ^. ancItemTagId) ++
                  " wind " ++ show (anchor ^. ancWinding) ++
                  " count " ++ show (anchor ^. ancCount) ++
                  " point " ++ show (anchor ^. ancPoint)

data Step s = Step
  { _stepSearchBox  :: Box s
  , _stepHoriWind  :: WindingNumber
  , _stepVertWind  :: WindingNumber
  , _stepStack    :: [Anchor s]
  , _stepConfine   :: Either (Confine Vertical s) (Confine Horizontal s)
  }
makeLenses ''Step

instance Show s => Show (Step s) where
  show step = "tag "   ++ show (eitherConfine confineCurveTag confineCurveTag (step ^. stepConfine)) ++
              " iT "   ++ show (eitherConfine confineItemTagId confineItemTagId (step ^. stepConfine)) ++
              " hori " ++ show (step ^. stepHoriWind) ++
              " vert " ++ show (step ^. stepVertWind)

windingEffect anchor oBez =
  if inRangeBez Vertical anchor oBez
  then (boolToWind $ lessThanBezier Vertical anchor oBez)
  else 0

setCornerWinding itemTagId tag oBez tree =
  let anchor  = confineAnchorPoint tree
  in
  if tree ^. confineItemTagId == itemTagId
  then over confineCornerHistory ((tag, windingEffect anchor oBez, oBez):) .
       over confineCornerWinding (+ windingEffect anchor oBez) $ tree
  else tree

anchorRing :: forall token s
           .  (Space s)
           => M.Map ItemTagId Color
           -> Point2 s
           -> Int
           -> Anchor s
           -> ShapeTree token s
anchorRing colorMap point layer anchor =
  let color = transparent 1 $ (M.!) colorMap (anchor ^. ancItemTagId)
  in  overlap [ if even (anchor ^. ancCornerWinding + anchor ^. ancWinding)
                then emptyItem
                else translateBy point $ ring (layer + 1) color
              , if point /= anchor ^. ancPoint
                then withColor (dark . dark $ color) . mask . shapeFrom . stroke 0.3 $ line point (anchor ^. ancPoint)
                else emptyItem
              ]

constructAnchorStack :: forall m token
           .  (Monad m, Show token)
           => M.Map ItemTagId Color
           -> Point2 SubSpace
           -> [Anchor SubSpace]
           -> [Step SubSpace]
           -> FontMonad m (ShapeTree token SubSpace)
constructAnchorStack colorMap point stack steps =
  do  --let string = ((concat $ imap (\i a -> "(iT " ++ show (a ^. ancItemTagId) ++ " tg " ++ show (a ^. ancTag) ++ " c " ++ show (a ^. ancCornerWinding) ++ " w " ++ show (a ^. ancWinding) ++ ")" ) stack)) ++ "\n" ++
      --             intercalate "\n" (map show steps)
      --text <- fromGlyph <$> paragraph 0.1 0.1 AlignMin AlignMin string :: FontMonad m (CompoundTree SubSpace)
      return . overlap $ cross point 1 8:(imap (anchorRing colorMap point) $ stack) -- ++ [withColor black . translateBy point . translateByXY 4 (-7.5) . scaleBy 15 $ text]


trTag tag = trWhen (tag == 22)

interimPoint start end = Point2 (start ^. pX) (end ^. pY)

crossesHorizontal start end bez =
    let ip = interimPoint start end
        oBez = orderedBezier Horizontal bez
    in  if {- tr ("inRangeBez Vertical ip " ++ show ip ++ " oBez " ++ show oBez) $ -} inRangeBez Horizontal ip oBez
        then (boolToWind $ {- tc ("lessThanBezier Vertical ip  " ++ show ip  ++ " oBez " ++ show oBez) $ -} lessThanBezier Horizontal ip  oBez) +
             (boolToWind $ {- tc ("lessThanBezier Vertical end " ++ show end ++ " oBez " ++ show oBez) $ -} lessThanBezier Horizontal end oBez)
        else 0

crossesVertical start end bez =
    let ip = interimPoint start end
        oBez = orderedBezier Vertical bez
    in  if {- tr ("inRangeBez Horizintal ip " ++ show ip ++ " oBez " ++ show oBez) $ -} inRangeBez Vertical ip oBez
        then (boolToWind $ {- tc ("lessThanBezier Horizontal start " ++ show start ++ "oBez " ++ show oBez) $ -} lessThanBezier Vertical start oBez) +
             (boolToWind $ {- tc ("lessThanBezier Horizontal ip    " ++ show ip    ++ "oBez " ++ show oBez) $ -} lessThanBezier Vertical ip    oBez)
        else 0

class HasSpace t => CanConfine t where
  addToConfineTree :: TreeOrderTable
                   -> Int
                   -> ItemTagId
                   -> ConfineTree (SpaceOf t)
                   -> t
                   -> ConfineTree (SpaceOf t)

boolToWind True  = 0
boolToWind False = 1

, _confineCornerWinding :: WindingNumber -- track whether the top left corner is inside the shape.
, _confineCornerHistory :: [(Int, WindingNumber, Bezier s)]

foldl (\ a b -> uncurry combine . uncurry (nextTo axis) . uncurry (align Vertical (nextAxis axis) $ (a, b))) emptyItem


-------

{-
type family Elem i :: *
data List i = Cons (Elem i) (List i) | Nil

deriving instance (Show (Elem i)) => Show (List i)

data MaybeElem a
newtype IdElem a = IdElem a deriving (Num, Show)

data WrapElem a

data MyWrap a = MyWrap a deriving (Show)

type instance Elem (MaybeElem a) = Maybe a
type instance Elem (IdElem a) = a
type instance Elem (WrapElem a) = MyWrap a

wrapList :: List (IdElem a) -> List (WrapElem a)
wrapList (Cons a rest) = Cons (MyWrap a) (wrapList rest)
wrapList Nil           = Nil

catMaybes :: List (MaybeElem a) -> List (IdElem a)
catMaybes (Cons (Just a) rest) = Cons a (catMaybes rest)
catMaybes (Cons Nothing rest) = catMaybes rest
catMaybes Nil = Nil

both :: List (MaybeElem a) -> List (WrapElem a)
both = wrapList . catMaybes
{-
wrapList2 :: (Elem b~MyWrap t, Elem a~t) => List a -> List b
wrapList2 (Cons a rest) = Cons (MyWrap a) (wrapList2 rest)
wrapList2 Nil           = Nil

catMaybes2 :: (Elem a~Maybe (Elem b)) => List a -> List b
catMaybes2 (Cons (Just a) rest) = Cons a (catMaybes2 rest)
catMaybes2 (Cons Nothing rest) = catMaybes2 rest
catMaybes2 Nil = Nil

both2 :: (Elem b~MyWrap t)=>List a -> List b
both2 = wrapList2 . catMaybes2
-}
--instance Show a => Show (MyWrap (IdElem a)) where
--    show  =
--main = putStrLn . show . both $ ( Cons (Just 1) . Cons Nothing . Cons (Just 2) . Cons (Just 3) $ Nil :: List (MaybeElem (IdElem Int)))
-}

myTestTree = STree (SLeaf (SItem (Just 1))) (STree (SLeaf (STag "left" (SLeaf (SItem (Just 2))))) (SLeaf (STag "right" (SLeaf (SItem Nothing) ))))
