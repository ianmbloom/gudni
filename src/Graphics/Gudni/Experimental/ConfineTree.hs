{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Graphics.Gudni.Experimental.ConfineTree
  ( ConfineTree (..)
  , Confine(..)
  , confineItemTagId
  , confineCurveTag
  , confineCurve
  , confineCut
  , confineTraceCross
  , confineCrossings
  , confineOverhang
  , confineLessCut
  , confineMoreCut
  , pointFromAxis
  , orderedBezier
  , Branch(..)
  , maxBoundaries
  , breakPixel
  , pointWinding
  , prepareOutline
  , addBezierToConfineTree
  , crossConfineTree
  , traverseCrossings
  , interimPoint
  , TraceItem(..)
  , traceTag
  , traceTreeTag
  , traceItemTagId
  , traceBox
  , traceAxis
  , traceCrosses
  , With(..)
  , onAxis
  , fromAxis
  , curveEndPointsBox
  , LessPoint(..)
  , positiveSlope
  , crossesAlong
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Axis
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Figure.Split
import Graphics.Gudni.Figure.Outline
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Experimental.TreeOrderTable
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Util.Util(breakVector, clamp)
import Graphics.Gudni.Util.Debug
import GHC.Exts
import Control.Lens
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List.Lens
import Data.Kind

import qualified Data.Vector as V
import Text.PrettyPrint.GenericPretty

newtype WindingNumber = Winding {unWind :: Int} deriving (Eq, Ord, Num, Integral, Real, Enum, Generic)

instance Show WindingNumber where
  show (Winding x) = show x
instance Out WindingNumber

newtype Depth = Depth {unDepth :: Int} deriving (Eq, Ord, Num, Integral, Real, Enum, Generic)

instance Show Depth where
  show (Depth x) = show x
instance Out Depth

data TraceItem s
     = TraceItem
     { _traceTag        :: Int
     , _traceTreeTag    :: Int
     , _traceItemTagId  :: ItemTagId
     , _traceBox        :: Box s
     , _traceAxis       :: EitherAxis
     , _traceCrosses    :: Bool
     }
     deriving (Show, Generic)
makeLenses ''TraceItem

instance Out s => Out (TraceItem s)

newtype With axis s = With {unAxis :: s} deriving (Generic, Show, Eq, Ord)

onAxis :: axis -> s -> With axis s
onAxis axis s = With s

instance (Out s) => Out (With axis s)

fromAxis :: axis -> With axis s -> s
fromAxis axis (With s) = s
data Confine axis s
     = Confine
     { _confineItemTagId :: ItemTagId
     , _confineCrossings :: [ItemTagId]
     , _confineTraceCross:: [TraceItem s]
     , _confineCurveTag  :: Int
     , _confineCurve     :: Bezier s
     , _confineCut       :: With axis s
     , _confineOverhang  :: With axis s
     , _confineLessCut   :: Maybe (Confine (NextAxis axis) s)
     , _confineMoreCut   :: Maybe (Confine (NextAxis axis) s)
     }
     deriving (Show, Generic)
makeLenses ''Confine

instance (Out s) => Out (Confine axis s)

type Branch axis s = Maybe (Confine axis s)
type ConfineTree s = Branch Vertical s

maxBoundaries :: Space s => Box s
maxBoundaries = Box (Point2 (-maxBound) (-maxBound)) (Point2 maxBound maxBound)

curveEndPointsBox :: Space s => Bezier s -> Box s
curveEndPointsBox (Bez v0 _ v1) = minMaxBox (boxOf v0) (boxOf v1)

curveOnAxis :: (Eq s, Axis axis) => axis -> Bezier s -> Bool
curveOnAxis axis bez = bez ^. bzStart . athwart axis == bez ^. bzEnd . athwart axis

prepareOutline :: (Space s) => Outline s -> V.Vector (Bezier s)
prepareOutline =
    join .
    fmap (replaceKnob horizontalAxis) .
    join .
    fmap (replaceKnob verticalAxis) .
    view outlineSegments .
    windClockwise -- actually just the beziers need to be reversed, the vector order shouldn't matter.

-- Order the curve so that the start point is less then or equal to the end point on axis
orderedBezier :: (Ord s, Axis axis) => axis -> Bezier s -> Bezier s
orderedBezier axis bez = if bez ^. bzStart . athwart axis <= bez ^. bzEnd . athwart axis then bez else reverseBezier bez

class (LessPoint (NextAxis axis)) => LessPoint axis where
    lessPoint :: axis -> Bool
    comp :: Space s => axis -> s -> s -> Bool

instance LessPoint Horizontal where
    lessPoint Horizontal = True
    comp Horizontal = (>=)

instance LessPoint Vertical where
    lessPoint Vertical = False
    comp Vertical = (>)

limit :: (Space s) => s
limit = 0.125 / 8

positiveSlope :: (Axis axis, Ord s) => axis -> Bezier s -> Bool
positiveSlope axis bez = bez ^. bzStart . along axis <= bez ^. bzEnd . along axis

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
  in  ((alo > 0) /= (ath > 0)) || (ath == 0 && alo /= 0)

crossesAlong :: (Axis axis, Space s) => axis -> s -> s -> s -> Bezier s -> Bool
crossesAlong axis baseline start end bez =
  if start == end
  then False
  else
  if start > end
  then crossesAlong axis baseline end start bez
  else go bez
  where
  go bez =
    let minAthwart = bezAthwart axis min bez
        maxAthwart = bezAthwart axis max bez
        minAlong   = bezAlong   axis min bez
        maxAlong   = bezAlong   axis max bez
        (lessBez, moreBez) = splitBezier 0.5 bez
    in  if baseline <  minAthwart ||
           baseline >= maxAthwart ||
           start >= maxAlong ||
           end   <  minAlong
        then -- segment is totally outside the range of curve
             False
        else if (maxAlong - minAlong > limit && -- curve size remains greater than the limit
                 baseline /= minAthwart &&      -- and the segment isn't right on the baseline
                 (start >= minAlong || end < maxAlong) -- and the start or end points are somewhere inside curve limits
                )
                || isKnob (along axis) bez -- or the curve creates a knob, meaning there could be more than one cross point
             then -- must split
                  go lessBez /= go moreBez
             else if start < minAlong && end >= maxAlong
                  then True
                  else
                  if bezierSlopeLTEZero axis bez
                  then (start < maxAlong && end >= maxAlong)
                  else -- we know that end >= minAlong and since start < end
                       -- then if end == minAlong then start < minAlong
                       -- else if end > minAlong then
                       ( start < minAlong                     &&      isVertical axis ) ||
                       ((start < maxAlong && end >= maxAlong) && not (isVertical axis))
                       --((start <= minAlong && end > minAlong) && not (isVertical axis))


interimPoint :: Point2 s -> Point2 s -> Point2 s
interimPoint start end = Point2 (start ^. pX) (end ^. pY)

crosses :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crosses start end bez =
    let iP = interimPoint start end
    in
    crossesAlong Vertical   (start ^. pX) (start ^. pY) (iP  ^. pY) bez /=
    crossesAlong Horizontal (iP    ^. pY) (iP    ^. pX) (end ^. pX) bez

createTraceItem :: (SwitchAxis axis, Axis axis, LessPoint axis, Space s)
                => axis
                -> Int
                -> Int
                -> ItemTagId
                -> Box s
                -> With           axis  s
                -> With (NextAxis axis) s
                -> With (NextAxis axis) s
                -> Bezier s
                -> TraceItem s
createTraceItem axis tag treeTag itemTagId box baseline start end bez =
    TraceItem
    { _traceTag        = tag
    , _traceTreeTag    = treeTag
    , _traceItemTagId  = itemTagId
    , _traceBox        = box
    , _traceAxis       = eitherAxis axis
    , _traceCrosses    = crossesAlong axis (unAxis baseline) (unAxis start) (unAxis end) bez
    }

addBezierToConfineTree :: forall s . (Space s)
                       => ItemTagId
                       -> Int
                       -> Bezier s
                       -> ConfineTree s
                       -> ConfineTree s
addBezierToConfineTree itemTagId tag bez =
    --tc ("addBezierToConfineTree itemTagId " ++ show itemTagId ++ " tag " ++ show tag ++ " bez " ++ show bez) .
    goInsert Vertical
    where
    box :: Box s
    box = curveEndPointsBox bez -- we use curveEndPointsBox because there are some situations where the control point
                                -- is slightly outside the range of the endpoints even though the knobs have been removed.
    goInsert :: (Axis axis) => axis -> Maybe (Confine axis s) -> Maybe (Confine axis s)
    goInsert axis mTree =
        case mTree of
          Nothing ->
              let cut = box ^. minBox . athwart axis
              in  Just $
                  Confine
                    { _confineItemTagId  = itemTagId
                    , _confineCrossings  = []
                    , _confineTraceCross = []
                    , _confineCurveTag   = tag
                    , _confineCurve      = bez
                    , _confineCut        = onAxis axis cut
                    , _confineOverhang   = onAxis axis cut
                    , _confineLessCut    = Nothing
                    , _confineMoreCut    = Nothing
                    }
          Just tree ->
              let oldTag = tree ^. confineCurveTag
                  setLess = lessCut axis (box ^. minBox) tree
                            (\t -> over confineOverhang (max (onAxis axis (box ^. maxBox . athwart axis))) .
                                   set confineLessCut (goInsert (nextAxis axis) t))
                  setMore = moreCut axis (box ^. minBox) tree
                            (set confineMoreCut . goInsert (nextAxis axis))
              in  Just . setLess . setMore $ tree

crossConfineTree :: forall s
                 .  (Space s)
                 => ConfineTree s
                 -> ConfineTree s
crossConfineTree mTop = go Vertical mTop mTop
    where
    go :: (Axis axis) => axis -> Maybe (Confine axis s) -> ConfineTree s -> ConfineTree s
    go axis mTree =
        case mTree of
            Nothing -> id
            Just tree ->
                let less = go (nextAxis axis) (tree ^. confineLessCut)
                    more = go (nextAxis axis) (tree ^. confineMoreCut)
                    this = addCrossingToConfineTree (tree ^. confineItemTagId) (tree ^. confineCurveTag) (tree ^. confineCurve)
                in  less . more . this

pointFromAxis :: (Axis axis, Space s) => axis -> With axis s -> With (NextAxis axis) s -> Point2 s
pointFromAxis axis parentLine parentCut =
    set (athwart axis)  (fromAxis axis            parentLine) .
    set (along axis) (fromAxis (nextAxis axis) parentCut) $
    zeroPoint

addCrossingToConfineTree :: forall s
                         .  (Space s)
                         => ItemTagId
                         -> Int
                         -> Bezier s
                         -> ConfineTree s
                         -> ConfineTree s
addCrossingToConfineTree itemTagId tag bez =
    tc ("addCrossingToConfineTree " ++ show tag) .
    goCrossing Vertical (onAxis Vertical minBound) (onAxis Horizontal minBound)
    where
    box :: Box s
    box = curveEndPointsBox bez -- we use curveEndPointsBox because there are some situations where the control point
                                -- is slightly outside the range of the endpoints even though the knobs have been removed.
    goCrossing :: forall axis
               .  (Axis axis, SwitchAxis axis, SwitchAxis (NextAxis axis), LessPoint axis, axis~NextAxis(NextAxis axis))
               => axis
               -> With axis s
               -> With (NextAxis axis) s
               -> Maybe (Confine axis s)
               -> Maybe (Confine axis s)
    goCrossing axis parentCut parentLine mTree =
        case mTree of
            Nothing -> Nothing
            Just tree ->
                let cut :: With axis s
                    cut = tree ^. confineCut
                    goNext :: Maybe (Confine (NextAxis axis) s) -> Maybe (Confine (NextAxis axis) s)
                    goNext = goCrossing (nextAxis axis) parentLine (tree ^. confineCut)
                    addNext :: Maybe (Confine (NextAxis axis) s) -> Maybe (Confine (NextAxis axis) s)
                    addNext = addCrossing (nextAxis axis) tag parentLine (tree ^. confineCut)
                    addOver :: Lens' (Confine axis s) (Maybe (Confine (NextAxis axis) s)) -> Confine axis s -> Confine axis s
                    addOver side t = set side (addNext (t ^. side)) t
                in  Just .
                    lessCut axis (box ^. minBox) tree (set confineLessCut . goNext) .
                    moreCut axis (box ^. maxBox) tree (set confineMoreCut . goNext) . -- this is maxBox not minBox
                    addOver confineLessCut .
                    addOver confineMoreCut $
                    tree
    addCrossing :: forall axis
                .  (Axis axis, SwitchAxis (NextAxis axis), LessPoint axis, axis~NextAxis(NextAxis (axis)))
                => axis
                -> Int
                -> With axis s
                -> With (NextAxis axis) s
                -> Maybe (Confine axis s)
                -> Maybe (Confine axis s)
    addCrossing axis tag parentCut parentLine mTree =
        case mTree of
            Nothing -> Nothing
            Just tree ->
                Just $
                tc ("-------- addCrossing from tag: " ++ show tag ++ " to: " ++ show (tree ^. confineCurveTag)) $
                let stopCut = tree ^. confineCut
                    start = tr "start" $ parentCut
                    end   = tr "end"   $ stopCut
                in
                over confineTraceCross (createTraceItem (nextAxis axis) tag (tree ^. confineCurveTag) itemTagId box parentLine start end bez:) $
                if crossesAlong (nextAxis axis) (unAxis parentLine) (unAxis start) (unAxis end) bez
                then over confineCrossings (toggleCrossing itemTagId) tree
                else tree

eitherConfine :: Lens' (Confine Vertical s) x -> Lens' (Confine Horizontal s) x -> Either (Confine Vertical s) (Confine Horizontal s) -> x
eitherConfine a b (Left  confine) = confine ^. a
eitherConfine a b (Right confine) = confine ^. b

lessCut :: (Axis axis, Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
lessCut axis point tree goNext =
    if onAxis axis (point ^. athwart axis) < tree ^. confineCut
    then goNext (tree ^. confineLessCut)
    else id

moreCut :: (Axis axis, Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
moreCut axis point tree goNext =
    if onAxis axis (point ^. athwart axis) >= tree ^. confineCut
    then goNext (tree ^. confineMoreCut)
    else id

lessOverhang :: (Axis axis, Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
lessOverhang axis point tree goNext =
    if onAxis axis (point ^. athwart axis) < tree ^. confineOverhang
    then goNext (tree ^. confineLessCut)
    else id

toggleCrossing :: ItemTagId -> [ItemTagId] -> [ItemTagId]
toggleCrossing itemTagId (x:xs) | itemTagId <  x = itemTagId:x:xs
                                | itemTagId == x = xs
                                | itemTagId >  x = x:toggleCrossing itemTagId xs
toggleCrossing itemTagId [] = [itemTagId]

crossCurve :: (Space s)
           => Point2 s
           -> Point2 s
           -> [ItemTagId]
           -> (ItemTagId, Bezier s)
           -> [ItemTagId]
crossCurve anchor point stack (itemTagId, bez) =
    if crosses anchor point bez
    then toggleCrossing itemTagId stack
    else stack

firstTwo (a, b, c) = (a, b)
lastOf3  (_, _, c) = c

pointWinding :: forall s . (Space s) => ConfineTree s -> Point2 s -> ([ItemTagId],[Int])
pointWinding mTree point =
    let (anchor, anchorStack) = collectAnchorStack mTree point
        curves = findCurves mTree anchor point
    in  (foldl (crossCurve anchor point) anchorStack $ map firstTwo curves, map lastOf3 curves)


traverseCrossings :: [ItemTagId] -> [ItemTagId] -> [ItemTagId]
traverseCrossings = flip (foldl (flip toggleCrossing))

collectAnchorStack :: forall s . (Space s) => ConfineTree s -> Point2 s -> (Point2 s, [ItemTagId])
collectAnchorStack mTree point =
    go Vertical (onAxis Vertical minBound) (onAxis Horizontal minBound) [] mTree
    where
    go :: (Axis axis, axis~NextAxis(NextAxis axis))
       => axis
       -> With axis s
       -> With (NextAxis axis) s
       -> [ItemTagId]
       -> Branch axis s
       -> (Point2 s, [ItemTagId])
    go axis parentCut parentLine layers mTree =
        case mTree of
            Nothing -> let anchor = pointFromAxis axis parentCut parentLine
                       in  (anchor, layers)
            Just tree ->
                let layers' = traverseCrossings (tree ^. confineCrossings) layers
                    goNext  = go (nextAxis axis) parentLine (tree ^. confineCut) layers'
                in
                if onAxis axis (point ^. athwart axis) < tree ^. confineCut
                then goNext (tree ^. confineLessCut)
                else goNext (tree ^. confineMoreCut)

curveOverlaps :: forall s axis
              .  (Space s)
              => Box s
              -> Confine axis s
              -> [(ItemTagId, Bezier s, Int)]
              -> [(ItemTagId, Bezier s, Int)]
curveOverlaps anchorBox tree =
  let itemTagId = tree ^. confineItemTagId
      bez = tree ^. confineCurve
      bezBox = curveEndPointsBox bez
  in
  if bezBox ^. maxBox . pX >= anchorBox ^. minBox . pX &&
     bezBox ^. maxBox . pY >= anchorBox ^. minBox . pY
  then ((itemTagId, bez, tree ^. confineCurveTag):)
  else id

findCurves :: forall s
           .  (Space s)
           => ConfineTree s
           -> Point2 s
           -> Point2 s
           -> [(ItemTagId, Bezier s, Int)]
findCurves mTree pointFromAxis point =
    go Vertical mTree []
    where
    box :: Box s
    box = minMaxBox (pointToBox pointFromAxis) (pointToBox point)
    go :: (Axis axis) => axis -> Branch axis s -> [(ItemTagId, Bezier s, Int)] -> [(ItemTagId, Bezier s, Int)]
    go axis mTree =
      case mTree of
        Nothing -> id
        Just tree ->
            moreCut      axis         point tree (go (nextAxis axis)) .
            lessOverhang axis pointFromAxis tree (go (nextAxis axis)) .
            curveOverlaps box tree

class Breaker (NextAxis axis) => Breaker axis where
    lessBreak :: (Space s) => axis -> Box s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a

instance Breaker Vertical where
    lessBreak axis box tree goNext =
        if box ^. minBox . athwart axis < unAxis (tree ^. confineOverhang)
        then goNext (tree ^. confineLessCut)
        else id

instance Breaker Horizontal where
    lessBreak axis point tree goNext=
        goNext (tree ^. confineLessCut)

moreBreak :: (Axis axis, Space s) => axis -> Box s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
moreBreak axis box tree goNext =
    if box ^. maxBox . athwart axis >= unAxis (tree ^. confineCut)
    then goNext (tree ^. confineMoreCut)
    else id

breakPixel :: forall s . (Space s) => ConfineTree s -> Box s -> [(Box s, [ItemTagId])]
breakPixel mTree box =
    go Vertical mTree box []
    where
    go :: (Axis axis, Breaker axis) => axis -> Branch axis s -> Box s -> [ItemTagId] -> [(Box s, [ItemTagId])]
    go axis mTree box layers =
       case mTree of
           Nothing   -> [(box, layers)]
           Just tree ->
               let crossed = traverseCrossings (tree ^. confineCrossings) layers
               in  undefined
