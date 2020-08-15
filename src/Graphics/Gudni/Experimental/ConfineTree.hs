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
  , confineCrossings
  , confineOverhang
  , confineLessCut
  , confineMoreCut
  , confineAnchorPoint
  , Branch(..)
  , maxBoundaries
  , inRangeBez
  , breakPixel
  , pointWinding
  , prepareOutline
  , addBezierToConfineTree
  , traverseCrossings
  , interimPoint
  , outsidePoint
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Axis
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Cut
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

data Confine axis s
     = Confine
     { _confineItemTagId :: ItemTagId
     , _confineCrossings :: [ItemTagId]
     , _confineCurveTag  :: Int
     , _confineCurve     :: Bezier s
     , _confineCut       :: s
     , _confineOverhang  :: s
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

confineAnchorPoint :: Space s => Confine t s -> Point2 s
confineAnchorPoint tree = (curveEndPointsBox $ tree ^. confineCurve) ^. minBox

curveOnAxis :: (Eq s, Axis axis) => axis -> Bezier s -> Bool
curveOnAxis axis bez = bez ^. bzStart . with axis == bez ^. bzEnd . with axis

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
orderedBezier axis bez = if bez ^. bzStart . with axis <= bez ^. bzEnd . with axis then bez else reverseBezier bez

-- This assumes the bezier is ordered on axis
sizeOnAxis :: (Space s, Axis axis) => axis -> Bezier s -> s
sizeOnAxis axis bez = bez ^. bzEnd . with axis - bez ^. bzStart . with axis

aboveRangeMin :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
aboveRangeMin axis p bez = p ^. with axis >= bez ^. bzStart . with axis

belowRangeMin :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
belowRangeMin axis p = not . aboveRangeMin axis p

belowRangeMax :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
belowRangeMax axis p bez = p ^. with axis < bez ^. bzEnd . with axis

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
         else let (less, more) = splitBezier 0.5 bez
              in
              if p ^. with axis <= less ^. bzEnd . with axis
              then go less
              else go more

interimPoint :: Point2 s -> Point2 s -> Point2 s
interimPoint start end = Point2 (start ^. pX) (end ^. pY)

-- Using /= for xor

crossesHorizontal :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crossesHorizontal start end bez =
    let ip = interimPoint start end
        oBez = orderedBezier Horizontal bez
    in  inRangeBez Horizontal ip oBez &&
        (lessThanBezier Horizontal ip  oBez /=
         lessThanBezier Horizontal end oBez)

crossesVertical :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crossesVertical start end bez =
    let ip = interimPoint start end
        oBez = orderedBezier Vertical bez
    in  inRangeBez Vertical ip oBez &&
        (lessThanBezier Vertical start oBez /=
         lessThanBezier Vertical ip    oBez)

crosses :: (Space s) => Point2 s -> Point2 s -> Bezier s -> Bool
crosses start end bez =
    crossesHorizontal start end bez /= crossesVertical start end bez

outsidePoint :: Space s => Point2 s
outsidePoint = Point2 0 minBound -- this point can be any arbitrary point outside the range of any shape.

addBezierToConfineTree :: forall s . (Space s)
                       => ItemTagId
                       -> Int
                       -> Bezier s
                       -> ConfineTree s
                       -> ConfineTree s
addBezierToConfineTree itemTagId tag bez =
    --tc ("addBezierToConfineTree itemTagId " ++ show itemTagId ++ " tag " ++ show tag ++ " bez " ++ show bez) .
    go Vertical outsidePoint
    where
    box = curveEndPointsBox bez -- we use curveEndPointsBox because there are some situations where the control point
                                -- is slightly outside the range of the endpoints even though the knobs have been removed.
    go :: forall axis . (Axis axis) => axis -> Point2 s -> Maybe (Confine axis s) -> Maybe (Confine axis s)
    go axis start mTree =
        case mTree of
          Nothing ->
              let cut = box ^. minBox . with axis
              in  Just .
                  addCrossing start $
                  Confine
                    { _confineItemTagId = itemTagId
                    , _confineCrossings = []
                    , _confineCurveTag  = tag
                    , _confineCurve     = bez
                    , _confineCut       = cut
                    , _confineOverhang  = cut
                    , _confineLessCut   = Nothing
                    , _confineMoreCut   = Nothing
                    }
          Just tree ->
              let cut = tree ^. confineCut
                  anchor = confineAnchorPoint tree
                  setLess = if box ^. minBox . with axis < cut
                            then let lessBranch = go (nextAxis axis) anchor (tree ^. confineLessCut)
                                 in  over confineOverhang (max (box ^. maxBox . with axis)) .
                                     set confineLessCut lessBranch
                            else id
                  setMore = if box ^. minBox . with axis >= cut
                            then let moreBranch = go (nextAxis axis) anchor (tree ^. confineMoreCut)
                                 in  set confineMoreCut moreBranch
                            else id
              in  Just . setLess . setMore . addCrossing start $ tree
    addCrossing :: Point2 s -> Confine axis s -> Confine axis s
    addCrossing start tree =
      if crosses start (confineAnchorPoint tree) bez
      then over confineCrossings (toggleCrossing itemTagId) tree
      else tree


eitherConfine :: Lens' (Confine Vertical s) x -> Lens' (Confine Horizontal s) x -> Either (Confine Vertical s) (Confine Horizontal s) -> x
eitherConfine a b (Left  confine) = confine ^. a
eitherConfine a b (Right confine) = confine ^. b

lessCut :: (Axis axis, Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
lessCut axis point tree goNext =
    if point ^. with axis < tree ^. confineCut
    then goNext (tree ^. confineLessCut)
    else id

moreCut :: (Axis axis, Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
moreCut axis point tree goNext =
    if point ^. with axis >= tree ^. confineCut
    then goNext (tree ^. confineMoreCut)
    else id

lessOverhang :: (Axis axis, Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
lessOverhang axis point tree goNext =
    if point ^. with axis < tree ^. confineOverhang
    then goNext (tree ^. confineLessCut)
    else id


overCurve :: (Axis axis, Space s) => axis -> Point2 s -> Confine axis s -> (a -> a) -> a -> a
overCurve axis point tree f =
    let oBez = orderedBezier Vertical (tree ^. confineCurve)
    in
    if point ^. with axis >= tree ^. confineCut &&
       inRangeBez Vertical point oBez
    then if lessThanBezier Vertical point oBez
         then id
         else f
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

pointWinding :: forall s . (Space s) => ConfineTree s -> Point2 s -> [ItemTagId]
pointWinding mTree point =
    let anchor = findAnchorPoint mTree point
        anchorStack = collectAnchorStack mTree point
        curves = findCurves mTree anchor point
    in  foldl (crossCurve anchor point) anchorStack curves

findAnchorPoint :: forall s
                . (Space s)
                => ConfineTree s
                -> Point2 s
                -> Point2 s
findAnchorPoint mTree point = go Vertical mTree outsidePoint
    where
    go :: (Axis axis) => axis -> Branch axis s -> Point2 s -> Point2 s
    go axis mTree lastAnchor =
      case mTree of
        Nothing -> lastAnchor
        Just tree ->
           let newAnchor = confineAnchorPoint tree
           in
           if point ^. with axis >= tree ^. confineCut
           then go (nextAxis axis) (tree ^. confineMoreCut) newAnchor
           else go (nextAxis axis) (tree ^. confineLessCut) newAnchor

traverseCrossings :: [ItemTagId] -> [ItemTagId] -> [ItemTagId]
traverseCrossings = flip (foldl (flip toggleCrossing))

collectAnchorStack :: forall s . (Space s) => ConfineTree s -> Point2 s -> [ItemTagId]
collectAnchorStack mTree point =
    go Vertical mTree []
    where
    go :: (Axis axis) => axis -> Branch axis s -> [ItemTagId] -> [ItemTagId]
    go axis mTree  =
        case mTree of
            Nothing -> id
            Just tree ->
                lessCut axis point tree (go (nextAxis axis)) .
                moreCut axis point tree (go (nextAxis axis)) .
                traverseCrossings (tree ^. confineCrossings)

curveOverlaps :: forall s axis
              .  (Space s)
              => Box s
              -> Confine axis s
              -> [(ItemTagId, Bezier s)]
              -> [(ItemTagId, Bezier s)]
curveOverlaps anchorBox tree =
  let itemTagId = tree ^. confineItemTagId
      bez = tree ^. confineCurve
      bezBox = curveEndPointsBox bez
  in
  if bezBox ^. maxBox . pX >= anchorBox ^. minBox . pX &&
     bezBox ^. maxBox . pY >= anchorBox ^. minBox . pY
  then ((itemTagId, bez):)
  else id

findCurves :: forall s
           .  (Space s)
           => ConfineTree s
           -> Point2 s
           -> Point2 s
           -> [(ItemTagId, Bezier s)]
findCurves mTree anchorPoint point =
    go Vertical mTree []
    where
    box = minMaxBox (pointToBox anchorPoint) (pointToBox point)
    go :: (Axis axis) => axis -> Branch axis s -> [(ItemTagId, Bezier s)] -> [(ItemTagId, Bezier s)]
    go axis mTree =
      case mTree of
        Nothing -> id
        Just tree ->
            moreCut      axis       point tree (go (nextAxis axis)) .
            lessOverhang axis anchorPoint tree (go (nextAxis axis)) .
            curveOverlaps box tree

class Breaker (NextAxis axis) => Breaker axis where
    lessBreak :: (Space s) => axis -> Box s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a

instance Breaker Vertical where
    lessBreak axis box tree goNext =
        if box ^. minBox . with axis < tree ^. confineOverhang
        then goNext (tree ^. confineLessCut)
        else id

instance Breaker Horizontal where
    lessBreak axis point tree goNext=
        goNext (tree ^. confineLessCut)

moreBreak :: (Axis axis, Space s) => axis -> Box s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
moreBreak axis box tree goNext =
    if box ^. maxBox . with axis >= tree ^. confineCut
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
