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
  , Layer(..)
  , layItemTagId
  , layWinding
  , layTags
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
     , _confinePathTo    :: [ItemTagId]
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
curveOnAxis axis bez = bez ^. bzStart . pick axis == bez ^. bzEnd . pick axis

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
orderedBezier axis bez = if bez ^. bzStart . pick axis <= bez ^. bzEnd . pick axis then bez else reverseBezier bez

-- This assumes the bezier is ordered on axis
sizeOnAxis :: (Space s, Axis axis) => axis -> Bezier s -> s
sizeOnAxis axis bez = bez ^. bzEnd . pick axis - bez ^. bzStart . pick axis

aboveRangeMin :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
aboveRangeMin axis p bez = p ^. pick axis >= bez ^. bzStart . pick axis

belowRangeMin :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
belowRangeMin axis p = not . aboveRangeMin axis p

belowRangeMax :: (Space s, Axis axis) => axis -> Point2 s -> Bezier s -> Bool
belowRangeMax axis p bez = p ^. pick axis < bez ^. bzEnd . pick axis

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
    if -- tr "p ^. along axis <= minAxis" $
           p ^. along axis <= minAxis
    then True
    else if --tr "p ^. along axis > maxAxis || sizeOnAxis axis bez <= iota" $
                p ^. along axis > maxAxis || sizeOnAxis axis bez <= iota
         then False
         else let (less, more) = splitBezier 0.5 bez
              in
              if p ^. pick axis <= less ^. bzEnd . pick axis
              then go -- $ tr "less" $
                            less
              else go -- $ tr "more" $
                            more

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
    outsidePoint = Point2 0 minBound
    box = curveEndPointsBox bez
    minPoint = box ^. minBox
    maxPoint = box ^. maxBox
    go :: (Axis axis) => axis -> Point2 s -> Maybe (Confine axis s) -> Maybe (Confine axis s)
    go axis mTree =
        case mTree of
          Nothing ->
              let cut = minPoint ^. pick axis
              in  Just $
                  Confine
                    { _confineItemTagId = itemTagId
                    , _confineCurveTag  = tag
                    , _confineCurve     = bez
                    , _confineCut       = cut
                    , _confineOverhang  = cut
                    , _confineLessCut   = Nothing
                    , _confineMoreCut   = Nothing
                    }
          Just tree ->
              let cut = tree ^. confineCut
                  setLess = if minPoint ^. pick axis < cut
                            then let lessBranch = go (nextAxis axis) (tree ^. confineLessCut)
                                 in  over confineOverhang (max (maxPoint ^. pick axis)) .
                                     set confineLessCut lessBranch
                            else id
                  setMore = if minPoint ^. pick axis >= cut
                            then let moreBranch = go (nextAxis axis) (tree ^. confineMoreCut)
                                 in  set confineMoreCut moreBranch
                            else id
              in  Just . setLess . setMore $ tree

eitherConfine :: Lens' (Confine Vertical s) x -> Lens' (Confine Horizontal s) x -> Either (Confine Vertical s) (Confine Horizontal s) -> x
eitherConfine a b (Left  confine) = confine ^. a
eitherConfine a b (Right confine) = confine ^. b

data Layer = Layer
   { _layItemTagId :: ItemTagId
   , _layWinding   :: WindingNumber
   , _layTags      :: [Int]
   }
makeLenses ''Layer

splitBoxAcrossPoint :: (Space s, Axis axis) => axis -> Box s -> Point2 s -> ([Box s], [Box s])
splitBoxAcrossPoint axis box point =
   if box ^. maxBox . pick axis <= point ^. pick axis
   then ([box], [])
   else if box ^. minBox . pick axis >= point ^. pick axis
   then ([], [box])
   else let (less, more) = splitBox axis (point ^. pick axis) box
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

overPixelStack :: ([Layer] -> [Layer]) -> (Box s, [Layer]) -> (Box s, [Layer])
overPixelStack f (pixel, stack) = (pixel, f stack)

class Switcher (NextAxis axis) => Switcher axis where
   lessSwitch :: (Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
   moreSwitch :: (Space s) => axis -> Point2 s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
   overCurve :: (Space s) => axis  -> Point2 s -> Confine axis s -> (a -> a) -> a -> a

instance Switcher Vertical where
   lessSwitch Vertical point tree goNext =
       if point ^. pX < tree ^. confineOverhang
       then goNext (tree ^. confineLessCut)
       else id
   moreSwitch Vertical point tree goNext =
       if point ^. pX >= tree ^. confineCut
       then goNext (tree ^. confineMoreCut)
       else id
   overCurve Vertical point tree f =
       let oBez      = orderedBezier Vertical (tree ^. confineCurve)
       in
       if inRangeBez Vertical point oBez
       then if lessThanBezier Vertical point oBez
            then id
            else f
       else id

instance Switcher Horizontal where
    lessSwitch Horizontal point tree goNext=
        goNext (tree ^. confineLessCut)
    moreSwitch Horizontal point tree goNext =
        if point ^. pY >= tree ^. confineCut
        then goNext (tree ^. confineMoreCut)
        else id
    overCurve Horizontal point tree f =
        let oBez      = orderedBezier Vertical (tree ^. confineCurve)
        in
        if   point ^. pY >= tree ^. confineCut &&
             inRangeBez Vertical point oBez
        then if lessThanBezier Vertical point oBez
             then id
             else f
        else id


breakWithCurve :: (Space s) => Confine axis s -> Box s -> [(Box s, Int)]
breakWithCurve tree box =
  go box (orderedBezier Vertical $ tree ^.  confineCurve)
  where
  go :: (Space s) => Box s -> Bezier s -> [(Box s, Int)]
  go box bez =
      let minAxis = min (bez ^. bzStart . along Vertical) (bez ^. bzEnd . along Vertical)
          maxAxis = max (bez ^. bzStart . along Vertical) (bez ^. bzEnd . along Vertical)
      in  if box ^. minBox . along Vertical >= maxAxis
          then [(box, 1)]
          else if box ^. maxBox . along Vertical <= minAxis
               then [(box, 0)]
               else let (lessBez, moreBez) = splitBezier 0.5 bez
                        center = lessBez ^. bzEnd
                        (lessBox, moreBox) = splitBox Vertical (center ^. pick Vertical) box
                    in
                    if box ^. maxBox . pick Vertical <= center ^. pick Vertical
                    then go box lessBez
                    else if box ^. minBox . pick Vertical >= center ^. pick Vertical
                         then go box moreBez
                         else go lessBox lessBez ++ go moreBox moreBez


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

pointWinding :: forall s . (Space s) => ConfineTree s -> Point2 s -> [Layer]
pointWinding mTree point =
    go Vertical mTree []
    where
    go :: (Axis axis, Switcher axis) => axis -> Branch axis s -> [Layer] -> [Layer]
    go axis mTree  =
      case mTree of
        Nothing -> id
        Just tree ->
          lessSwitch axis point tree (go (nextAxis axis)) .
          moreSwitch axis point tree (go (nextAxis axis)) .
          overCurve  axis point tree (overLayer (+1) tree)
