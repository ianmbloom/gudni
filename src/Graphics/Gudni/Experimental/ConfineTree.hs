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
{-# LANGUAGE StandaloneDeriving         #-}

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
  , pointFromAxis
  , Branch(..)
  , pointWinding
  , prepareOutline
  , addBezierToConfineTree
  , crossConfineTree
  , traverseCrossings
  , With(..)
  , onAxis
  , fromAxis

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
import Graphics.Gudni.Experimental.CrossesBezier
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
import Text.PrettyPrint hiding ((<>))

newtype With axis s = With {unAxis :: s} deriving (Generic, Eq, Ord)

instance (Show s, Axis axis) => Show (With axis s) where
  show (With s) = showSymbol (undefined :: axis) ++ show s

onAxis :: axis -> s -> With axis s
onAxis axis s = With s

instance (Out s, Axis axis, Show s) => Out (With axis s) where
      doc (With x) = text (showSymbol (undefined :: axis)) <> (text . show $ x)
      docPrec _ = doc

fromAxis :: axis -> With axis s -> s
fromAxis axis (With s) = s

data Confine axis s
     = Confine
     { _confineItemTagId :: ItemTagId
     , _confineCrossings :: [ItemTagId]
     , _confineCurveTag  :: Int
     , _confineCurve     :: Bezier s
     , _confineCut       :: With axis s
     , _confineOverhang  :: With axis s
     , _confineLessCut   :: Maybe (Confine (NextAxis axis) s)
     , _confineMoreCut   :: Maybe (Confine (NextAxis axis) s)
     }
     deriving (Generic)
makeLenses ''Confine

deriving instance (Axis axis, Show s) => Show (Confine axis s)

instance (Out s, Axis axis, Show s) => Out (Confine axis s)

type Branch axis s = Maybe (Confine axis s)
type ConfineTree s = Branch Vertical s

prepareOutline :: (Space s) => Outline s -> V.Vector (Bezier s)
prepareOutline =
    join .
    fmap (replaceKnob horizontalAxis) .
    join .
    fmap (replaceKnob verticalAxis) .
    view outlineSegments .
    windClockwise -- actually just the beziers need to be reversed, the vector order shouldn't matter.



addBezierToConfineTree :: forall s . (Space s, Out s)
                       => ItemTagId
                       -> Int
                       -> Bezier s
                       -> ConfineTree s
                       -> ConfineTree s
addBezierToConfineTree itemTagId tag bez =
    --tcP ("addBezierToConfineTree itemTagId " ++ show itemTagId ++ " tag " ++ show tag ++ " bez " ++ show bez) .
    goInsert Vertical
    where
    box :: Box s
    box = boxOf bez
    goInsert :: (Axis axis) => axis -> Maybe (Confine axis s) -> Maybe (Confine axis s)
    goInsert axis mTree =
        case mTree of
          Nothing ->
              let cut = box ^. minBox . athwart axis
              in  Just $
                  Confine
                    { _confineItemTagId  = itemTagId
                    , _confineCrossings  = []
                    , _confineCurveTag   = tag
                    , _confineCurve      = bez
                    , _confineCut        = onAxis axis cut
                    , _confineOverhang   = onAxis axis cut
                    , _confineLessCut    = Nothing
                    , _confineMoreCut    = Nothing
                    }
          Just tree ->
              let oldTag = tree ^. confineCurveTag
                  setLess = lessCut axis (box ^. minBox . athwart axis) tree
                            (\t -> over confineOverhang (max (onAxis axis (box ^. maxBox . athwart axis))) .
                                   set confineLessCut (goInsert (nextAxis axis) t))
                  setMore = moreCut axis (box ^. minBox . athwart axis) tree
                            (set confineMoreCut . goInsert (nextAxis axis))
              in  Just . setLess . setMore $ tree

crossConfineTree :: forall s
                 .  ( Space s
                    , Out s
                    )
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
                         .  (Space s
                            , Out s)
                         => ItemTagId
                         -> Int
                         -> Bezier s
                         -> ConfineTree s
                         -> ConfineTree s
addCrossingToConfineTree itemTagId tag bez mTree =
    case mTree of
        Nothing -> Nothing
        Just tree ->
            --trP ("addCrossingToConfineTree " ++ show tag) .
            Just $ goChildren Vertical True (onAxis Vertical minBound) (onAxis Horizontal minBound) tree
    where
    box = boxOf bez
    goChildren :: forall axis
               .  (Axis axis, SwitchAxis axis, SwitchAxis (NextAxis axis), axis~NextAxis(NextAxis axis))
               => axis
               -> Bool
               -> With axis s
               -> With (NextAxis axis) s
               -> Confine axis s
               -> Confine axis s
    goChildren axis moreSide parentCut parentLine tree =
       let cut :: With axis s
           cut = tree ^. confineCut
           goNext :: Bool -> Maybe (Confine (NextAxis axis) s) -> Maybe (Confine (NextAxis axis) s)
           goNext nextSide = goAdd (nextAxis axis) nextSide parentLine (tree ^. confineCut)
       in  if True
              -- tr "both" $
              -- (tr "    moreSide && (box ^. maxBox . athwart axis >= fromAxis axis parentCut)" $     moreSide && (box ^. maxBox . athwart axis >= fromAxis axis parentCut)) ||
              -- (tr "not moreSide && (box ^. minBox . athwart axis <  fromAxis axis parentCut)" $ not moreSide && (box ^. minBox . athwart axis <  fromAxis axis parentCut))
           then set confineMoreCut (goNext True  (tree ^. confineMoreCut)) .
                set confineLessCut (goNext False (tree ^. confineLessCut)) $
                tree
           else tree

    goAdd :: forall axis
          .  (Axis axis, SwitchAxis axis, SwitchAxis (NextAxis axis), axis~NextAxis(NextAxis axis))
          => axis
          -> Bool
          -> With axis s
          -> With (NextAxis axis) s
          -> Maybe (Confine axis s)
          -> Maybe (Confine axis s)
    goAdd axis moreSide parentCut parentLine mTree =
        case mTree of
            Nothing -> Nothing
            Just tree ->
                Just .
                goChildren axis moreSide parentCut parentLine .
                addCrossing axis parentCut parentLine $
                tree

    addCrossing :: forall axis
                .  (Axis axis, SwitchAxis (NextAxis axis), axis~NextAxis(NextAxis (axis)))
                => axis
                -> With axis s
                -> With (NextAxis axis) s
                -> Confine axis s
                -> Confine axis s
    addCrossing axis parentCut parentLine tree =
        --tcP ("-------- addCrossing from new tag: " ++ show tag ++ " over: " ++ show (tree ^. confineCurveTag)) $
        let stopCut = tree ^. confineCut
            start   = parentCut
            end     = stopCut
        in
        if --tr ("crossesAlong new " ++ show tag ++ " over: " ++ show (tree ^. confineCurveTag) ++ " parentLine "++ show parentLine ++ " start " ++ show start ++ " end " ++ show end) $
           crossesAlong (nextAxis axis) (unAxis parentLine) (unAxis start) (unAxis end) bez
        then over confineCrossings (toggleCrossing itemTagId) tree
        else tree

eitherConfine :: Lens' (Confine Vertical s) x -> Lens' (Confine Horizontal s) x -> Either (Confine Vertical s) (Confine Horizontal s) -> x
eitherConfine a b (Left  confine) = confine ^. a
eitherConfine a b (Right confine) = confine ^. b

lessCut :: (Axis axis, Space s) => axis -> s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
lessCut axis s tree goNext =
    if onAxis axis s < tree ^. confineCut
    then goNext (tree ^. confineLessCut)
    else id

moreCut :: (Axis axis, Space s) => axis -> s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
moreCut axis s tree goNext =
    if onAxis axis s >= tree ^. confineCut
    then goNext (tree ^. confineMoreCut)
    else id

lessOverhang :: (Axis axis, Space s) => axis -> s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
lessOverhang axis point tree goNext =
    if onAxis axis point < tree ^. confineOverhang
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

pointWinding :: forall s . (Space s) => ConfineTree s -> Point2 s -> (Point2 s, [ItemTagId],[Int])
pointWinding mTree point =
    let (anchor, anchorStack) = collectAnchorStack mTree point
        curves = findCurves mTree anchor point
    in  (anchor, foldl (crossCurve anchor point) anchorStack $ map firstTwo curves, map lastOf3 curves)


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
      bezBox = boxOf bez
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
findCurves mTree anchor point =
    go Vertical mTree []
    where
    box :: Box s
    box = boxAroundPoints anchor point
    go :: (Axis axis) => axis -> Branch axis s -> [(ItemTagId, Bezier s, Int)] -> [(ItemTagId, Bezier s, Int)]
    go axis mTree =
      case mTree of
        Nothing -> id
        Just tree ->
            moreCut      axis (box ^. maxBox . athwart axis) tree (go (nextAxis axis)) .
            lessOverhang axis (box ^. minBox . athwart axis) tree (go (nextAxis axis)) .
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
