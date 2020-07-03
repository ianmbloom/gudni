{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Experimental.ConfineTree
  ( ConfineSubstanceId (..)
  , ConfineTree (..)
  , Confine(..)
  , confineSubstance
  , confineCurveTag
  , confineCornerWinding
  , confineCurve
  , confineBranch
  , ConfineV(..)
  , confineXCut
  , confineXOverlap
  , confineLeft
  , confineRight
  , BranchV(..)
  , ConfineH(..)
  , confineYCut
  , confineYOverlap
  , confineTop
  , confineBottom
  , BranchH(..)
  , confineToLeft
  , confineAbove
  , maxBoundaries
  , CanConfine(..)
  , insideBoundaryPoint
  , outsideBoundaryPoint
  , inHorizontalRangeBox
  , inVerticalRangeBox
  , isAboveCurve
  , isLeftOfCurve
  , pointWinding
  , StackFrame(..)
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Figure.Outline
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Experimental.TreeOrderTable
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Util.Util(breakVector, clamp)
import Graphics.Gudni.Util.Debug
import GHC.Exts
import Control.Lens
import Control.Monad
import Data.Maybe
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

type ConfineSubstanceId = Int

type ConfineTree s = BranchV s

data Confine t s =
     Confine
     { _confineSubstance :: ConfineSubstanceId
     , _confineCurveTag  :: Int
     , _confineCornerWinding :: WindingNumber -- track whether the top left corner is inside the shape.
     , _confineCurve     :: Bezier s
     , _confineBranch    :: t s
     } deriving (Show, Generic)
instance (Out s, Out (t s)) => Out (Confine t s)

data ConfineV s =
     ConfineV
     { _confineXCut  :: s
     , _confineXOverlap :: s
     , _confineLeft  :: BranchH s
     , _confineRight :: BranchH s
     } deriving (Show, Generic)
instance (Out s) => Out (ConfineV s)

type BranchV s = Maybe (Confine ConfineV s)

data ConfineH s =
     ConfineH
     { _confineYCut   :: s
     , _confineYOverlap :: s
     , _confineTop    :: BranchV s
     , _confineBottom :: BranchV s
     } deriving (Show, Generic)
instance (Out s) => Out (ConfineH s)

type BranchH s = Maybe (Confine ConfineH s)

makeLenses ''Confine
makeLenses ''ConfineV
makeLenses ''ConfineH

maxBoundaries :: Space s => Box s
maxBoundaries = Box (Point2 (-maxBound) (-maxBound)) (Point2 maxBound maxBound)

curveIsVertical :: Eq s => Bezier s -> Bool
curveIsVertical bez = bez ^. bzStart . pX == bez ^. bzEnd . pX

curveIsHorizontal :: Eq s => Bezier s -> Bool
curveIsHorizontal bez = bez ^. bzStart . pY == bez ^. bzEnd . pY

isAboveCurve :: Space s => Point2 s -> Bezier s ->  Bool
isAboveCurve point bezier =
  if curveIsVertical bezier
  then False
  else let t = findCutBezier Vertical (point ^. pX) bezier
           bPoint = eval t bezier
       in  point ^. pY < bPoint ^. pY

isLeftOfCurve :: Space s => Point2 s -> Bezier s ->  Bool
isLeftOfCurve point bezier =
  if curveIsHorizontal bezier
  then False
  else let t = findCutBezier Horizontal (point ^. pY) bezier
           bPoint = eval t bezier
       in  point ^. pX < bPoint ^. pX


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


prepareOutline :: (Space s) => Outline s -> V.Vector (Bezier s)
prepareOutline =
    join .
    fmap (replaceKnob horizontalAxis) .
    join .
    fmap (replaceKnob verticalAxis) .
    view outlineSegments .
    windClockwise -- actually just the beziers need to be reversed, the vector order shouldn't matter.

class HasSpace t => CanConfine t where
  addToConfineTree :: TreeOrderTable
                   -> Int
                   -> ConfineSubstanceId
                   -> ConfineTree (SpaceOf t)
                   -> t
                   -> ConfineTree (SpaceOf t)

clampBox :: Ord s => Lens' (Point2 s) s -> Box s -> s -> s
clampBox axis box = clamp (box ^. topLeftBox . axis) (box ^. bottomRightBox . axis)

inHorizontalRangeBox :: Space s => Point2 s -> Box s -> Bool
inHorizontalRangeBox p box =
    --tr ("inHorizontalRangeBox p " ++ show p ++ " box " ++ show box) $
    p ^. pX >= box ^. leftSide && p ^. pX < box ^. rightSide

inVerticalRangeBox :: Space s => Point2 s -> Box s -> Bool
inVerticalRangeBox p box =
    --tr ("inVerticalRangeBox p " ++ show p ++ " box " ++ show box) $
    p ^. pY >= box ^. topSide && p ^. pY < box ^. bottomSide


boxAbovePoint :: Space s => Point2 s -> Box s -> Bool
boxAbovePoint p bx =
    inHorizontalRangeBox p bx && p ^. pY >= bx ^. bottomSide

boxLeftOfPoint :: Space s => Point2 s -> Box s -> Bool
boxLeftOfPoint p bx =
    inVerticalRangeBox p bx && p ^. pX >= bx ^. rightSide

addBezierToConfineTree :: forall s . (Space s)
                       => ConfineSubstanceId
                       -> ConfineTree s
                       -> (Bezier s, Int)
                       -> ConfineTree s
addBezierToConfineTree substance mNode (bez, tag) =
    goV mNode
    where
    box = curveBox bez
    goV :: BranchV s -> BranchV s
    goV mNode =
        case mNode of
          Nothing ->
              let xCut = box ^. leftSide
              in  Just .
                  Confine substance tag 0 bez $
                  ConfineV
                    { _confineXCut     = xCut
                    , _confineXOverlap = xCut
                    , _confineLeft     = Nothing
                    , _confineRight    = Nothing
                    }
          Just vTree ->
              let xCut = vTree ^. confineBranch . confineXCut
                  topLeft = curveBox (vTree ^. confineCurve) ^. topLeftBox
                  setLeft  = if box ^. leftSide < xCut
                             then let leftBranch = goH (vTree ^. confineBranch . confineLeft)
                                  in  over (confineBranch . confineXOverlap) (max (box ^. rightSide)) .
                                      set (confineBranch . confineLeft) leftBranch
                             else id
                  setRight = if box ^. leftSide >= xCut
                             then let rightBranch = goH (vTree ^. confineBranch . confineRight)
                                  in  set (confineBranch . confineRight) rightBranch
                             else id
              in  Just . setLeft . setRight $ vTree
    goH :: BranchH s -> BranchH s
    goH mNode =
        case mNode of
          Nothing ->
              let yCut = box ^. topSide
              in  Just .
                  Confine substance tag 0 bez $
                  ConfineH
                     { _confineYCut     = yCut
                     , _confineYOverlap = yCut
                     , _confineTop      = Nothing
                     , _confineBottom   = Nothing
                     }
          Just hTree ->
              let yCut = hTree ^. confineBranch . confineYCut
                  topLeft = curveBox (hTree ^. confineCurve) ^. topLeftBox
                  setTop    = if box ^. topSide < yCut
                              then let topBranch = goV (hTree ^. confineBranch . confineTop)
                                   in  over (confineBranch . confineYOverlap) (max (box ^. bottomSide)) .
                                       set (confineBranch . confineTop) topBranch
                              else id
                  setBottom = if box ^. topSide >= yCut
                              then let bottomBranch = goV (hTree ^. confineBranch . confineBottom)
                                   in  set (confineBranch . confineBottom) bottomBranch
                              else id
              in  Just . setTop . setBottom $ hTree

windConfineTree :: forall s . (Space s)
                => ConfineSubstanceId
                -> ConfineTree s
                -> Bezier s
                -> ConfineTree s
windConfineTree substance mNode bez = goWindV mNode
    where
    box = curveBox bez
    goWindV :: BranchV s -> BranchV s
    goWindV mVNode =
      case mVNode of
        Nothing -> Nothing
        Just vTree ->
            let xCut     = vTree ^. confineBranch . confineXCut
                topLeft  = curveBox (vTree ^. confineCurve) ^. topLeftBox
                setLeft  = if box ^. leftSide  <  xCut then set (confineBranch . confineLeft ) (goWindH (vTree ^. confineBranch . confineLeft )) else id
                setRight = if box ^. rightSide >= xCut then set (confineBranch . confineRight) (goWindH (vTree ^. confineBranch . confineRight)) else id
                setCornerWinding = over confineCornerWinding (+ (boolToWind $ inHorizontalRangeBox topLeft box && verticalBezierWind topLeft box bez))
            in  Just . setLeft . setRight . setCornerWinding $ vTree
    goWindH :: BranchH s -> BranchH s
    goWindH mHNode =
      case mHNode of
        Nothing -> Nothing
        Just hTree ->
            let yCut      = hTree ^. confineBranch . confineYCut
                topLeft   = curveBox (hTree ^. confineCurve) ^. topLeftBox
                setTop    = if box ^. topSide < yCut then set (confineBranch . confineTop   ) (goWindV (hTree ^. confineBranch . confineTop )) else id
                setBottom =                               set (confineBranch . confineBottom) (goWindV (hTree ^. confineBranch . confineBottom ))
                setCornerWinding = over confineCornerWinding (+ (boolToWind $ inHorizontalRangeBox topLeft box && verticalBezierWind topLeft box bez))
            in  Just . setTop . setBottom . setCornerWinding $ hTree

boolToWind True  = 1
boolToWind False = 0
horizontalBezierWind :: Space s => Point2 s -> Box s -> Bezier s -> Bool
horizontalBezierWind p box bez =
  if {-tr "p ^. pX >= box ^. rightSide" $ -} p ^. pX >= box ^. rightSide
  then True
  else if {-tr "p ^. pX < box ^. leftSide" $ -} p ^. pX < box ^. leftSide
       then False
       else if {-tr "p ^. pY == bez ^. bzStart . pY" $-} p ^. pY == bez ^. bzStart . pY
            then {-tr "p ^. pX >= bez ^. bzStart . pX" $-} p ^. pX >= bez ^. bzStart . pX
            else if {-tr "p ^. pY == bez ^. bzEnd . pY" $-} p ^. pY == bez ^. bzEnd . pY
                 then {-tr "p ^. pX >= bez ^. bzEnd . pX" $-} p ^. pX >= bez ^. bzEnd . pX
                 else if {-tr "p ^. pX == box ^. leftSide" $-} p ^. pX == box ^. leftSide
                      then False
                      else {-tr "not isLeftOfCurve p bez" $-} not $ isLeftOfCurve p bez

verticalBezierWind :: Space s => Point2 s -> Box s -> Bezier s -> Bool
verticalBezierWind p box bez =
  if {-tr "p ^. pY >= box ^. bottomSide" $ -} p ^. pY >= box ^. bottomSide
  then True
  else if {-tr "p ^. pY < box ^. topSide" $ -} p ^. pY < box ^. topSide
       then False
       else if{-tr "p ^. pX == bez ^. bzStart . pX" $-} p ^. pX == bez ^. bzStart . pX
            then {-tr "p ^. pY >= bez ^. bzStart . pY" $-} p ^. pY >= bez ^. bzStart . pY
            else if {-tr "p ^. pX == bez ^. bzEnd . pX" $-} (p ^. pX) == bez ^. bzEnd . pX
                 then {-tr "p ^. pY >= bez ^. bzEnd . pY" $-} p ^. pY >= bez ^. bzEnd . pY
                 else if {-tr "p ^. pY == box ^. topSide" $-} p ^. pY == box ^. topSide
                      then False
                      else {-tr "not isAboveCurve p bez" $-} not $ isAboveCurve p bez

crossesHorizontal start end box bez =
    let interimPoint = {-tr "interimPoint" $ -} Point2 (end ^. pX) (start ^. pY)
    in  if {-tr "inVerticalRangeBox interimPoint" $ -} inVerticalRangeBox interimPoint box
        then (boolToWind $ {- tc "horizontalBezierWind start "        $-} horizontalBezierWind start          box bez) +
             (boolToWind $ {-tc "horizontalBezierWind interimPoint " $-} horizontalBezierWind interimPoint   box bez)
        else 0

crossesVertical start end box bez =
    let interimPoint = {-tr "interimPoint" $ -} Point2 (end ^. pX) (start ^. pY)
    in  if {-tr "inHorizontalRangeBox interimPoint" $-} inHorizontalRangeBox interimPoint box
        then (boolToWind $ {-tc "verticalBezierWind interimPoint " $-} verticalBezierWind interimPoint box bez) +
             (boolToWind $ {-tc "verticalBezierWind end "          $-} verticalBezierWind end          box bez)
        else 0

taxiWindingBezier :: Space s => Point2 s -> Point2 s -> Box s -> Bezier s -> WindingNumber
taxiWindingBezier start end box bez =
    let interimPoint = Point2 (end ^. pX) (start ^. pY)
        crossesH = crossesHorizontal start end box bez
        crossesV = crossesVertical   start end box bez
    in  crossesH + crossesV


data StackFrame s = SF
  { sFDepth :: Depth
  , sFCurveWindHori :: WindingNumber
  , sFCurveWindVert :: WindingNumber
  }

instance Show s => Show (StackFrame s) where
    show (SF depth curveWindHori curveWindVert ) = "d"++show depth ++ "h"++ show curveWindHori ++ "v" ++ show curveWindVert

curveBox (Bez v0 _ v1) = minMaxBox (boxOf v0) (boxOf v1)

pointWinding :: forall s . (Reasonable s, Space s) => ConfineTree s -> Point2 s -> (Point2 s, WindingNumber)
pointWinding tree point = (anchorPoint, anchorWinding + goV 0 tree)
    where
    (anchorPoint, anchorWinding) = goFindAnchorV tree (point, 0)
    searchBox = minMaxBox (boxOf anchorPoint) (boxOf point)
    goFindAnchorV :: BranchV s -> (Point2 s, WindingNumber) -> (Point2 s , WindingNumber)
    goFindAnchorV mVTree prevAnchor =
       case mVTree of
         Nothing    -> prevAnchor
         Just vTree ->
             let xCut   = vTree ^. confineBranch . confineXCut
                 curve  = vTree ^. confineCurve
                 box    = curveBox curve
                 anchor = (box ^. topLeftBox, vTree ^. confineCornerWinding)
             in  if point ^. pX < xCut
                 then goFindAnchorH (vTree ^. confineBranch . confineLeft ) prevAnchor
                 else goFindAnchorH (vTree ^. confineBranch . confineRight)     anchor

    goFindAnchorH :: BranchH s -> (Point2 s, WindingNumber) -> (Point2 s, WindingNumber)
    goFindAnchorH  mHTree prevAnchor =
      case mHTree of
        Nothing -> prevAnchor
        Just hTree ->
             let yCut   = hTree ^. confineBranch . confineYCut
                 curve  = hTree ^. confineCurve
                 box    = curveBox curve
                 anchor = (box ^. topLeftBox, hTree ^. confineCornerWinding)
             in  if point ^. pY < yCut
                 then goFindAnchorV (hTree ^. confineBranch . confineTop   ) prevAnchor
                 else goFindAnchorV (hTree ^. confineBranch . confineBottom)     anchor

    goV :: Depth -> BranchV s -> WindingNumber
    goV depth mVTree =
        case mVTree of
            Nothing -> 0
            Just vTree ->
                let xCut          = vTree ^. confineBranch . confineXCut
                    xOverlap      = vTree ^. confineBranch . confineXOverlap
                    curve         = vTree ^. confineCurve
                    box           = curveBox curve
                    --curveWind     = tr ("goV taxiWindingBezier depth: " ++ show depth ++ " curve " ++ show curve) $  taxiWindingBezier point anchor box curve
                    curveWindHori = --tc ("crossesHorizontal i" ++ show (vTree ^. confineCurveTag) ++ " point " ++ show point ++ " anchor " ++ show anchorPoint) $
                                    crossesHorizontal point anchorPoint box curve
                    curveWindVert = --tc ("crossesVertical i" ++ show (vTree ^. confineCurveTag) ++ " point " ++ show point ++ " anchor " ++ show anchorPoint) $
                                    crossesVertical   point anchorPoint box curve
                    leftBranch    = if searchBox ^. leftSide < xOverlap then goH (depth + 1) (vTree ^. confineBranch . confineLeft ) else 0
                    rightBranch   = if searchBox ^. rightSide >= xCut   then goH (depth + 1) (vTree ^. confineBranch . confineRight) else 0
                    fromCurve     = if searchBox ^. rightSide >= xCut   then curveWindHori + curveWindVert                           else 0
                in  fromCurve + leftBranch + rightBranch

    goH :: Depth -> BranchH s -> WindingNumber
    goH depth mHTree =
        case mHTree of
            Nothing -> 0
            Just hTree ->
                 let yCut          = hTree ^. confineBranch . confineYCut
                     yOverlap      = hTree ^. confineBranch . confineYOverlap
                     curve         = hTree ^. confineCurve
                     box           = curveBox curve
                     --curveWind     = tr ("goH taxiWindingBezier depth: " ++ show depth ++ " curve " ++ show curve) $ taxiWindingBezier point anchor box curve
                     curveWindHori = --tc ("crossesHorizontal i" ++ show (hTree ^. confineCurveTag) ++ " point " ++ show point ++ " anchor "++ show anchorPoint) $
                                     crossesHorizontal point anchorPoint box curve
                     curveWindVert = --tc ("crossesVertical i" ++ show (hTree ^. confineCurveTag) ++ " point " ++ show point ++ " anchor "++ show anchorPoint) $
                                     crossesVertical   point anchorPoint box curve
                     topBranch     = if searchBox ^. topSide < yOverlap then goV (depth + 1) (hTree ^. confineBranch . confineTop     ) else 0
                     bottomBranch  = if searchBox ^. bottomSide >= yCut then goV (depth + 1) (hTree ^. confineBranch . confineBottom  ) else 0
                     fromCurve     = if searchBox ^. bottomSide >= yCut then curveWindHori + curveWindVert                              else 0
                 in  fromCurve + topBranch + bottomBranch

instance Space s => CanConfine (Shape s) where
    addToConfineTree table maxSize substance tree shapes =
        let curves = V.concat .
                     map prepareOutline .
                     view shapeOutlines $
                     shapes
            curveTags = V.zip curves $ V.iterateN (V.length curves) (+1) 0
        in
        --foldl (V.foldl (treeOrderFold table addConfineTree)) tree .
        --map (breakVector maxSize . outlineToConfinements substance) .
        --view shapeOutlines
        V.foldl (windConfineTree substance) (V.foldl (addBezierToConfineTree substance) tree curveTags) curves


instance Space s => CanConfine (ShapeTree Int s) where
    addToConfineTree table maxSize substance tree =
        foldl (addToConfineTree table maxSize substance) tree . flattenShapeTree
