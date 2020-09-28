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

module Graphics.Gudni.Raster.ConfineTree.Traverse
  ( pointWinding
  , traverseCrossings
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ConfineTree.Type
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

{-
myCrosses :: (Space s) => CurveTag -> Point2 s -> Point2 s -> Bezier s -> Bool
myCrosses curveTag start end bez =
    let iP = interimPoint start end
    in
    tr ("crossesAlong " ++ show curveTag ++ " Vertical"  ) (crossesAlong Vertical   (start ^. pX) (start ^. pY) (iP  ^. pY) bez) /=
    tr ("crossesAlong " ++ show curveTag ++ " Horizontal") (crossesAlong Horizontal (iP    ^. pY) (iP    ^. pX) (end ^. pX) bez)
-}

crossCurve :: (Space s)
           => CurveTag
           -> Point2 s
           -> Point2 s
           -> [ItemTagId]
           -> (ItemTagId, Bezier s)
           -> [ItemTagId]
crossCurve curveTag anchor point stack (itemTagId, bez) =
    if --tc ("crossCurve " ++ show curveTag ++ " bez " ++ show bez ++ " anchor " ++ show anchor ++ " point "++ show point) $
       crosses anchor point bez
    then toggleCrossing itemTagId stack
    else stack

firstTwo (a, b, c) = (a, b)
lastOf3  (_, _, c) = c

pointWinding :: forall s . (Space s) => ConfineTree s -> Point2 s -> (Point2 s, [ItemTagId],[CurveTag])
pointWinding mTree point =
    let (anchor, anchorStack) = {-trP "anchorStack" $-} collectAnchorStack mTree point
        curves = findCurves mTree anchor point
        --crossedOverCurves = foldl (crossCurve anchor point) anchorStack $ map firstTwo curves
        crossedStack = traverseCurves mTree anchor point anchorStack
    in  (anchor, crossedStack, map lastOf3 curves)

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

traverseCurve :: forall s axis
              .  (Space s)
              => Point2 s
              -> Point2 s
              -> Box s
              -> Confine axis s
              -> [ItemTagId]
              -> [ItemTagId]
traverseCurve anchor point box tree stack =
  --tcP ("traverseCurve " ++ show (tree ^. confineCurveTag)) $
  let itemTagId = tree ^. confineItemTagId
      bez = tree ^. confineCurve
      bezBox = boxOf bez
  in
  if bezBox ^. maxBox . pX >= box ^. minBox . pX &&
     bezBox ^. maxBox . pY >= box ^. minBox . pY
  then crossCurve (tree ^. confineCurveTag) anchor point stack (itemTagId, bez)
  else stack

traverseCurves :: forall s
               .  (Space s)
               => ConfineTree s
               -> Point2 s
               -> Point2 s
               -> [ItemTagId]
               -> [ItemTagId]
traverseCurves mTree anchor point =
    go Vertical mTree
    where
    box :: Box s
    box = boxAroundPoints anchor point
    go :: (Axis axis) => axis -> Branch axis s -> [ItemTagId] -> [ItemTagId]
    go axis mTree =
      case mTree of
        Nothing -> id
        Just tree ->
            moreCut      axis (box ^. maxBox . athwart axis) tree (go (nextAxis axis)) .
            lessOverhang axis (box ^. minBox . athwart axis) tree (go (nextAxis axis)) .
            traverseCurve anchor point box tree

curveOverlaps :: forall s axis
              .  (Space s)
              => Box s
              -> Confine axis s
              -> [(ItemTagId, Bezier s, CurveTag)]
              -> [(ItemTagId, Bezier s, CurveTag)]
curveOverlaps anchorBox tree =
  let itemTagId = tree ^. confineItemTagId
      bez = tree ^. confineCurve
      bezBox = boxOf bez
  in
  if bezBox ^. maxBox . pX >= anchorBox ^. minBox . pX &&
     bezBox ^. maxBox . pY >= anchorBox ^. minBox . pY
  then ((itemTagId, bez, tree ^. confineCurveTag):)
  else id

moreCut :: (Axis axis, Space s) => axis -> s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
moreCut axis s tree goNext =
    if onAxis axis s > tree ^. confineCut
    then goNext (tree ^. confineMoreCut)
    else id

lessOverhang :: (Axis axis, Space s) => axis -> s -> Confine axis s -> (Branch (NextAxis axis) s -> a -> a) -> a -> a
lessOverhang axis point tree goNext =
    if onAxis axis point <=  tree ^. confineOverhang
    then goNext (tree ^. confineLessCut)
    else id

findCurves :: forall s
           .  (Space s)
           => ConfineTree s
           -> Point2 s
           -> Point2 s
           -> [(ItemTagId, Bezier s, CurveTag)]
findCurves mTree anchor point =
    go Vertical mTree []
    where
    box :: Box s
    box = boxAroundPoints anchor point
    go :: (Axis axis)
       => axis
       -> Branch axis s
       -> [(ItemTagId, Bezier s, CurveTag)]
       -> [(ItemTagId, Bezier s, CurveTag)]
    go axis mTree =
      case mTree of
        Nothing -> id
        Just tree ->
            moreCut      axis (box ^. maxBox . athwart axis) tree (go (nextAxis axis)) .
            lessOverhang axis (box ^. minBox . athwart axis) tree (go (nextAxis axis)) .
            curveOverlaps box tree
