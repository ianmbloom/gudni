{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.ConfineTree.Decorate
  ( addCrossingToConfineTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad

checkTag = 0

addCrossingToConfineTree :: forall s m
                         .  (Space s, Monad m)
                         => m ()
                         -> ItemTagId
                         -> CurveTag
                         -> Bezier s
                         -> ConfineTree s
                         -> m (ConfineTree s)
addCrossingToConfineTree op itemTagId tag initBez mTree =
    case mTree of
        Nothing -> return Nothing
        Just tree ->
            fmap Just $ goChildren Vertical True (onAxis Vertical minBound) (onAxis Horizontal minBound) initBez tree
    where
    goChildren :: forall axis
               .  ( Axis axis
                  , SwitchAxis axis
                  , SwitchAxis (NextAxis axis)
                  , axis~NextAxis(NextAxis axis)
                  )
               => axis
               -> Bool
               -> With axis s
               -> With (NextAxis axis) s
               -> Bezier s
               -> Confine axis s
               -> m (Confine axis s)
    goChildren axis moreSide parentCut parentLine bez tree =
       let goNext :: Bool -> Maybe (Confine (NextAxis axis) s) -> m (Maybe (Confine (NextAxis axis) s))
           goNext nextSide = goAdd (nextAxis axis) nextSide parentLine (tree ^. confineCut) bez
           --message = "newTag: " ++ show tag ++ " treeCurveTag: " ++ show (tree ^. confineCurveTag) ++ "   pLine " ++ show parentLine ++ " pCut " ++ show parentCut ++ " "
           pLine = fromAxis (nextAxis axis) parentLine
           box = boxOf bez
           cut = fromAxis axis (tree ^. confineCut)
           maxAlo = box ^. maxBox . along axis
           minAlo = box ^. minBox . along axis
           maxAth = box ^. maxBox . athwart axis
           minAth = box ^. minBox . athwart axis
       in  if --trWhen (tag == checkTag) (" " ++ message ++ " both ") $
              --(
              -- trWhen (tag == checkTag) (" " ++ message ++ "    moreSide " ++ show (    moreSide) ++ " && (maxAth >= pLine)" ++ show (maxAth >= pLine)) $
              --                                    moreSide                               && (maxAth >= pLine)) ||
              --(
              -- trWhen (tag == checkTag) (" " ++ message ++ "not moreSide " ++ show (not moreSide) ++ " && (minAth <  pLine)" ++ show (minAth <  pLine)) $
              --                                not moreSide                               && (minAth <= pLine))
              True
           then (if (maxAth >= cut)
                 then (\tree ->
                         do moreTree <- goNext True  (tree ^. confineMoreCut)
                            return $ set confineMoreCut moreTree tree
                      )
                 else return) <=<
                (if (minAth <= cut)
                 then (\tree ->
                         do lessTree <- goNext False (tree ^. confineLessCut)
                            return $ set confineLessCut lessTree tree
                      )
                 else return) $
                tree
           else return tree

    goAdd :: forall axis
          .  ( Axis axis
             , SwitchAxis axis
             , SwitchAxis (NextAxis axis)
             , axis~NextAxis(NextAxis axis)
             )
          => axis
          -> Bool
          -> With axis s
          -> With (NextAxis axis) s
          -> Bezier s
          -> Maybe (Confine axis s)
          -> m (Maybe (Confine axis s))
    goAdd axis moreSide parentCut parentLine bez mTree =
        case mTree of
            Nothing -> return Nothing
            Just tree ->
                let box = boxOf bez
                    maxAth = box ^. maxBox . athwart axis
                    minAth = box ^. minBox . athwart axis
                    cut    = fromAxis axis (tree ^. confineCut)
                in
                fmap Just $
                if minAth < cut && maxAth > cut
                then  let (less, more) = splitBezier 0.5 bez
                      in  goChildren axis moreSide parentCut parentLine less <=<
                          addCrossing axis parentCut parentLine less <=<
                          goChildren axis moreSide parentCut parentLine more <=<
                          addCrossing axis parentCut parentLine more $
                          tree
                else      goChildren axis moreSide parentCut parentLine bez <=<
                          addCrossing axis parentCut parentLine bez $
                          tree

    addCrossing :: forall axis
                .  ( Axis axis
                   , SwitchAxis (NextAxis axis)
                   , axis~NextAxis(NextAxis (axis))
                   )
                => axis
                -> With axis s
                -> With (NextAxis axis) s
                -> Bezier s
                ->    Confine axis s
                -> m (Confine axis s)
    addCrossing axis parentCut parentLine bez tree =
        --tcP ("-------- addCrossing from new tag: " ++ show tag ++ " over: " ++ show (tree ^. confineCurveTag)) $
        let start = parentCut
            end   = tree ^. confineCut
            tree' = if --trWhen (tag == checkTag) ("+newTag: " ++ show tag ++ " treeCurveTag: " ++ show (tree ^. confineCurveTag) ++ "   pLine " ++ show parentLine ++ " start: " ++ show start ++ " end: " ++ show end) $
                       crossesAlong (nextAxis axis) (unAxis parentLine) (unAxis start) (unAxis end) bez
                    then over confineCrossings (toggleCrossing itemTagId) .
                         over confineCrossedCurves ({-trWhen (tag==checkTag) ("consTag " ++ show tag ++ " treeTag " ++ show (tree ^. confineCurveTag)) .-} (tag:)) $
                         tree
                    else tree
        in  do op
               return tree'
