{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.ConfineTree.Sweep
  ( sweepConfineTree
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

sweepConfineTree :: forall s m
                 . ( Space s
                   , MonadIO m
                   )
                 => m ()
                 -> ([Bezier s] -> m ())
                 -> ([Bezier s] -> m ())
                 -> ConfineTree s
                 -> m (ConfineTree s)
sweepConfineTree crossingOp
                 discardOp
                 continueOp
                 mTree =
   do (mTree', _) <- sweep Vertical
                           0
                           True
                           (onAxis Vertical   minBound)
                           (onAxis Horizontal minBound)
                           ( makeBox minBound
                                     minBound
                                     maxBound
                                     maxBound )
                           mTree
                           []
      return mTree'
   where
   sweep :: forall axis
         . (Axis axis, axis~NextAxis(NextAxis axis))
         => axis
         -> Int
         -> Bool
         -> With axis s
         -> With (NextAxis axis) s
         -> Box s
         -> Branch axis s
         -> [(CurveTag, TaggedBezier s)]
         -> m (Branch axis s, [(CurveTag, TaggedBezier s)])
   sweep axis
         depth
         moreSide
         parentCut
         parentLine
         boundary
         mTree
         parentOverhangs =
       case mTree of
           Nothing -> do --liftIO . putStrLn $ concat (replicate depth "   ") ++ "sweep Nothing"
                         return (Nothing, parentOverhangs)
           Just tree ->
               let newBez = tree ^. confineCurve
                   newBezBox = boxOf newBez
                   itemTagId = tree ^. confineItemTagId
                   cut = tree ^. confineCut
                   lessBox = set (maxBox . athwart axis) (fromAxis axis cut) boundary
                   moreBox = set (minBox . athwart axis) (fromAxis axis cut) boundary
                   sweepLess = sweep (nextAxis axis) (depth + 1) False parentLine cut lessBox (tree ^. confineLessCut)
                   sweepMore = sweep (nextAxis axis) (depth + 1) True  parentLine cut moreBox (tree ^. confineMoreCut)
                   ---mess x = liftIO $ putStrLn $ concat (replicate depth "   ") ++ "sweep tag " ++ show (tree ^. confineCurveTag) ++ " " ++ x
                   thisCurve = (tree ^. confineCurveTag, TaggedBezier newBez itemTagId)
               in
               do  --mess $ "pCut " ++ show parentCut ++ " pLine " ++ show parentLine ++ " boundary " ++ show boundary
                   --mess $ "  parentOverhangs " ++ showOverhangs parentOverhangs
                   parentModified <- if moreSide
                                     then do modified <- foldM (addCrossing crossingOp axis parentCut parentLine) tree parentOverhangs
                                             --mess $ "crossingsParent " ++ show (modified ^. confineCrossings)
                                             return modified
                                     else return tree
                   (mLess, fromLess) <- sweepLess parentOverhangs
                   --mess $ "         fromLess " ++ showOverhangs fromLess
                   let (overhangsCut, rest) = segregate (bezOverhangsCut axis cut) fromLess
                       forMore = thisCurve:overhangsCut
                   --mess $ "     overhangsCut " ++ showOverhangs overhangsCut
                   --mess $ "             rest " ++ showOverhangs rest
                   --mess $ "          forMore " ++ showOverhangs forMore
                   (mMore, fromMore) <- sweepMore forMore
                   --mess $ "         fromMore " ++ showOverhangs fromMore
                   let (continueOverhangs, discarded) = segregate (bezOverhangs axis boundary) (rest ++ fromMore)
                   discardOp (map (tBez. snd) discarded)
                   continueOp (map (tBez. snd) continueOverhangs)
                   --mess $ "continueOverhangs " ++ showOverhangs continueOverhangs
                   --mess $ "fromLess " ++ show (length fromLess) ++ " fromMore " ++ show (length fromMore) ++ " continue " ++ show (length continueOverhangs)
                   childModified <- if not moreSide
                                    then do modified <- foldM (addCrossing crossingOp axis parentCut parentLine) parentModified continueOverhangs
                                            --mess $ "crossingsChild " ++ show (modified ^. confineCrossings)
                                            return modified
                                    else return parentModified
                   return ( Just .
                            set confineMoreCut mMore .
                            set confineLessCut mLess $
                            childModified
                          , continueOverhangs
                          )
   bezOverhangsCut :: (Axis axis, Space s)
                   => axis
                   -> With axis s
                   -> (CurveTag, TaggedBezier s)
                   -> Bool
   bezOverhangsCut  axis
                   cut
                   (curveTag, TaggedBezier bez _) =
     let box = boxOf bez
     in  box ^. maxBox . athwart axis >= fromAxis axis cut

   bezOverhangs :: Axis axis
                => axis
                -> Box s
                -> (CurveTag, TaggedBezier s)
                -> Bool
   bezOverhangs axis
                boundary
                (curveTag, TaggedBezier bez _) =
       let box = boxOf bez
           maxAthwart = box ^. maxBox . athwart axis >= boundary ^. maxBox . athwart axis
           maxAlong   = box ^. maxBox . along   axis >= boundary ^. maxBox . along   axis
       in  maxAthwart || maxAlong

   showOverhangs :: [(CurveTag, TaggedBezier s)] -> String
   showOverhangs = show . map (\(curveTag, TaggedBezier _ itemTagId) -> (curveTag, itemTagId))

addCrossing :: forall axis s m
            .  ( Axis axis
               --, SwitchAxis (NextAxis axis)
               , axis~NextAxis(NextAxis (axis))
               , Monad m
               , Space s
               )
            => m ()
            -> axis
            -> With axis s
            -> With (NextAxis axis) s
            ->    Confine axis s
            -> (CurveTag, TaggedBezier s)
            -> m (Confine axis s)
addCrossing op axis parentCut parentLine tree (tag, TaggedBezier bez itemTagId) =
    --tcP ("-------- addCrossing from new tag: " ++ show tag ++ " over: " ++ show (tree ^. confineCurveTag)) $
    let start = parentCut
        end   = tree ^. confineCut
        tree' = over confineConsidered (+1) $
                if --trWhen (tag == checkTag) ("+newTag: " ++ show tag ++ " treeCurveTag: " ++ show (tree ^. confineCurveTag) ++ "   pLine " ++ show parentLine ++ " start: " ++ show start ++ " end: " ++ show end) $
                   crossesAlong (nextAxis axis) (unAxis parentLine) (unAxis start) (unAxis end) bez
                then over confineCrossings (toggleCrossing itemTagId) $
                     --over confineCrossedCurves (tag:) $
                     tree
                else tree
    in  do op
           return tree'
