{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.ConfineTree.Sweep
  ( sweepConfineTree
  , SweepStored(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.ItemStack
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

data SweepStored s = SweepStored
    { ssBox    :: Box s
    , ssCurves :: [TaggedBezier s]
    }

instance Show s => Show (SweepStored s) where
  show ss = "sto " ++ show (ssCurves ss)

addCrossingM :: forall axis s m
             .  ( Axis axis
                --, ToEitherAxis (PerpendicularTo axis)
                , axis~PerpendicularTo(PerpendicularTo (axis))
                , Monad m
                , Space s
                )
             => m ()
             -> axis
             -> Athwart axis s
             -> Along axis s
             -> Confine axis s
             -> ItemStack
             -> TaggedBezier s
             -> m ItemStack
addCrossingM op axis parentCut parentLine tree stack curve =
    do op
       return $ crossCurveAlong (perpendicularTo axis) parentCut parentLine (tree ^. confineCut) curve stack

sweepConfineTree :: forall s m
                 . ( Space s
                   , MonadIO m
                   )
                 => m ()
                 -> m ()
                 -> ([TaggedBezier s] -> m ())
                 -> ([TaggedBezier s] -> m ())
                 -> (Box s -> m ())
                 -> ([TaggedBezier s] -> m ())
                 -> m ()
                 -> ((Point2 s, Point2 s) -> m ())
                 -> m ()
                 -> Int
                 -> ConfineTree s
                 -> m (DecorateTree s)
sweepConfineTree crossingOp
                 branchStep
                 discardOp
                 keepOp
                 nothingOp
                 pushBypassOp
                 popBypassOp
                 pushPathOp
                 popPathOp
                 depthLimit
                 mTree =
   do (dTree, _, _) <- sweep Vertical
                              0
                              True
                              (toAlong Horizontal minBound)
                              (toAlong Vertical   minBound)
                              ( makeBox minBound
                                        minBound
                                        maxBound
                                        maxBound )
                              mTree
                              []
      return dTree
   where
   sweep :: forall axis
         . (Axis axis, axis~PerpendicularTo(PerpendicularTo axis))
         => axis
         -> Int
         -> Bool
         -> Athwart axis s
         -> Along axis s
         -> Box s
         -> Branch axis s
         -> [TaggedBezier s]
         -> m (DecoTree axis s, [TaggedBezier s], Int)
   sweep axis
         depth
         moreSide
         parentCut
         parentLine
         boundary
         mTree
         parentOverhangs =
       let lessSide = not moreSide
           ind x = return () -- liftIO $ putStrLn $ concat (replicate depth "      ") ++ "sweep " ++ x

           addCrossings overhangs tree itemStack = foldM (addCrossingM crossingOp axis parentCut parentLine tree) itemStack overhangs
           handleOverhangs :: String -> [TaggedBezier s] -> m ([TaggedBezier s])
           handleOverhangs mess curves =
               let (keep, discard) = segregate (overhangs axis boundary) curves
               in
               do  ind $ mess ++ "keep " ++ show keep ++ " discard " ++ show discard
                   keepOp keep
                   discardOp discard
                   return keep
       in
       case mTree of
           Nothing ->
               do  ind "Nothing "
                   nothingOp boundary
                   return (DecoLeaf, parentOverhangs, 0)
           Just tree ->
               let mess = "tag " ++ show (tree ^. confineCurve . tCurveTag) ++ " "
                   indTag x = ind (mess ++ x)
                   thisCurve = tree ^. confineCurve
                   cut :: Athwart axis s
                   cut = tree ^. confineCut
                   lessBox = set (maxBox . athwart axis) cut boundary
                   moreBox = set (minBox . athwart axis) cut boundary
                   sweepLess = sweep (perpendicularTo axis) (depth + 1) False parentLine cut lessBox (tree ^. confineLessCut)
                   sweepMore = sweep (perpendicularTo axis) (depth + 1) True  parentLine cut moreBox (tree ^. confineMoreCut)
                   goLessSide = do indTag $ "lessSide ===="
                                   (dLess, fromLess, depthLess) <- sweepLess parentOverhangs
                                   indTag $ "        fromLess " ++ show fromLess
                                   let (forMore, bypassMore) = segregate (overhangsCut axis cut) fromLess
                                   indTag $ "         forMore " ++ show forMore
                                   indTag $ "      bypassMore " ++ show bypassMore
                                   pushBypassOp bypassMore
                                   (dMore, fromMore, depthMore) <- sweepMore (thisCurve:forMore)
                                   popBypassOp
                                   indTag $ "        fromMore " ++ show fromMore
                                   let continue = fromMore ++ bypassMore
                                   itemStack <- addCrossings continue tree []
                                   indTag $ "       crossings " ++ show itemStack
                                   let depthMax = max depthLess depthMore
                                   return (dLess, dMore, itemStack, continue, depthMax)
                   goMoreSide = do indTag $ "moreSide ===="
                                   itemStack <- addCrossings parentOverhangs tree []
                                   indTag $ "       crossings " ++ show itemStack
                                   let (bypassLess, forLess) = segregate (beyondCut axis cut) parentOverhangs
                                   indTag $ "         forLess " ++ show forLess
                                   indTag $ "      bypassLess " ++ show bypassLess
                                   pushBypassOp bypassLess
                                   (dLess, fromLess, depthLess) <- sweepLess forLess
                                   popBypassOp
                                   indTag $ "        fromLess " ++ show fromLess
                                   let forMore = fromLess ++ bypassLess
                                   indTag $ "         forMore " ++ show forMore
                                   (dMore, fromMore, depthMore) <- sweepMore (thisCurve:forMore)
                                   indTag $ "        fromMore " ++ show fromMore
                                   let depthMax = max depthLess depthMore
                                   return (dLess, dMore, itemStack, fromMore, depthMax)
               in
               do pushPathOp (makePath (perpendicularTo axis) parentCut parentLine cut)
                  indTag $ "moreSide " ++ show moreSide ++ " pCut " ++ show parentCut ++ " pLine " ++ show parentLine ++ " boundary " ++ show boundary
                  indTag $ " parentOverhangs " ++ show parentOverhangs
                  (mLess, mMore, itemStack, fromMore, depth) <- if lessSide
                                                                then goLessSide
                                                                else goMoreSide
                  popPathOp
                  keep <- handleOverhangs mess fromMore
                  branchStep
                  let ret = if depth < depthLimit
                            then DecoLeaf
                            else DecoBranch
                                     { _decoCurveTag = tree ^. confineCurve . tCurveTag
                                     , _decoCut = tree ^. confineCut
                                     , _decoCrossings = itemStack
                                     , _decoLessCut = mLess
                                     , _decoMoreCut = mMore
                                     }
                  return ( ret
                         , keep
                         , depth
                         )

   collectOverhangs :: Axis axis => axis -> Branch axis s -> [TaggedBezier s]
   collectOverhangs axis mTree =
     case mTree of
       Nothing -> []
       Just tree ->
          let lessOverhangs = collectOverhangs (perpendicularTo axis) (tree ^. confineLessCut)
              moreOverhangs = collectOverhangs (perpendicularTo axis) (tree ^. confineMoreCut)
          in  (tree ^. confineCurve):(lessOverhangs ++ moreOverhangs)

   overhangsCut :: (Axis axis, Space s)
                => axis
                -> Athwart axis s
                -> TaggedBezier s
                -> Bool
   overhangsCut axis cut (TaggedBezier bez _ _) =
       let box = boxOf bez
       in  box ^. maxBox . athwart axis >= cut

   beyondCut :: (Axis axis, Space s)
             => axis
             -> Athwart axis s
             -> TaggedBezier s
             -> Bool
   beyondCut axis cut (TaggedBezier bez _ _) =
       let box = boxOf bez
       in  box ^. minBox . athwart axis >= cut

   overhangs :: Axis axis
             => axis
             -> Box s
             -> TaggedBezier s
             -> Bool
   overhangs axis
             boundary
             (TaggedBezier bez _ _) =
       let box = boxOf bez
           overhangsAthwart = box ^. maxBox . athwart axis >= boundary ^. maxBox . athwart axis
           overhangsAlong   = box ^. maxBox . along   axis >= boundary ^. maxBox . along   axis
       in  overhangsAthwart || overhangsAlong


makePath :: (Axis axis, Space s) => axis -> Along axis s -> Athwart axis s -> Along axis s -> (Point2 s, Point2 s)
makePath axis parentCut parentLine cut =
     let start  = pointAlongAxis axis parentCut parentLine
         end    = pointAlongAxis axis cut       parentLine
     in  (start, end)
