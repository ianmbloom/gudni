{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Sweep
  ( sweepConfineTree
  , SweepStored(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.Primitive.Stack
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

data SweepStored s = SweepStored
    { ssBox    :: Box s
    , ssPrims :: [TPrim s]
    }

instance Show s => Show (SweepStored s) where
  show ss = "sto " ++ show (ssPrims ss)

addCrossingM :: forall axis s m
             .  ( Axis axis
                --, ToEitherAxis (PerpendicularTo axis)
                , axis~PerpendicularTo(PerpendicularTo (axis))
                , Monad m
                , Space s
                )
             => (PrimTagId -> m (Primitive s))
             -> m ()
             -> axis
             -> Athwart axis s
             -> Along axis s
             -> Confine axis s
             -> PrimStack
             -> PrimTagId
             -> m PrimStack
addCrossingM getPrim op axis parentCut parentLine tree stack primTagId =
    do op
       prim <- getPrim primTagId
       return $ crossPrimAlong (perpendicularTo axis) parentCut parentLine (tree ^. confineCut) primTagId prim stack

sweepConfineTree :: forall s m
                 . ( Space s
                   , MonadIO m
                   )
                 => (PrimTagId -> m (Box s))
                 -> (PrimTagId -> m (Primitive s))
                 -> m ()
                 -> m ()
                 -> (PrimStack -> PrimStack -> m ())
                 -> (Box s -> m ())
                 -> (PrimStack -> m ())
                 -> m ()
                 -> ((Point2 s, Point2 s) -> m ())
                 -> m ()
                 -> Int
                 -> ConfineTree s
                 -> m (DecorateTree s)
sweepConfineTree getBox
                 getPrim
                 crossingOp
                 branchStep
                 overhangOp
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
         -> [PrimTagId]
         -> m (DecoTree axis s, [PrimTagId], Int)
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

           addCrossings overhangs tree primStack = foldM (addCrossingM getPrim crossingOp axis parentCut parentLine tree) primStack overhangs
           handleOverhangs :: String -> [PrimTagId] -> m [PrimTagId]
           handleOverhangs mess primStack =
               do  (keep, discard) <- partitionM (overhangs axis boundary) primStack
                   ind $ mess ++ "keep " ++ show keep ++ " discard " ++ show discard
                   overhangOp keep discard
                   return keep
       in
       case mTree of
           Nothing ->
               do  ind "Nothing "
                   nothingOp boundary
                   return (DecoLeaf, parentOverhangs, 0)
           Just tree ->
               let mess = "tag " ++ show (tree ^. confinePrimTagId) ++ " "
                   indTag x = ind (mess ++ x)
                   thisCurve = tree ^. confinePrimTagId
                   cut :: Athwart axis s
                   cut = tree ^. confineCut
                   lessBox = set (maxBox . athwart axis) cut boundary
                   moreBox = set (minBox . athwart axis) cut boundary
                   sweepLess = sweep (perpendicularTo axis) (depth + 1) False parentLine cut lessBox (tree ^. confineLessCut)
                   sweepMore = sweep (perpendicularTo axis) (depth + 1) True  parentLine cut moreBox (tree ^. confineMoreCut)
                   goLessSide = do indTag $ "lessSide ===="
                                   (dLess, fromLess, depthLess) <- sweepLess parentOverhangs
                                   indTag $ "        fromLess " ++ show fromLess
                                   (forMore, bypassMore) <- partitionM (overhangsCut axis cut) fromLess
                                   indTag $ "         forMore " ++ show forMore
                                   indTag $ "      bypassMore " ++ show bypassMore
                                   pushBypassOp bypassMore
                                   (dMore, fromMore, depthMore) <- sweepMore (thisCurve:forMore)
                                   popBypassOp
                                   indTag $ "        fromMore " ++ show fromMore
                                   let continue = fromMore ++ bypassMore
                                   primStack <- addCrossings continue tree []
                                   indTag $ "       crossings " ++ show primStack
                                   let depthMax = 1 + max depthLess depthMore
                                   return (dLess, dMore, primStack, continue, depthMax)
                   goMoreSide = do indTag $ "moreSide ===="
                                   primStack <- addCrossings parentOverhangs tree []
                                   indTag $ "       crossings " ++ show primStack
                                   (bypassLess, forLess) <- partitionM (beyondCut axis cut) parentOverhangs
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
                                   let depthMax = 1 + max depthLess depthMore
                                   return (dLess, dMore, primStack, fromMore, depthMax)
               in
               do pushPathOp (makePath (perpendicularTo axis) parentCut parentLine cut)
                  indTag $ "moreSide " ++ show moreSide ++ " pCut " ++ show parentCut ++ " pLine " ++ show parentLine ++ " boundary " ++ show boundary
                  indTag $ " parentOverhangs " ++ show parentOverhangs
                  (mLess, mMore, primStack, fromMore, depth) <- if lessSide
                                                                then goLessSide
                                                                else goMoreSide
                  popPathOp
                  keep <- handleOverhangs mess fromMore
                  branchStep
                  let ret = if depth <= depthLimit
                            then DecoLeaf
                            else DecoBranch
                                     { _decoCut = tree ^. confineCut
                                     , _decoCrossings = primStack
                                     , _decoLessCut = mLess
                                     , _decoMoreCut = mMore
                                     }
                  return ( ret
                         , keep
                         , depth
                         )

   collectOverhangs :: Axis axis => axis -> Branch axis s -> PrimStack -> m PrimStack
   collectOverhangs axis mTree stack =
     case mTree of
       Nothing -> return stack
       Just tree ->
          fmap ((tree ^. confinePrimTagId):) $
          collectOverhangs (perpendicularTo axis) (tree ^. confineLessCut) =<<
          collectOverhangs (perpendicularTo axis) (tree ^. confineMoreCut) =<<
          return stack

   overhangsCut :: Axis axis
                => axis
                -> Athwart axis s
                -> PrimTagId
                -> m Bool
   overhangsCut axis cut primTagId =
       do box <- getBox primTagId
          return $ box ^. maxBox . athwart axis >= cut

   beyondCut :: Axis axis
             => axis
             -> Athwart axis s
             -> PrimTagId
             -> m Bool
   beyondCut axis cut primTagId =
       do box <- getBox primTagId
          return $ box ^. minBox . athwart axis >= cut

   overhangs :: Axis axis
             => axis
             -> Box s
             -> PrimTagId
             -> m Bool
   overhangs axis
             boundary
             primTagId =
       do  box <- getBox primTagId
           let overhangsAthwart = box ^. maxBox . athwart axis >= boundary ^. maxBox . athwart axis
               overhangsAlong   = box ^. maxBox . along   axis >= boundary ^. maxBox . along   axis
           return $ overhangsAthwart || overhangsAlong


makePath :: (Axis axis, Space s) => axis -> Along axis s -> Athwart axis s -> Along axis s -> (Point2 s, Point2 s)
makePath axis parentCut parentLine cut =
     let start  = pointAlongAxis axis parentCut parentLine
         end    = pointAlongAxis axis cut       parentLine
     in  (start, end)
