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
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Stack
import Graphics.Gudni.Raster.ConfineTree.Primitive.Cross
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Storage
import Graphics.Gudni.Raster.TagTypes

import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class


addCrossingM :: forall axis s m
             .  ( Axis axis
                --, ToEitherAxis (PerpendicularTo axis)
                , axis~PerpendicularTo(PerpendicularTo (axis))
                , TreeConstraints s m
                )
             => s
             -> axis
             -> Athwart axis s
             -> Along axis s
             -> ConfineTagId s
             -> [FabricTagId]
             -> PrimTagId
             -> TreeMonad s m [FabricTagId]
addCrossingM limit axis parentCut parentLine treeId stack primTagId =
    do tree <- loadConfineTag treeId
       prim <- loadTreePrim primTagId
       return $
           if crossesPrimAlong limit (perpendicularTo axis) parentCut parentLine (toAthwart axis $ tree ^. confineTagCut) prim
           then toggleItemActive (prim ^. primFabricTagId) stack
           else stack

sweepConfineTree :: forall s m
                 . ( TreeConstraints s m
                   )
                 => s
                 -> Int
                 -> ConfineTagId s
                 -> TreeMonad s m (DecoTagId s)
sweepConfineTree limit
                 depthLimit
                 treeId =
   do (dTree, _, _) <- sweep limit
                             Vertical
                             0
                             True
                             (toAlong Horizontal minBound)
                             (toAlong Vertical   minBound)
                             ( makeBox minBound
                                       minBound
                                       maxBound
                                       maxBound )
                             treeId
                             []
      return dTree
   where
   sweep :: forall axis
         . (Axis axis, axis~PerpendicularTo(PerpendicularTo axis))
         => s
         -> axis
         -> Int
         -> Bool
         -> Athwart axis s
         -> Along axis s
         -> Box s
         -> ConfineTagId s
         -> [PrimTagId]
         -> TreeMonad s m (DecoTagId s, [PrimTagId], Int)
   sweep limit
         axis
         depth
         moreSide
         parentCut
         parentLine
         boundary
         treeId
         parentOverhangs =
       let lessSide = not moreSide
           ind x = return () -- liftIO $ putStrLn $ concat (replicate depth "      ") ++ "sweep " ++ x

           addCrossings overhangs tree primStack = foldM (addCrossingM limit axis parentCut parentLine tree) primStack overhangs
           handleOverhangs :: [PrimTagId] -> TreeMonad s m [PrimTagId]
           handleOverhangs primStack =
               do  (keep, discard) <- partitionM (overhangs axis boundary) primStack
                   return keep
       in
       if treeId == nullConfineTagId
       then return (nullDecoTagId, parentOverhangs, 0)
       else do  tree <- loadConfineTag treeId
                let thisCurve = tree ^. confineTagPrimTagId
                    cut :: Athwart axis s
                    cut = toAthwart axis $ tree ^. confineTagCut
                    lessBox = set (maxBox . athwart axis) cut boundary
                    moreBox = set (minBox . athwart axis) cut boundary
                    sweepLess = sweep limit (perpendicularTo axis) (depth + 1) False parentLine cut lessBox (tree ^. confineTagLessCut)
                    sweepMore = sweep limit (perpendicularTo axis) (depth + 1) True  parentLine cut moreBox (tree ^. confineTagMoreCut)
                    goLessSide = do (dLess, fromLess, depthLess) <- sweepLess parentOverhangs
                                    (forMore, bypassMore) <- partitionM (overhangsCut axis cut) fromLess
                                    (dMore, fromMore, depthMore) <- sweepMore (thisCurve:forMore)
                                    let continue = fromMore ++ bypassMore
                                    primStack <- addCrossings continue treeId []
                                    let depthMax = 1 + max depthLess depthMore
                                    return (dLess, dMore, primStack, continue, depthMax)
                    goMoreSide = do primStack <- addCrossings parentOverhangs treeId []
                                    (bypassLess, forLess) <- partitionM (beyondCut axis cut) parentOverhangs
                                    (dLess, fromLess, depthLess) <- sweepLess forLess
                                    let forMore = fromLess ++ bypassLess
                                    (dMore, fromMore, depthMore) <- sweepMore (thisCurve:forMore)
                                    let depthMax = 1 + max depthLess depthMore
                                    return (dLess, dMore, primStack, fromMore, depthMax)
                (mLess, mMore, primStack, fromMore, depth) <- if lessSide
                                                              then goLessSide
                                                              else goMoreSide
                keep <- handleOverhangs fromMore
                ret <-
                      if depth <= depthLimit
                      then return nullDecoTagId
                      else do slice <- foldIntoPileS treeCrossingPile primStack
                              let treeTag = DecoTag { _decoTagCut        = tree ^. confineTagCut
                                                    , _decoTagHorizontal = isHorizontal axis
                                                    , _decoTagCrossings  = slice
                                                    , _decoTagLessCut    = mLess
                                                    , _decoTagMoreCut    = mMore
                                                    }
                              DecoTagId <$> addToPileS treeDecoPile treeTag
                return ( ret
                       , keep
                       , depth
                       )

   collectOverhangs :: Axis axis => axis -> ConfineTagId s -> [PrimTagId] -> TreeMonad s m [PrimTagId]
   collectOverhangs axis treeId stack =
     if treeId == nullConfineTagId
     then return stack
     else do tree <- loadConfineTag treeId
             fmap ((tree ^. confineTagPrimTagId):) $
                  collectOverhangs (perpendicularTo axis) (tree ^. confineTagLessCut) =<<
                  collectOverhangs (perpendicularTo axis) (tree ^. confineTagMoreCut) =<<
                  return stack

   overhangsCut :: Axis axis
                => axis
                -> Athwart axis s
                -> PrimTagId
                -> TreeMonad s m Bool
   overhangsCut axis cut primTagId =
       do box <- boxOf <$> loadTreePrim primTagId
          return $ box ^. maxBox . athwart axis >= cut

   beyondCut :: Axis axis
             => axis
             -> Athwart axis s
             -> PrimTagId
             -> TreeMonad s m Bool
   beyondCut axis cut primTagId =
       do box <- boxOf <$> loadTreePrim primTagId
          return $ box ^. minBox . athwart axis >= cut

   overhangs :: Axis axis
             => axis
             -> Box s
             -> PrimTagId
             -> TreeMonad s m Bool
   overhangs axis
             boundary
             primTagId =
       do  box <- boxOf <$> loadTreePrim primTagId
           let overhangsAthwart = box ^. maxBox . athwart axis >= boundary ^. maxBox . athwart axis
               overhangsAlong   = box ^. maxBox . along   axis >= boundary ^. maxBox . along   axis
           return $ overhangsAthwart || overhangsAlong


makePath :: (Axis axis, Space s) => axis -> Along axis s -> Athwart axis s -> Along axis s -> (Point2 s, Point2 s)
makePath axis parentCut parentLine cut =
     let start  = pointAlongAxis axis parentCut parentLine
         end    = pointAlongAxis axis cut       parentLine
     in  (start, end)
