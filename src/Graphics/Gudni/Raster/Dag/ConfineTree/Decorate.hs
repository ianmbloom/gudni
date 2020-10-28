{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module Graphics.Gudni.Raster.Dag.ConfineTree.Decorate
  ( buildDecorateTree
  , traverseDecorateTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.ConfineTree.Traverse
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Cross
import Graphics.Gudni.Raster.Dag.Primitive.Stack
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
import Control.Monad.State

modifyItemStackIfCrossedAlong :: ( Axis axis
                                 , Space s
                                 , Monad m
                                 )
                              => m ()
                              -> axis
                              -> Along axis s
                              -> Athwart axis s
                              -> Along axis s
                              -> PrimTagId
                              -> Primitive s
                              -> StateT ShapeStack m ()
modifyItemStackIfCrossedAlong crossOp lineAxis start baseline end primTagId prim =
  do lift crossOp
     if crossesPrimAlong lineAxis start baseline end prim
     then modify (toggleItem (prim ^. primShapeId))
     else return ()

buildDecorateTree :: forall s m
                  .  ( Space s
                     , Monad m
                     )
                  => (PrimTagId -> m (Primitive s))
                  -> m ()
                  -> Int
                  -> ConfineTree s
                  -> m (DecorateTree s)
buildDecorateTree getPrim crossOp limit mRoot =
    fst <$> go Vertical (toAlong Horizontal minBound) (toAlong Vertical minBound) mRoot
    where
    go :: (Axis axis, axis~PerpendicularTo(PerpendicularTo(axis)))
       => axis
       -> Athwart axis s
       -> Along   axis s
       -> Branch axis s
       -> m (DecoTree axis s, Int)
    go axis parentCut parentLine mTree =
        case mTree of
            Nothing -> return (DecoLeaf, 0)
            Just tree ->
                     let parentAxis = perpendicularTo axis
                         cut = tree ^. confineCut
                         collector primTagId =
                              do prim <- lift $ getPrim primTagId
                                 modifyItemStackIfCrossedAlong crossOp parentAxis parentCut parentLine cut primTagId prim
                     in
                     do  crossings <- execStateT (traverseCTAlong collector
                                                                  parentAxis
                                                                  parentCut
                                                                  parentLine
                                                                  cut
                                                                  mRoot
                                                 ) []
                         (lessTree, lessDepth) <- go (perpendicularTo axis) parentLine (tree ^. confineCut) (tree ^. confineLessCut)
                         (moreTree, moreDepth) <- go (perpendicularTo axis) parentLine (tree ^. confineCut) (tree ^. confineMoreCut)
                         let depth = 1 + max lessDepth moreDepth
                             this  = if depth <= limit
                                     then DecoLeaf
                                     else DecoBranch
                                              { _decoCut       = tree ^. confineCut
                                              , _decoCrossings = crossings
                                              , _decoLessCut   = lessTree
                                              , _decoMoreCut   = moreTree
                                              }
                         return (this, depth)

traverseDecorateTree :: forall s m
                     .  ( Space s
                        , Monad m
                        )
                     => (ShapeStack -> m ())
                     -> (Point2 s -> m ())
                     -> Point2 s
                     -> DecorateTree s
                     -> m ()
traverseDecorateTree doBranch doLeaf point =
  go Vertical (toAlong Horizontal minBound) (toAlong Vertical minBound)
  where
  go :: (Axis axis, axis~PerpendicularTo(PerpendicularTo axis))
     => axis
     -> Athwart axis s
     -> Along   axis s
     -> DecoTree axis s
     -> m ()
  go axis parentCut parentLine tree =
      case tree of
        DecoLeaf ->
            let anchor = pointAlongAxis (perpendicularTo axis) parentCut parentLine
            in  doLeaf anchor
        DecoBranch {} ->
            let cut = tree ^?! decoCut
            in
            do  doBranch (tree ^?! decoCrossings)
                if point ^. athwart axis < cut
                then go (perpendicularTo axis) parentLine cut (tree ^?! decoLessCut)
                else go (perpendicularTo axis) parentLine cut (tree ^?! decoMoreCut)
