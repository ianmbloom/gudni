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
{-# LANGUAGE DoAndIfThenElse            #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Traverse
  ( traverseCTAlong
  , traverseCTagBetweenPoints
  , traverseCTagBox
  , traverseDecorateTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.TagTypes

import Graphics.Gudni.Raster.Serial.Slice

import Control.Lens
import Control.Monad
import Control.Monad.State

traverseCTAlong :: forall lineAxis s m f
                  .  ( Axis lineAxis
                     , TreeConstraints s m
                     , MonadTrans f
                     , Monad (f (TreeMonad s m))
                     )
                  => (PrimTagId -> f (TreeMonad s m) ())
                  -> lineAxis
                  -> Along   lineAxis s
                  -> Athwart lineAxis s
                  -> Along   lineAxis s
                  -> ConfineTagId s
                  -> f (TreeMonad s m) ()
traverseCTAlong f lineAxis start baseline end =
    traverseCTagBox f box
    where
    box :: Box s
    box = set (minBox . athwart lineAxis) baseline .
          set (maxBox . athwart lineAxis) baseline .
          set (minBox . along   lineAxis) (min start end) .
          set (maxBox . along   lineAxis) (max start end) $
          emptyBox


traverseCTagBetweenPoints :: forall s m f
                          .  ( TreeConstraints s m
                             , MonadTrans f
                             , Monad (f (TreeMonad s m))
                             )
                          => (PrimTagId -> f (TreeMonad s m) ())
                          -> Point2 s
                          -> Point2 s
                          -> ConfineTagId s
                          -> f (TreeMonad s m) ()
traverseCTagBetweenPoints f anchor point =
    traverseCTagBox f box
    where
    box :: Box s
    box = boxAroundPoints anchor point

traverseCTagBox :: forall s m f
                .  ( TreeConstraints s m
                   , MonadTrans f
                   , Monad (f (TreeMonad s m))
                   )
                => (PrimTagId -> f (TreeMonad s m) ())
                -> Box s
                -> ConfineTagId s
                -> f (TreeMonad s m) ()
traverseCTagBox f box tree =
    go Vertical tree
    where
    go :: (Axis axis)
       => axis
       -> ConfineTagId s
       -> f (TreeMonad s m) ()
    go axis treeId =
      if treeId == nullConfineTagId
      then return ()
      else do tree <- lift $ loadConfineTag treeId
              moreCut      axis (box ^. maxBox . athwart axis) tree (go (perpendicularTo axis))
              lessOverhang axis (box ^. minBox . athwart axis) tree (go (perpendicularTo axis))
              f (tree ^. confineTagPrimTagId)

    moreCut :: ( Axis axis
               )
            => axis
            -> Athwart axis s
            -> ConfineTag s
            -> (ConfineTagId s -> f (TreeMonad s m) ())
            -> f (TreeMonad s m) ()
    moreCut axis s tree goNext =
        if s > toAthwart axis (tree ^. confineTagCut)
        then goNext (tree ^. confineTagMoreCut)
        else return ()

    lessOverhang :: ( Axis axis
                    )
                 => axis
                 -> Athwart axis s
                 -> ConfineTag s
                 -> (ConfineTagId s -> f (TreeMonad s m) ())
                 -> f (TreeMonad s m) ()
    lessOverhang axis s tree goNext =
        if s <= toAthwart axis (tree ^. confineTagOverhang)
        then goNext (tree ^. confineTagLessCut)
        else return ()

traverseDecorateTree :: forall s m f
                     .  ( TreeConstraints s m
                        , MonadTrans f
                        , MonadIO (f (TreeMonad s m))
                        )
                     => (Slice FabricTagId -> f (TreeMonad s m) ())
                     -> (Point2 s          -> f (TreeMonad s m) ())
                     -> Point2 s
                     -> DecoTagId s
                     -> f (TreeMonad s m) ()
traverseDecorateTree doBranch doLeaf point =
  go Vertical (toAlong Horizontal minBound) (toAlong Vertical minBound)
  where
  go :: (Axis axis, axis~PerpendicularTo(PerpendicularTo axis))
     => axis
     -> Athwart axis s
     -> Along   axis s
     -> DecoTagId s
     -> f (TreeMonad s m) ()
  go axis parentCut parentLine treeId =
      if treeId == nullDecoTagId
      then let anchor = pointAlongAxis (perpendicularTo axis) parentCut parentLine
           in  doLeaf anchor
      else do tree <- lift $ loadDecoTag treeId
              let cut = toAthwart axis (tree ^. decoTagCut)
              doBranch (tree ^. decoTagCrossings)
              if point ^. athwart axis < cut
              then go (perpendicularTo axis) parentLine cut (tree ^. decoTagLessCut)
              else go (perpendicularTo axis) parentLine cut (tree ^. decoTagMoreCut)
