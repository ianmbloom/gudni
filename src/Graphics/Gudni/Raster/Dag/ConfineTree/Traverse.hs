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

module Graphics.Gudni.Raster.Dag.ConfineTree.Traverse
  ( traverseCTAlong
  , traverseCTBetweenPoints
  , traverseCTBox
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.WithTag

import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
import Control.Monad.State

traverseCTAlong :: forall lineAxis s m
                .  ( Axis lineAxis
                   , Space s
                   , Monad m
                   )
                => (PrimTagId -> m ())
                -> lineAxis
                -> Along   lineAxis s
                -> Athwart lineAxis s
                -> Along   lineAxis s
                -> ConfineTree s
                -> m ()
traverseCTAlong f lineAxis start baseline end =
    traverseCTBox f box
    where
    box :: Box s
    box = set (minBox . athwart lineAxis) baseline .
          set (maxBox . athwart lineAxis) baseline .
          set (minBox . along   lineAxis) (min start end) .
          set (maxBox . along   lineAxis) (max start end) $
          emptyBox


traverseCTBetweenPoints :: forall s a m
                        .  ( Space s
                           , Monad m
                           )
                        => (PrimTagId -> m ())
                        -> Point2 s
                        -> Point2 s
                        -> ConfineTree s
                        -> m ()
traverseCTBetweenPoints f anchor point =
    traverseCTBox f box
    where
    box :: Box s
    box = boxAroundPoints anchor point

traverseCTBox :: forall s m
              .  ( Space s
                 , Monad m
                 )
              => (PrimTagId -> m ())
              -> Box s
              -> ConfineTree s
              -> m ()
traverseCTBox f box mTree =
    go Vertical mTree
    where
    go :: (Axis axis)
       => axis
       -> Branch axis s
       -> m ()
    go axis mTree =
      case mTree of
        Nothing -> return ()
        Just tree ->
            do moreCut      axis (box ^. maxBox . athwart axis) tree (go (perpendicularTo axis))
               lessOverhang axis (box ^. minBox . athwart axis) tree (go (perpendicularTo axis))
               f (tree ^. confinePrimTagId)

    moreCut :: ( Axis axis
               , Space s
               , Monad m
               )
            => axis
            -> Athwart axis s
            -> Confine axis s
            -> (Branch (PerpendicularTo axis) s -> m ())
            -> m ()
    moreCut axis s tree goNext =
        if s > tree ^. confineCut
        then goNext (tree ^. confineMoreCut)
        else return ()

    lessOverhang :: ( Axis axis
                    , Space s
                    , Monad m
                    )
                 => axis
                 -> Athwart axis s
                 -> Confine axis s
                 -> (Branch (PerpendicularTo axis) s -> m ())
                 -> m ()
    lessOverhang axis s tree goNext =
        if s <= tree ^. confineOverhang
        then goNext (tree ^. confineLessCut)
        else return ()
