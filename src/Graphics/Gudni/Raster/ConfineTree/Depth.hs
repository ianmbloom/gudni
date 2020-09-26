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

module Graphics.Gudni.Raster.ConfineTree.Depth
  ( confineTreeDepth
  , confineTreeSize
  , confineTreeCountOverlaps
  )
where

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Raster.ConfineTree.Type
import Control.Lens

confineTreeDepth :: ConfineTree s -> Int
confineTreeDepth = go Vertical
  where
  go :: (Axis axis) => axis -> Maybe (Confine axis s) -> Int
  go axis mTree =
     case mTree of
       Nothing -> 0
       Just tree ->
         1 + max (go (nextAxis axis) (tree ^. confineLessCut))
                 (go (nextAxis axis) (tree ^. confineMoreCut))

confineTreeSize :: ConfineTree s -> Int
confineTreeSize = go Vertical
  where
  go :: (Axis axis) => axis -> Maybe (Confine axis s) -> Int
  go axis mTree =
     case mTree of
       Nothing -> 0
       Just tree ->
         1
         + (go (nextAxis axis) (tree ^. confineLessCut))
         + (go (nextAxis axis) (tree ^. confineMoreCut))

confineTreeCountOverlaps :: forall s . Space s => ConfineTree s -> Int
confineTreeCountOverlaps = go Vertical
  where
  go :: (Axis axis) => axis -> Maybe (Confine axis s) -> Int
  go axis mTree =
     case mTree of
       Nothing -> 0
       Just tree ->
         let x = if tree ^. confineOverhang > tree ^. confineCut then 1 else 0
         in
         x
         + (go (nextAxis axis) (tree ^. confineLessCut))
         + (go (nextAxis axis) (tree ^. confineMoreCut))
