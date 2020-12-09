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

module Graphics.Gudni.Raster.Dag.ConfineTree.Depth
  ( confineTreeDepth
  , confineTreeSize
  , confineTreeCountOverlaps
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage

import Control.Lens

confineTreeDepth :: forall s m . (TreeConstraints s m) => ConfineTagId s -> TreeMonad s m Int
confineTreeDepth = go Vertical
  where
  go :: (Axis axis) => axis -> ConfineTagId s -> TreeMonad s m Int
  go axis treeId =
     if treeId == nullConfineTagId
     then return 0
     else do tree <- loadConfineTag treeId
             less <- go (perpendicularTo axis) (tree ^. confineTagLessCut)
             more <- go (perpendicularTo axis) (tree ^. confineTagMoreCut)
             return $ 1 + max less more

confineTreeSize :: forall s m . (TreeConstraints s m) => ConfineTagId s -> TreeMonad s m Int
confineTreeSize = go Vertical
  where
  go :: (Axis axis) => axis -> ConfineTagId s -> TreeMonad s m Int
  go axis treeId =
     if treeId == nullConfineTagId
     then return 0
     else do tree <- loadConfineTag treeId
             less <- go (perpendicularTo axis) (tree ^. confineTagLessCut)
             more <- go (perpendicularTo axis) (tree ^. confineTagMoreCut)
             return $ 1 + less + more

confineTreeCountOverlaps :: forall s m . (TreeConstraints s m) => ConfineTagId s -> TreeMonad s m Int
confineTreeCountOverlaps = go Vertical
  where
  go :: (Axis axis) => axis -> ConfineTagId s -> TreeMonad s m Int
  go axis treeId =
     if treeId == nullConfineTagId
     then return 0
     else do tree <- loadConfineTag treeId
             less <- go (perpendicularTo axis) (tree ^. confineTagLessCut)
             more <- go (perpendicularTo axis) (tree ^. confineTagMoreCut)
             let x = if tree ^. confineTagOverhang > tree ^. confineTagCut then 1 else 0
             return $ x + less + more
