{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Gudni.Raster.Dag.TreeStorage
  ( TreeStorage(..)
  , initTreeStorage
  , freeTreeStorage
  , storeTree
  , loadTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile
import Graphics.Gudni.Raster.Dag.Primitive
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.PrimTag
import Graphics.Gudni.Raster.Dag.SubstanceTag
import Graphics.Gudni.Raster.Dag.FabricTag
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.Build

import Foreign.Storable
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens
import qualified Data.Map as M

data TreeStorage s
    = TreeStorage
      -- | A list of every confineTree
    { _treeLastId :: ConfineTreeId
    , _treeMap :: M.Map ConfineTreeId (ConfineTree s, DecorateTree s)
    }
makeLenses ''TreeStorage

initTreeStorage :: MonadIO m => m (TreeStorage s)
initTreeStorage = return $ TreeStorage 0 M.empty

freeTreeStorage :: MonadIO m => TreeStorage s -> m ()
freeTreeStorage treeStorage = return ()

storeTree :: (MonadIO m, Space s, Storable s)
          => (PrimTagId -> m (Box s))
          -> (PrimTagId -> m (Bezier s))
          -> Slice PrimTagId
          -> Pile PrimTagId
          -> StateT (TreeStorage s) m ConfineTreeId
storeTree getBox getCurve slice primTagPile =
    do (confineTree, decoTree, _) <- lift $ buildConfineTree getBox getCurve True 0 0 slice primTagPile
       treeId <- use treeLastId
       treeMap %= M.insert treeId (confineTree, decoTree)
       treeLastId += 1
       return treeId

loadTree :: (MonadIO m, Show s, Storable s)
         => ConfineTreeId
         -> StateT (TreeStorage s) m (ConfineTree s, DecorateTree s)
loadTree treeId =
    do mapping <- use treeMap
       return $ (M.!) mapping treeId
