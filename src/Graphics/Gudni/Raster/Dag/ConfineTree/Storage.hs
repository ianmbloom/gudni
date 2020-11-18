{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Storage
  ( TreeStorage(..)
  , treeRootPile
  , treeConfinePile
  , treeDecoPile
  , treeCrossingPile
  , initTreeStorage
  , freeTreeStorage
  , storeTree
  , loadTreeRoot
  , loadTreeTag
  , loadDecoTag
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Build

import Graphics.Gudni.Util.StorableM

import Foreign.Storable
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens
import qualified Data.Map as M

data TreeStorage s
    = TreeStorage
      -- | A list of every confineTree
    { _treeRootPile     :: Pile (Root s)
    , _treeConfinePile  :: Pile (ConfineTag s)
    , _treeDecoPile     :: Pile (DecoTag s)
    , _treeCrossingPile :: Pile ShapeId
    -- This is here temporarily.
    -- , _confineTreeMap   :: M.Map (Reference (ConfineTag s)) (ConfineTree  s)
    -- , _decoTreeMap      :: M.Map (Reference (DecoTag    s)) (DecorateTree s)
    }
makeLenses ''TreeStorage

initTreeStorage :: ( MonadIO m
                   , Storable s
                   )
                => m (TreeStorage s)
initTreeStorage =
    do rootPile     <- newPile
       confinePile  <- newPile
       decoPile     <- newPile
       crossingPile <- newPile
       return $ TreeStorage rootPile
                            confinePile
                            decoPile
                            crossingPile

freeTreeStorage :: MonadIO m => TreeStorage s -> m ()
freeTreeStorage storage =
    do freePile (storage ^. treeRootPile    )
       freePile (storage ^. treeConfinePile )
       freePile (storage ^. treeDecoPile    )
       freePile (storage ^. treeCrossingPile)

loadTreeRoot :: (Storable s, MonadIO m) => Reference (Root s) -> StateT (TreeStorage s) m (Root       s)
loadTreeTag  :: (Storable s, MonadIO m) => ConfineTagId    s  -> StateT (TreeStorage s) m (ConfineTag s)
loadDecoTag  :: (Storable s, MonadIO m) => DecoTagId       s  -> StateT (TreeStorage s) m (DecoTag    s)
loadTreeRoot treeId = fromPileS treeRootPile                     $ treeId
loadTreeTag  treeId = fromPileS treeConfinePile . unConfineTagId $ treeId
loadDecoTag  treeId = fromPileS treeDecoPile    . unDecoTagId    $ treeId

storeTree :: (MonadIO m, Space s, Storable s)
          => (PrimTagId -> m (Box s))
          -> (PrimTagId -> m (Primitive s))
          -> Slice PrimTagId
          -> Pile PrimTagId
          -> StateT (TreeStorage s) m (Reference (Root s))
storeTree getBox getPrim slice primTagPile =
    do (confineTree, decoTree, _) <- lift $ buildConfineTree getBox getPrim True 0 0 slice primTagPile
       -- store tree in confinePile and decoPile
       confineRoot <- storeConfineTree confineTree
       decoRoot    <- storeDecoTree decoTree
       root        <- addToPileS treeRootPile (confineRoot, decoRoot)
       -- currently storing the tree twice with this map for haskell side compatibility
       --confineTreeMap %= M.insert confineRoot confineTree
       --decoTreeMap    %= M.insert decoRoot    decoTree)
       return root

storeConfineTree :: forall m s
                 .  ( Storable s
                    , MonadIO m
                    )
                 => ConfineTree s
                 -> StateT (TreeStorage s) m (ConfineTagId s)
storeConfineTree = go Vertical
    where
    go :: (Axis axis) => axis -> Maybe (Confine axis s) -> StateT (TreeStorage s) m (ConfineTagId s)
    go axis mTree =
      case mTree of
        Just tree -> do less <- go (perpendicularTo axis) (tree ^. confineLessCut)
                        more <- go (perpendicularTo axis) (tree ^. confineMoreCut)
                        let treeTag = ConfineTag { _confineTagPrimTagId = tree ^. confinePrimTagId
                                                 , _confineTagCut       = fromAthwart axis (tree ^. confineCut     )
                                                 , _confineTagOverhang  = fromAthwart axis (tree ^. confineOverhang)
                                                 , _confineTagLessCut   = less
                                                 , _confineTagMoreCut   = more
                                                 }
                        thisId <- ConfineTagId <$> addToPileS treeConfinePile treeTag
                        return thisId
        Nothing -> return nullConfineTagId

storeDecoTree :: forall m s
              .  ( Storable s
                 , MonadIO m
                 )
              => DecorateTree s
              -> StateT (TreeStorage s) m (DecoTagId s)
storeDecoTree = go Vertical
    where
    go :: forall axis . (Axis axis) => axis -> DecoTree axis s -> StateT (TreeStorage s) m (DecoTagId s)
    go axis tree =
      case tree of
          DecoBranch {} -> do less <- go (perpendicularTo axis) (tree ^?! decoLessCut)
                              more <- go (perpendicularTo axis) (tree ^?! decoMoreCut)
                              slice <- foldIntoPileS treeCrossingPile (tree ^?! decoCrossings)
                              let treeTag = DecoTag { _decoTagCut       = fromAthwart axis (tree ^?! decoCut)
                                                    , _decoTagCrossings = slice
                                                    , _decoTagLessCut   = less
                                                    , _decoTagMoreCut   = more
                                                    }
                              thisId <- DecoTagId <$> addToPileS treeDecoPile treeTag
                              return thisId
          DecoLeaf -> return nullDecoTagId
