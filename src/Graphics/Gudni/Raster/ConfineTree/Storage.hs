{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Graphics.Gudni.Raster.ConfineTree.Storage
  ( TreeStorage(..)
  , TreeMonad(..)
  , TreeConstraints(..)
  , treeConfinePile
  , treeDecoPile
  , treeCrossingPile
  , treePrimStorage
  , initTreeStorage
  , freeTreeStorage

  , addConfineTag
  , overwriteConfineTag
  , loadConfineTag

  , addDecoTag
  , loadDecoTag

  , addTreePrim
  , loadTreePrim
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Storage
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.TagTypes

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.Util

import Foreign.Storable
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

data TreeStorage s
    = TreeStorage
      -- | A list of every confineTree node
    { _treeConfinePile  :: Pile (ConfineTag s)
      -- | A list of every decoTree node
    , _treeDecoPile     :: Pile (DecoTag s)
      -- | A pile of every fabricTagId used in a decoCrossing
    , _treeCrossingPile :: Pile FabricTagId
      -- | Piles storing all of the items (beziers, facets and other primitives) that define the scene.
    , _treePrimStorage  :: PrimStorage s
    }
makeLenses ''TreeStorage

type TreeMonad s m = StateT (TreeStorage s) m

type TreeConstraints s m = ( MonadIO m
                           , Space s
                           , Storable s
                           )

initTreeStorage :: ( TreeConstraints s m
                   )
                => m (TreeStorage s)
initTreeStorage =
    do confinePile  <- newPile
       decoPile     <- newPile
       crossingPile <- newPile
       primStorage   <- initPrimStorage
       return $ TreeStorage confinePile
                            decoPile
                            crossingPile
                            primStorage

freeTreeStorage :: (TreeConstraints s m) => TreeStorage s -> m ()
freeTreeStorage storage =
    do freePile (storage ^. treeConfinePile )
       freePile (storage ^. treeDecoPile    )
       freePile (storage ^. treeCrossingPile)
       freePrimStorage (storage ^. treePrimStorage)

addConfineTag       :: (TreeConstraints s m) => ConfineTag s                   -> TreeMonad s m (ConfineTagId s)
overwriteConfineTag :: (TreeConstraints s m) => ConfineTagId s -> ConfineTag s -> TreeMonad s m ()
loadConfineTag      :: (TreeConstraints s m) => ConfineTagId s                 -> TreeMonad s m (ConfineTag s)
addConfineTag                 tag = ConfineTagId <$> addToPileS treeConfinePile tag
overwriteConfineTag confineId tag = toPileS   treeConfinePile (unConfineTagId confineId) tag
loadConfineTag      confineId     = fromPileS treeConfinePile . unConfineTagId $ confineId

addDecoTag  :: (TreeConstraints s m) => DecoTag s                      -> TreeMonad s m (DecoTagId s)
loadDecoTag :: (TreeConstraints s m) => DecoTagId    s  -> TreeMonad s m (DecoTag    s)
addDecoTag  tag    = DecoTagId <$> addToPileS treeDecoPile   tag
loadDecoTag decoId =               fromPileS  treeDecoPile . unDecoTagId $ decoId

addTreePrim  :: (TreeConstraints s m) => Primitive s -> TreeMonad s m PrimTagId
loadTreePrim :: (TreeConstraints s m) => PrimTagId   -> TreeMonad s m (Primitive s)
addTreePrim  prim      = overStateT treePrimStorage $ storePrim prim
loadTreePrim primTagId = overStateT treePrimStorage $ loadPrim  primTagId
