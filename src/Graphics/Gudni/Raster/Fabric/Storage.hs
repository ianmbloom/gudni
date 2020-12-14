{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Raster.Fabric.Storage
  ( FabricStorage(..)
  , FabricStorageMonad(..)
  , fabricTagPile
  , fabricHeapPile
  , initFabricStorage
  , freeFabricStorage
  , addFabricTag
  , loadFabricTag
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile
import Graphics.Gudni.Raster.TagTypes
--  import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
--  import Graphics.Gudni.Raster.ConfineTree.Primitive.Tag
--  import Graphics.Gudni.Raster.Fabric.Filter.Type
--  import Graphics.Gudni.Raster.Fabric.Transformer.Type
--  import Graphics.Gudni.Raster.Fabric.Combine.Type
--  import Graphics.Gudni.Raster.Fabric.Combine.Tag
--  import Graphics.Gudni.Raster.Fabric.Substance.Type
--  import Graphics.Gudni.Raster.Fabric.Substance.Storage
--  import Graphics.Gudni.Raster.Fabric.Type
--  import Graphics.Gudni.Raster.Fabric.Tag
--  import Graphics.Gudni.Raster.TextureReference
import Graphics.Gudni.Util.Util

import Foreign.Storable
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

type FabricStorageMonad s m = StateT (FabricStorage s) m

data FabricStorage s
    = FabricStorage
      -- | A list of every tag defining the fabric (or structure of the scene DAG)
    { _fabricTagPile  :: Pile FabricTag
      -- | A heap of all extra data used by the dag including transformations and boundaries
    , _fabricHeapPile :: BytePile
    }
makeLenses ''FabricStorage

initFabricStorage :: MonadIO m => m (FabricStorage s)
initFabricStorage =
   do fabricTagPile    <- newPile
      fabricHeapPile   <- newPile
      return $ FabricStorage
               { _fabricTagPile    = fabricTagPile
               , _fabricHeapPile   = fabricHeapPile
               }

freeFabricStorage :: MonadIO m => FabricStorage s -> m ()
freeFabricStorage storage =
  do freePile $ storage ^. fabricTagPile
     freePile $ storage ^. fabricHeapPile

addFabricTag :: MonadIO m => FabricTag -> FabricStorageMonad s m FabricTagId
addFabricTag tag = FabricTagId <$> addToPileS fabricTagPile tag

loadFabricTag :: MonadIO m => FabricTagId -> FabricStorageMonad s m FabricTag
loadFabricTag fabricTagId = fromPileS fabricTagPile (unFabricTagId fabricTagId)
