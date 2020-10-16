{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Util.Pile
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A pile is an appendable buffer of data that reallocates memory as it grows∘
-- Piles can be reused without reallocating memory.

module Graphics.Gudni.Raster.Serial.Pile
  ( Pile (..)
  , CanPile (..)
  , allocateItemPile
  , foldIntoPile
  , CanLoad (..)
  , pileCursor

  , ptrFromIndex
  , addToPileS
  , foldIntoPileS
  , allocatePileS
  , toPileS
  , fromPileS
  , newPile
  , newPileSize
  , freePile
  , resetPile
  , isEmptyPile
  , extendPile

  , pileToList
  , listToPile
  )
where

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice


import Control.Monad
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Lens

import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array(peekArray)

import GHC.Ptr

-- Additional amount to reallocate with each step.
dEFAULTpILEaLLOCATION = 1024 :: Reference_

-- | A pile is an appendable memory buffer that reallocates memory as it grows.
data Pile t = Pile
  { -- The current position of the write head.
    _pileCursor    :: !Reference_
    -- The current total allocated units of t for the buffer.
  , _pileAllocated :: !Reference_
    -- The pointer to the start of the buffer.
  , _pileData      :: !(Ptr t)
  } deriving (Show)

-- | Lens for the current position of the write head.
pileCursor :: Lens' (Pile t) (Reference t)
pileCursor plFunc (Pile cursor allocated ptr) = (\(Ref cursor') -> Pile cursor' allocated ptr) <$> plFunc (Ref cursor)

-- | Lens for the current allocation size of the pile.
pileAllocated :: Lens' (Pile t) Reference_
pileAllocated plFunc (Pile cursor allocated ptr) = (\ allocated' -> Pile cursor allocated' ptr) <$> plFunc allocated

newPile :: forall t m . (Storable t, MonadIO m) => m (Pile t)
newPile = newPileSize dEFAULTpILEaLLOCATION

-- | Create a new pile.
newPileSize :: forall t m . (Storable t, MonadIO m) => Reference_ -> m (Pile t)
newPileSize allocated =
  do
    ptr <- liftIO $ mallocBytes (fromIntegral allocated * sizeOf (undefined :: t))
    return (Pile 0 allocated ptr)

-- | Free the memory allocated by a pile.
freePile :: MonadIO m => Pile a -> m ()
freePile (Pile _ _ ptr) = liftIO $ free ptr

-- | Reset the start point of a tile as though it were new.
resetPile :: Pile t -> Pile t
resetPile (Pile _ allocated ptr) = Pile 0 allocated ptr

-- | Return true if a pile has any items.
isEmptyPile :: Pile t -> Bool
isEmptyPile (Pile c _ _ ) = c == 0

-- | Reallocate the pile with new space.
extendPile :: forall t m . (Storable t, MonadIO m) => Pile t -> m (Pile t)
extendPile pile@(Pile cursor allocated startPtr) =
  do
    let itemSize = sizeOf (undefined :: t)
        newSize  = allocated * 2
    newPtr <- liftIO $ reallocBytes startPtr (fromIntegral newSize * itemSize)
    return $ Pile cursor newSize newPtr

addToPile' :: forall t m . (Storable t, MonadIO m) => Pile t -> t -> m (Pile t)
addToPile' pile@(Pile cursor allocated startPtr) item =
  do liftIO $ poke (startPtr `plusPtr` (fromIntegral cursor * sizeOf (undefined :: t))) item
     allocateItemPile' pile item

allocateItemPile' :: forall t m . (Storable t, MonadIO m) => Pile t -> t -> m (Pile t)
allocateItemPile' pile item =
   if ((unRef $ pile ^. pileCursor) + 1 ) < pile ^. pileAllocated
   then return $ over pileCursor (+1) pile
   else
     do e <- extendPile pile
        allocateItemPile' e item

allocateItemPile :: forall t m . (Storable t, MonadIO m) => Pile t -> m (Reference t, Pile t)
allocateItemPile pile =
  do let ref = Ref . fromIntegral $ pile ^. pileCursor
     pile' <- allocateItemPile' pile (undefined :: t)
     return (ref, pile')

class (Storable a) => CanPile a b where
  addToPile :: MonadIO m => Pile a -> b -> m (Pile a, Reference a)

class (Storable a) => CanLoad a b where
  fromPile  :: MonadIO m => Pile a -> Reference a -> m b
  toPile    :: MonadIO m => Pile a -> Reference a -> b -> m ()

instance (Storable a) => CanPile a a where
    addToPile pile item =
      do let ref = Ref . fromIntegral $ pile ^. pileCursor
         pile' <- addToPile' pile item
         return (pile', ref)

foldIntoPile :: (Foldable f, Storable a, MonadIO m) => Pile a -> f a -> m (Pile a, Slice     a)
foldIntoPile pile foldable =
  do let cursor = pile ^. pileCursor
     pile' <- foldM addToPile' pile foldable
     let breadth = pile' ^. pileCursor - cursor
     return (pile', Slice (Ref $ fromIntegral cursor) (Ref $ fromIntegral breadth))


ptrFromIndex :: (Storable b) => Pile a -> Reference a -> b -> Ptr a
ptrFromIndex pile index a =  _pileData pile `plusPtr` ((fromIntegral . unRef $ index) * sizeOf a)

instance (Storable a) => CanLoad a a where
    fromPile pile index      = liftIO $ peek (ptrFromIndex pile index (undefined :: a))
    toPile   pile index item = liftIO $ poke (ptrFromIndex pile index (undefined :: a)) item

-- | Add an item to a pile within a StateT monad transformer with a lens to the pile within the state.
-- updating the state along the way.

allocatePileS :: ( MonadIO m
                 , MonadState s m
                 , Storable t)
              => Lens' s (Pile t)
              -> m (Reference t)
allocatePileS lens =
  do pile <- use lens
     (ref, pile') <- allocateItemPile pile
     lens .= pile'
     return ref

addToPileS :: (Storable a, CanPile a b, MonadState s m, MonadIO m)
               => Lens' s (Pile a)
               -> b
               -> m (Reference a)
addToPileS lens object =
  do pile <- use lens
     (pile', ref) <- addToPile pile object
     lens .= pile'
     return ref

foldIntoPileS :: (Storable a, MonadState s m, MonadIO m, Foldable f)
                       => Lens' s (Pile a)
                       -> f a
                       -> m (Slice a)
foldIntoPileS lens object =
  do pile <- use lens
     (pile', ref) <- foldIntoPile pile object
     lens .= pile'
     return ref

toPileS :: ( MonadIO m
           , MonadState s m
           , Storable t)
        => Lens' s (Pile t)
        -> Reference t
        -> t
        -> m ()
toPileS lens ref item =
    do pile <- use lens
       toPile pile ref item

fromPileS :: ( Storable a
             , CanLoad a b
             , MonadState s m
             , MonadIO m
             )
          => Lens' s (Pile a)
          -> Reference a
          -> m b
fromPileS lens ref =
  do pile <- use lens
     fromPile pile ref

-- | Convert a pile to a list.
pileToList :: forall t m . (Storable t, MonadIO m) => Pile t -> m [t]
pileToList (Pile cursor allocated startPtr) = liftIO $ peekArray (fromIntegral cursor) startPtr

-- | Convert a list to a pile.
listToPile :: forall t m . (Storable t, MonadIO m) => [t] -> m (Pile t)
listToPile list = do pile <- liftIO $ newPile
                     (pile', _) <- foldIntoPile pile list
                     return pile'
