{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Graphics.Gudni.Image.Type
  ( IsImage(..)
  , IsBoundedImage(..)
  , ($~)
  , MutableImage(..)
  , getPixel
  , FrozenImage(..)
  , newMutableFromGrid
  , newMutableImage
  , setPixelM
  , getPixelM
  , mutableToImage
  , imageToMutable
  , mapOverMutable
  , createImageM
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Image.Fast2D
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.MonadST

import Control.Applicative
import Control.Monad.ST
import Control.Monad.IO.Class
import Control.Lens
import Control.Loop

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.Maybe

class IsImage img where
  type PixelType img
  pixelAt :: img -> Location -> PixelType img

class IsBoundedImage img where
  imageSize :: img -> BoxSize

($~) :: IsImage img => img -> Location -> PixelType img
($~) = pixelAt

data MutableImage s a = MutableImage (V.MVector s a) Grid2D BoxSize

instance (V.Storable a) => IsImage (MutableImage s a) where
  type PixelType (MutableImage s a) = ST s a
  pixelAt img location = getPixelM location img

instance IsBoundedImage (MutableImage s a) where
  imageSize (MutableImage _ _ size) = size

getPixel :: (V.Storable a)
         => Location
         -> FrozenImage a
         -> a
getPixel loc (FrozenImage table grid size) = (V.!) table (locationToIndex grid loc)

data FrozenImage a = FrozenImage (V.Vector a) Grid2D BoxSize

instance (V.Storable a) => IsImage (FrozenImage a) where
  type PixelType (FrozenImage a) = a
  pixelAt img location = getPixel location img

newMutableFromGrid :: forall m a
                   .  ( V.Storable a
                      , MonadST m
                      , Monad m
                      )
                   => Grid2D
                   -> BoxSize
                   -> a
                   -> m (MutableImage (StateThread m) a)
newMutableFromGrid grid imgSize initialValue =
  do table <- liftST $ MV.replicate (gridAllocation grid) initialValue
     return $ MutableImage table grid imgSize

newMutableImage :: forall m a
                .  ( V.Storable a
                   , MonadST m
                   , Monad m
                   )
                => BoxSize
                -> a
                -> m (MutableImage (StateThread m) a)
newMutableImage imgSize initialValue =
  let grid = newGrid2D imgSize
  in  newMutableFromGrid grid imgSize initialValue

setPixelM :: forall m s a
          .  ( V.Storable a
             , MonadST m
             , Monad m
             )
          => Location
          -> a
          -> MutableImage (StateThread m) a
          -> m ()
setPixelM loc val (MutableImage table grid imgSize) =
   liftST $ MV.write table (locationToIndex grid loc) val

getPixelM :: forall m a
          .  ( V.Storable a
             , MonadST m
             , Monad m
             )
          => Location
          -> MutableImage (StateThread m) a
          -> m a
getPixelM loc (MutableImage table grid imgSize) =
  liftST $ MV.read table (locationToIndex grid loc)

mutableToImage :: forall m a
               .  ( V.Storable a
                  , MonadST m
                  , Monad m
                  )
               => MutableImage (StateThread m) a
               -> m (FrozenImage a)
mutableToImage (MutableImage table grid size) =
  do frozen <- liftST $ V.freeze table
     return $ FrozenImage frozen grid size

imageToMutable :: forall m a
               .  ( V.Storable a
                  , MonadST m
                  , Monad m
                  )
               => FrozenImage a
               -> m (MutableImage (StateThread m) a)
imageToMutable (FrozenImage table grid size) =
  do tableThawed <- liftST $ V.thaw table
     return $ MutableImage tableThawed grid size

mapOverMutable :: forall m a
               .  ( V.Storable a
                  , MonadST m
                  , Monad m
                  )
               => (Location -> m a)
               -> MutableImage (StateThread m) a
               -> m (MutableImage (StateThread m) a)
mapOverMutable f image@(MutableImage tableThawed grid size) =
    do numLoop 0 (size ^. pY - 1) $ \ y ->
           numLoop 0 (size ^. pX - 1) $ \ x ->
               do let loc = Loc (fromAlong Horizontal x) (fromAlong Vertical y)
                  a <- f loc
                  setPixelM loc a image
       return image

createImageM :: forall m a
             .  ( V.Storable a
                , HasDefault a
                , MonadST m
                , Monad m
                )
             => (Location -> m a)
             -> BoxSize
             -> m (FrozenImage a)
createImageM f size =
  do fresh <- newMutableImage size defaultValue
     image <- mapOverMutable f fresh
     mutableToImage image
