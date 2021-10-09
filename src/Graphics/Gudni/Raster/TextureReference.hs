{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.TextureReference
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions used by TraverseShapeTree to extract named textures from the tree and assign them to memory references.

module Graphics.Gudni.Raster.TextureReference
  ( PixelPile(..)
  , PictMemId(..)
  , PictUsageId(..)
  , PictureMemoryReference(..)
  , PictureMemoryMap(..)
  , NamedTexture(..)
  , makePictureMap
  , collectPictureMemory
  , getPixelColor
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.StorableInstances
<<<<<<< HEAD
import Graphics.Gudni.Layout.FromLayout
=======
>>>>>>> origin/flatpath

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile
import Graphics.Gudni.Raster.Serial.CopyPile

import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Debug

import Foreign.C.Types
import Codec.Picture

import Data.List
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M
import qualified Data.Foldable as Foldable
import Data.Traversable

import Control.Lens
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Lens.Tuple
import Control.Monad.ListM
import Linear.V4
import Data.Word

type PixelPile   = Pile CFloat
type PictMemId   = CUInt
type PictUsageId = CUInt
type MemOffset_  = Reference CFloat

type PictureMemoryMap = M.Map PictureName PictureMemoryReference
type PictureIdMap     = M.Map PictureName PictMemId

data NamedTexture
  = NewTexture PictureName Picture
  | SharedTexture PictureName

-- | The starting memory offset and size of a picture.
data PictureMemoryReference = PictureMemory
  { -- | The dimensions of the picture in memory.
    pictSize      :: Point2 PixelSpace
    -- | The offset of the picture data within the combined buffer of all source pictures.
  , pictMemOffset :: MemOffset_
  } deriving (Show, Generic)

instance Out PictureMemoryReference

instance (Storable a) => CanLoad Word8 (AsBytes a) where
    fromPile pile index = AsBytes    <$> (liftIO $ peek (castPtr $ ptrFromIndex pile index (undefined :: Word8)))
    toPile   pile index  (AsBytes item) = liftIO $ poke (castPtr $ ptrFromIndex pile index (undefined :: Word8)) item

instance (Storable a) => CanLoad CFloat (AsBytes a) where
    fromPile pile index = AsBytes    <$> (liftIO $ peek (castPtr $ ptrFromIndex pile index (undefined :: CFloat)))
    toPile   pile index  (AsBytes item) = liftIO $ poke (castPtr $ ptrFromIndex pile index (undefined :: CFloat)) item

getPixelColor :: Space s => MonadIO m => PixelPile -> PictureMemoryReference -> Point2 PixelSpace -> m (Color s)
getPixelColor pixelPile (PictureMemory (Point2 w h) offset) (Point2 x y) =
    do  let pos = Ref ((fromIntegral $ unPSpace (y * w + x)) * (fromIntegral $ sizeOf (undefined :: V4 CFloat))) :: Reference CFloat
        (AsBytes (colorWord8 :: V4 CFloat)) <- liftIO $ fromPile pixelPile pos
        return . Color . fmap realToFrac $ colorWord8

nameTexture :: NamedTexture -> State PictureMap PictureName
nameTexture namedTexture =
    case namedTexture of
        NewTexture name image ->
           do modify (M.insert name image)
              return name
        SharedTexture name -> return name

accumulatePicture :: Picture
                  -> StateT PixelPile IO PictureMemoryReference
accumulatePicture picture =
  do pictPile <- get
     let size    = pictureSize picture
         memory  = PictureMemory size (pictPile ^. pileCursor)
         pVector = pictureData picture
     (pictPile', _) <- liftIO $ copyIntoPile pictPile pVector
     put pictPile'
     return memory

collectPictureMemory :: PictureMap -> IO (PictureMemoryMap, PixelPile)
collectPictureMemory mapping =
  do pictPile <- newPile
     runStateT (mapM accumulatePicture mapping) pictPile

instance StorableM PictureMemoryReference where
  sizeOfM _ =
    do sizeOfM (undefined :: Point2 PixelSpace)
       sizeOfM (undefined :: MemOffset_)
       sizeOfM (undefined :: CUInt     ) -- padding
  alignmentM _ =
    do alignmentM (undefined :: Point2 PixelSpace)
       alignmentM (undefined :: MemOffset_)
       alignmentM (undefined :: CUInt     ) -- padding
  peekM = do size      <- peekM
             memOffset <- peekM
             return (PictureMemory size memOffset)
  pokeM (PictureMemory size memOffset) =
    do  pokeM size
        pokeM memOffset

instance Storable PictureMemoryReference where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
