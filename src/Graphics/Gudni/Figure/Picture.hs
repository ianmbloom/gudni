{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Picture
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for importing and storing bitmap data to use as textured substances in shapes.

module Graphics.Gudni.Figure.Picture
  ( PictId(..)
  , PictureMemoryReference(..)
  , PictureUsage(..)
  , makePictures
  )
where

import Foreign.C.Types
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Util (mapAccumM)

import Codec.Picture

import Data.List
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as VS

import Control.DeepSeq

type PictId = CUInt
type MemOffset_ = Reference Word8

-- | The starting memory offset and size of a picture.
data PictureMemoryReference = PictureMemory
  { -- | The dimensions of the picture in memory.
    pictSize      :: Point2 PixelSpace
    -- | The offset of the picture data within the combined buffer of all source pictures.
  , pictMemOffset :: MemOffset_

  } deriving (Show)

instance NFData PictureMemoryReference where
  rnf (PictureMemory a b) = a `deepseq` b `deepseq` ()

-- | A reference to a picture by a particular substance. Currently this just has translation information
-- But it could also include more tranformation information those were implemented for pictures.
data PictureUsage ref = PictureUsage
  { -- | Translation vector for this particular usage.
    pictTranslate :: Point2 PixelSpace
    -- | An identifier for the source picture.
  , pictSource    :: ref
  } deriving (Show)

instance NFData s => NFData (PictureUsage s) where
  rnf (PictureUsage a b) = a `deepseq` b `deepseq` ()

-- | Create a vector of raw bytes and list of picture memory offsets that the rasterizer
-- can use to reference images. (Rickety)
makePictures :: [DynamicImage] -> IO (VS.Vector Word8, [PictureMemoryReference])
makePictures images =
  do let rgba8s  = map convertRGBA8 images
         pictData = VS.concat (map imageData rgba8s)
         imageAllocation img = imageWidth img * imageHeight img * 4
         allocSizes :: [Int]
         allocSizes = map imageAllocation rgba8s
         offsets :: [Int]
         total :: Int
         (total, offsets) = mapAccumL (\ a b -> (a + b, a)) 0 allocSizes
         dimensions   = zipWith Point2 (map (fromIntegral . imageWidth ) rgba8s)
                                       (map (fromIntegral . imageHeight) rgba8s)
         pictMems = zipWith PictureMemory dimensions (map (Ref . fromIntegral) offsets)
     return (pictData, pictMems)

instance StorableM PictureMemoryReference where
  sizeOfM _ =
    do sizeOfM (undefined :: Point2 PixelSpace)
       sizeOfM (undefined :: MemOffset_     )
  alignmentM _ =
    do alignmentM (undefined :: Point2 PixelSpace)
       alignmentM (undefined :: MemOffset_     )
  peekM = do size      <- peekM
             memOffset <- peekM
             return (PictureMemory size memOffset)
  pokeM (PictureMemory size memOffset) =
    do  pokeM size
        pokeM memOffset

instance StorableM (PictureUsage PictureMemoryReference) where
  sizeOfM _ =
    do sizeOfM (undefined :: Point2 PixelSpace)
       sizeOfM (undefined :: PictureMemoryReference  )
  alignmentM _ =
    do alignmentM (undefined :: Point2 PixelSpace)
       alignmentM (undefined :: PictureMemoryReference  )
  peekM = do translate <- peekM
             pMem      <- peekM
             return (PictureUsage translate pMem)
  pokeM (PictureUsage translate pMem) =
    do  pokeM translate
        pokeM pMem

instance Storable (PictureUsage PictureMemoryReference) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
