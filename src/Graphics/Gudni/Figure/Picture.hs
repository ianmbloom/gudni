{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
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
  , noPictures
  )
where

import Foreign.C.Types
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Transformer
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
data PictureUsage ref s = PictureUsage
  { -- | Translation vector for this particular usage.
    pictTranslate :: Point2 s
    -- | Basic scaling for image.
  , pictScale :: s
    -- | An identifier for the source picture.
  , pictSource    :: ref
  } deriving (Show)

instance (Space s) => HasSpace (PictureUsage ref s) where
  type SpaceOf (PictureUsage ref s) = s

instance (Space s) => SimpleTransformable (PictureUsage ref s) where
  tTranslate p (PictureUsage translate scale ref) = PictureUsage (p + translate) scale ref
  tScale     s (PictureUsage translate scale ref) = PictureUsage translate (s * scale) ref
instance (Space s) => Transformable (PictureUsage ref s) where
  tRotate    a usage = usage -- currently not implemented

instance (NFData ref, NFData s) => NFData (PictureUsage ref s) where
  rnf (PictureUsage a b c) = a `deepseq` b `deepseq` c `deepseq` ()

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

noPictures = makePictures []
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

instance StorableM (PictureUsage PictureMemoryReference SubSpace) where
  sizeOfM _ =
    do sizeOfM (undefined :: Point2 SubSpace)
       sizeOfM (undefined :: PictureMemoryReference  )
       sizeOfM (undefined :: CFloat)
  alignmentM _ =
    do alignmentM (undefined :: Point2 SubSpace)
       alignmentM (undefined :: PictureMemoryReference  )
       alignmentM (undefined :: CFloat)
  peekM = do translate <- peekM
             pMem      <- peekM
             scale     <- peekM
             return (PictureUsage translate scale pMem)
  pokeM (PictureUsage translate scale pMem) =
    do  pokeM translate
        pokeM pMem
        pokeM scale

instance Storable (PictureUsage PictureMemoryReference SubSpace) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
