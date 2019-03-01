{-# LANGUAGE FlexibleInstances #-}
module Graphics.Gudni.Figure.Picture
  ( PictId(..)
  , PictureMemory(..)
  , PictureRef(..)
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
type PictScale_ = CUInt
type MemOffset_ = Reference Word8

-- | The starting memory offset and size of a picture.
data PictureMemory = PictureMemory
  { pictMemOffset :: MemOffset_
  , pictSize      :: Point2 IntSpace
  } deriving (Show)

instance NFData PictureMemory where
  rnf (PictureMemory a b) = a `deepseq` b `deepseq` ()

-- | A reference to a picture and an offset.
data PictureRef s = PictureRef
  { pictTranslate :: Point2 IntSpace
  , pictData      :: s
  } deriving (Show)

instance NFData s => NFData (PictureRef s) where
  rnf (PictureRef a b) = a `deepseq` b `deepseq` ()

-- | Create a vector of raw bytes and list of picture memory offsets that the rasterizer
-- can use to reference images. (Rickety)
makePictures :: [DynamicImage] -> IO (Maybe (Pile Word8), [PictureMemory])
makePictures images =
  do pile <- newPile :: IO (Pile Word8)
     let rgba8s  = map convertRGBA8 images
     (pile', offsets) <- mapAccumM addVectorToPile pile (map imageData rgba8s)
     let sizes   = zipWith Point2 (map (fromIntegral . imageWidth ) rgba8s)
                                  (map (fromIntegral . imageHeight) rgba8s)
         pictMems = zipWith PictureMemory offsets sizes
         mPile = if length images > 0 then Just pile' else Nothing
     return (mPile, pictMems)

instance StorableM PictureMemory where
  sizeOfM _ =
    do sizeOfM (undefined :: MemOffset_     )
       sizeOfM (undefined :: Point2 IntSpace)
  alignmentM _ =
    do alignmentM (undefined :: MemOffset_     )
       alignmentM (undefined :: Point2 IntSpace)
  peekM = do size      <- peekM
             memOffset <- peekM
             return (PictureMemory size memOffset)
  pokeM (PictureMemory size memOffset) =
    do  pokeM size
        pokeM memOffset

instance StorableM (PictureRef PictureMemory) where
  sizeOfM _ =
    do sizeOfM (undefined :: Point2 IntSpace)
       sizeOfM (undefined :: PictureMemory  )
  alignmentM _ =
    do alignmentM (undefined :: Point2 IntSpace)
       alignmentM (undefined :: PictureMemory  )
  peekM = do translate <- peekM
             pMem      <- peekM
             return (PictureRef translate pMem)
  pokeM (PictureRef translate pMem) =
    do  pokeM translate
        pokeM pMem

instance Storable (PictureRef PictureMemory) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
