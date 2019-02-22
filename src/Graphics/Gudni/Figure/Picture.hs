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

data PictureMemory = PictureMemory
  { pictMemOffset :: MemOffset_
  , pictSize      :: Point2 IntSpace
  } deriving (Show)

instance NFData PictureMemory where
  rnf (PictureMemory a b) = a `deepseq` b `deepseq` ()

data PictureRef s = PictureRef
  { pictTranslate :: Point2 IntSpace
  , pictScale     :: PictScale_
  , pictData      :: s
  } deriving (Show)

instance NFData s => NFData (PictureRef s) where
  rnf (PictureRef a b c) = a `deepseq` b `deepseq` c `deepseq` ()

mapAccumM :: Monad m => (a -> b -> m (a, b')) -> a ->[b] -> m (a, [b'])
mapAccumM f a (b:bs) = do (a', b') <- f a b
                          (a'', bs') <- mapAccumM f a' bs
                          return (a'', b':bs')
mapAccumM f a [] = return (a, [])

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
       sizeOfM (undefined :: PictScale_     )
       sizeOfM (undefined :: PictureMemory  )
  alignmentM _ =
    do alignmentM (undefined :: Point2 IntSpace)
       alignmentM (undefined :: PictScale_     )
       alignmentM (undefined :: PictureMemory  )
  peekM = do translate <- peekM
             scale     <- peekM
             pMem      <- peekM
             return (PictureRef translate scale pMem)
  pokeM (PictureRef translate scale pMem) =
    do  pokeM translate
        pokeM scale
        pokeM pMem

instance Storable (PictureRef PictureMemory) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
