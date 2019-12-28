{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
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
  , PictureName(..)
  , Picture(..)
  , PictureMap(..)
  , PictureMemoryReference(..)
  , PictureUsage(..)
  , makePictureMap
  , makePictData
  , noPictures
  )
where

import Foreign.C.Types
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Debug

import Codec.Picture

import Data.List
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as VS
import qualified Data.Map as M
import qualified Data.Foldable as Foldable
import Data.Traversable

import Control.DeepSeq
import Control.Lens
import Control.Lens.Tuple
import Control.Monad.ListM

type PictId = CUInt
type MemOffset_ = Reference Word8

type PictureName = String

data Picture
   = PictureFunction
   { pictureFunction :: Point2 PixelSpace -> Color
   , pictureFunctionSize :: Point2 PixelSpace
   }
   | PictureImage
   { pictureImage :: Image PixelRGBA8
   }

allPixels :: Point2 PixelSpace -> [Point2 PixelSpace]
allPixels size = [(makePoint x y) | y <- [0 .. size ^. pY - 1], x <- [0 ..  size ^. pX - 1]]

pictureData :: Picture -> VS.Vector Word8
pictureData (PictureImage image) = imageData image
pictureData (PictureFunction f size) = VS.concatMap (VS.fromList . Foldable.toList . colorToRGBA8 . f) (VS.fromList $ allPixels size)

pictureSize :: Picture -> Point2 PixelSpace
pictureSize (PictureImage img) = Point2 (fromIntegral $ imageWidth  img) (fromIntegral $ imageHeight img)
pictureSize (PictureFunction _ size) = size


type PictureMap = M.Map PictureName Picture

instance Show Picture where
  show (PictureFunction _ size) = "PictureFunction" ++ show size
  show (PictureImage _) = "PictureImage"

instance NFData Picture where
  rnf (PictureFunction _ s) = s `deepseq` ()
  rnf (PictureImage i) = i `deepseq` ()

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
  translateBy p (PictureUsage translate scale ref) = PictureUsage (p + translate) scale ref
  stretchBy   s (PictureUsage translate scale ref) = PictureUsage translate (unOrtho (s ^. pX) * scale) ref
  scaleBy     s (PictureUsage translate scale ref) = PictureUsage translate (s * scale) ref
instance (Space s) => Transformable (PictureUsage ref s) where
  rotateBy    a usage = usage -- currently not implemented

instance (NFData ref, NFData s) => NFData (PictureUsage ref s) where
  rnf (PictureUsage a b c) = a `deepseq` b `deepseq` c `deepseq` ()

-- | Create a vector of raw bytes and list of picture memory offsets that the rasterizer
-- can use to reference images. (Rickety)
makePictureMap :: [(PictureName, DynamicImage)] -> PictureMap
makePictureMap images =
     let rgba8s :: [(PictureName, Image PixelRGBA8)]
         rgba8s  = map (over _2 convertRGBA8) images
     in  foldl (\m (name, image) -> M.insert name (PictureImage image) m) M.empty rgba8s

noPictures :: IO PictureMap
noPictures = return M.empty

findPicture ::  (M.Map PictureName (Either Picture PictureMemoryReference), Pile Word8) -> PictureUsage PictureName s
            ->  IO ((M.Map PictureName (Either Picture PictureMemoryReference), Pile Word8), PictureUsage PictureMemoryReference s)
findPicture (mapping, pictPile) usage =
    let name = pictSource usage
    in
    case M.lookup name mapping of
        Nothing -> error "cannot find picture name in picture map."
        Just eitherPicture ->
            case eitherPicture of
                Right foundReference ->
                      return ((mapping, pictPile), usage {pictSource = foundReference})
                Left picture ->
                  do  let size = pictureSize picture
                          memory = PictureMemory size (pictPile ^. pileCursor)
                          pVector = pictureData picture
                      (pictPile', _) <- addVectorToPile pictPile pVector
                      let mapping' = M.insert name (Right memory) mapping
                          newUsage = usage {pictSource = memory}
                      return ((mapping', pictPile'), newUsage)


-- | Create a vector of raw bytes and list of picture memory offsets that the rasterizer
-- can use to reference images. (Rickety)

makePictData :: (Show s, Storable (PictureUsage PictureMemoryReference s))
             => PictureMap
             -> [PictureUsage PictureName s]
             -> IO (Pile Word8, Pile (PictureUsage PictureMemoryReference s))
makePictData mapping usages =
  do  dataPile <- newPile
      ((_, pictDataPile), usages') <- mapAccumM findPicture (M.map Left mapping, dataPile) usages
      usagePile <- listToPile usages'
      return (pictDataPile, usagePile)

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
