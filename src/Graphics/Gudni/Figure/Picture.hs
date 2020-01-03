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
  ( PictureName(..)
  , Picture(..)
  , pictureSize
  , pictureData
  , PictureMap(..)
  , makePictureMap
  , noPictures
  )
where

import Foreign.C.Types
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Facet
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
import Control.Monad.State
import Control.Lens.Tuple
import Control.Monad.ListM


type PictureName = String

data Picture
   = PictureFunction
   { pictureFunction :: Point2 PixelSpace -> Color
   , pictureFunctionSize :: Point2 PixelSpace
   }
   | PictureImage
   { pictureImage :: Image PixelRGBA8
   }

type PictureMap = M.Map PictureName Picture

-- | Create a vector of raw bytes and list of picture memory offsets that the rasterizer
-- can use to reference images. (Rickety)
makePictureMap :: [(PictureName, DynamicImage)] -> PictureMap
makePictureMap images =
     let rgba8s :: [(PictureName, Image PixelRGBA8)]
         rgba8s  = map (over _2 convertRGBA8) images
     in  foldl (\m (name, image) -> M.insert name (PictureImage image) m) M.empty rgba8s

noPictures :: IO PictureMap
noPictures = return M.empty

checkPicture :: PictureName -> PictureMap -> Picture
checkPicture name mapping = (M.!) mapping name

allPixels :: Point2 PixelSpace -> [Point2 PixelSpace]
allPixels size = [(makePoint x y) | y <- [0 .. size ^. pY - 1], x <- [0 ..  size ^. pX - 1]]

pictureData :: Picture -> VS.Vector Word8
pictureData (PictureImage image) = imageData image
pictureData (PictureFunction f size) = VS.concatMap (VS.fromList . Foldable.toList . colorToRGBA8 . f) (VS.fromList $ allPixels size)

pictureSize :: Picture -> Point2 PixelSpace
pictureSize (PictureImage img) = Point2 (fromIntegral $ imageWidth  img) (fromIntegral $ imageHeight img)
pictureSize (PictureFunction _ size) = size

instance Show Picture where
  show (PictureFunction _ size) = "PictureFunction" ++ show size
  show (PictureImage _) = "PictureImage"

instance NFData Picture where
  rnf (PictureFunction _ s) = s `deepseq` ()
  rnf (PictureImage i) = i `deepseq` ()
