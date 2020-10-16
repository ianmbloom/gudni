{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DeriveGeneric     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Substance.Picture
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for importing and storing bitmap data to use as textured substances in shapes.

module Graphics.Gudni.Figure.Substance.Picture
  ( PictureName(..)
  , Picture(..)
  , pictureSize
  , pictureData
  , PictureMap(..)
  , makePictureMap
  , noPictures
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Substance.Color
import Graphics.Gudni.Figure.Facet

import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Debug

import Codec.Picture

import Foreign.C.Types

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
import Control.Lens.Tuple
import Control.Monad.ListM

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))


type PictureName = String

data Picture
   = PictureFunction
   { pictureFunction     :: Point2 PixelSpace -> Color
   , pictureFunctionSize :: Point2 PixelSpace
   }
   | PictureImage
   { pictureImage :: Image PixelRGBA8
   }

instance Out Picture where
    doc x = text "Picture"
    docPrec _ = doc

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
pictureData (PictureFunction f size) = error "need to reimplement PictureFunction"
  -- VS.concatMap (VS.fromList . Foldable.toList . colorToRGBA8 . f) (VS.fromList $ allPixels size)

pictureSize :: Picture -> Point2 PixelSpace
pictureSize (PictureImage img) = Point2 (fromIntegral $ imageWidth  img) (fromIntegral $ imageHeight img)
pictureSize (PictureFunction _ size) = size

instance Show Picture where
  show (PictureFunction _ size) = "PictureFunction" ++ show size
  show (PictureImage _) = "PictureImage"
