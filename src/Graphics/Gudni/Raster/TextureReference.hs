{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
  ( PictMemId(..)
  , PictUsageId(..)
  , PictureMap(..)
  , PictureMemoryReference(..)
  , ShapeTreePictureMemory(..)
  , makePictureMap
  , namePicturesInShapeTree
  , pictureTextureSize
  --, makePictData
  , collectPictureMemory
  , assignScenePictureMemory
  )
where

import Foreign.C.Types
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Picture
import Graphics.Gudni.Figure.Substance
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Raster.TraverseShapeTree
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


type PictMemId = CUInt
type PictUsageId = CUInt
type MemOffset_ = Reference Word8


type PictureMemoryMap = M.Map PictureName PictureMemoryReference
type PictureIdMap = M.Map PictureName PictMemId

-- | The starting memory offset and size of a picture.
data PictureMemoryReference = PictureMemory
  { -- | The dimensions of the picture in memory.
    pictSize      :: Point2 PixelSpace
    -- | The offset of the picture data within the combined buffer of all source pictures.
  , pictMemOffset :: MemOffset_
  } deriving (Show)

pictureTextureSize :: PictureMemoryReference -> Point2 TextureSpace
pictureTextureSize = fmap pixelSpaceToTextureSpace . pictSize

instance NFData PictureMemoryReference where
  rnf (PictureMemory a b) = a `deepseq` b `deepseq` ()

type ShapeTreeNamed token s = STree () (SRep token PictureName (CompoundTree s))
type ShapeTreePictureMemory token s = STree () (SRep token PictureMemoryReference (CompoundTree s))

overRepSubstanceM :: Monad m => (a -> m b) -> SRep token a rep -> m (SRep token b rep)
overRepSubstanceM f (SRep token a rep) =
  do b <- mapMSubstanceTexture f a
     return (SRep token b rep)

nameTexture :: NamedTexture -> State PictureMap PictureName
nameTexture namedTexture =
    case namedTexture of
        NewTexture name image ->
           do modify (M.insert name image)
              return name
        SharedTexture name -> return name

namePicturesInShapeTree :: ShapeTree token s -> State PictureMap (ShapeTreeNamed token s)
namePicturesInShapeTree tree = mapMSTree (overRepSubstanceM nameTexture) tree

accumulatePicture :: Picture -> StateT (Pile Word8) IO PictureMemoryReference
accumulatePicture picture =
  do pictPile <- get
     let size = pictureSize picture
         memory = PictureMemory size (pictPile ^. pileCursor)
         pVector = pictureData picture
     (pictPile', _) <- liftIO $ addVectorToPile pictPile pVector
     put pictPile'
     return memory

collectPictureMemory :: PictureMap -> IO (PictureMemoryMap, Pile Word8)
collectPictureMemory mapping =
  do pictPile <- newPile
     runStateT (mapM accumulatePicture mapping) pictPile

assignPictUsage :: PictureMemoryMap -> SRep token PictureName rep -> SRep token PictureMemoryReference rep
assignPictUsage mapping (SRep token substance rep) =
     let substance' =
             case substance of
                 Texture name -> Texture ((M.!) mapping name)
                 Solid color  -> Solid color
     in  SRep token substance' rep

assignScenePictureMemory :: PictureMap -> Scene (ShapeTree token s) -> IO (Scene (ShapeTreePictureMemory token s), Pile Word8)
assignScenePictureMemory pictureMap scene =
  do let (namedTree, pictureMap') = runState (namePicturesInShapeTree (scene ^. sceneShapeTree)) pictureMap
     (pictureRefMap, pictDataPile) <- collectPictureMemory pictureMap'
     let pictRefScene = set sceneShapeTree (mapSTree (assignPictUsage pictureRefMap) namedTree) scene
     return (pictRefScene, pictDataPile)


instance StorableM PictureMemoryReference where
  sizeOfM _ =
    do sizeOfM (undefined :: Point2 PixelSpace)
       sizeOfM (undefined :: MemOffset_     )
       sizeOfM (undefined :: CUInt     ) -- padding
  alignmentM _ =
    do alignmentM (undefined :: Point2 PixelSpace)
       alignmentM (undefined :: MemOffset_     )
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
