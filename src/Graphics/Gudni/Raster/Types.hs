{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Types
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic constructors and types for the rasterizer.

module Graphics.Gudni.Raster.Types
  ( Group (..)
  , Outlines (..)
  , Tile (..)
  , tileBox
  , tileHDepth
  , tileVDepth
  , tileRep
  , Shape        (..)
  , shShapeInfo, shRep
  , Combiner      (..)
  , coCompound, coRep
  , NumShapes     (..)
  , GeoReference  (..)
  , ShapeEntry(..)
  , shapeGeoRef
  , shapeStrandCount
  , shapeBox
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.ShapeInfo

import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Debug

import Control.DeepSeq
import qualified Data.Vector.Storable as VS

import Control.Lens

import Foreign.C.Types
import Foreign.Storable
import Graphics.Gudni.Util.StorableM

import System.Random
import Control.Monad.Random
import Linear

-- | Group is just a newtype for "list of"
newtype Group t = Group
    { unGroup :: [t]
    } deriving (Show)

instance HasSpace [t] => HasSpace (Group t) where
  type SpaceOf (Group t) = SpaceOf t

instance HasBox [t] => HasBox (Group t) where
  boxOf (Group ts) = boxOf ts

instance Functor Group where
  fmap f (Group vs) = Group (fmap f vs)

-- | Convenience Synonym
type Outlines s = Group (Outline s)

-- | Tile is just a pairing of the Tile Info Header and some representation of its contents.
data Tile rep = Tile
  { -- | Pixel boundaries of tile.
    _tileBox    :: !(Box PixelSpace)
    -- | Logarithmic horizontal depth.
  , _tileHDepth :: !Int
    -- | Logarithmic vertical depth.
  , _tileVDepth :: !Int
    -- | Representation of the contents of the tile.
  , _tileRep :: rep
  } deriving (Show)
makeLenses ''Tile

-- | Shape is just a pairing of ShapeInfo with some representation of reference to a shape.
data Shape t = Shape
    { _shShapeInfo :: ShapeInfo
    , _shRep      :: t
    } deriving (Show)
makeLenses ''Shape

instance Functor Shape where
  fmap f (Shape s t) = Shape s (f t)

instance NFData t => NFData (Shape t) where
  rnf (Shape a b) = a `deepseq` b `deepseq` ()

-- | Combiner is just a pairing of Compound with some representation of reference to a shape.
data Combiner t = Combiner
    { _coCompound :: Compound
    , _coRep         :: t
    } deriving (Show)
makeLenses ''Combiner

instance Functor Combiner where
  fmap f (Combiner c t) = Combiner c (f t)

instance NFData t => NFData (Combiner t) where
  rnf (Combiner a b) = a `deepseq` b `deepseq` ()

type NumShapes_ = CInt
newtype NumShapes = NumShapes {unNumShapes :: NumShapes_} deriving (Eq, Ord, Num)

instance Show NumShapes where
  show = show . unNumShapes

-- | A GeoReference is not a slice, it points to the first piece of data in memory and gives the number of strands
-- the header of the strand gives the actual size of geometric data in memory.
data GeoReference = GeoRef
  { _geoStart      :: Reference CChar
  , _geoNumStrands :: NumStrands
  }
makeLenses ''GeoReference

instance NFData GeoReference where
  rnf (GeoRef a b) = a `deepseq` b `deepseq` ()

instance Show GeoReference where
  show (GeoRef start strands) = "Geo[" ++ show start ++ "," ++ show strands ++"]"

instance StorableM GeoReference where
  sizeOfM _ = do sizeOfM (undefined :: Reference CChar)
                 sizeOfM (undefined :: NumStrands     )
  alignmentM _ = do alignmentM (undefined :: Reference CChar)
                    alignmentM (undefined :: NumStrands     )
  peekM = do s <- peekM
             n <- peekM
             return (GeoRef s n)
  pokeM (GeoRef s n) = do pokeM s
                          pokeM n

data ShapeEntry = ShapeEntry
    { _shapeGeoRef      :: GeoReference
    , _shapeStrandCount :: NumStrands
    , _shapeBox         :: BoundingBox
    } deriving (Show)
makeLenses ''ShapeEntry

-- Instances

instance StorableM (Shape GeoReference) where
  sizeOfM _ = do sizeOfM (undefined :: ShapeInfo)
                 sizeOfM (undefined :: GeoReference   )
  alignmentM _ = do alignmentM (undefined :: ShapeInfo)
                    alignmentM (undefined :: GeoReference   )
  peekM = do shapeInfo <- peekM
             geoRef <- peekM
             return (Shape shapeInfo geoRef)
  pokeM (Shape shapeInfo geoRef) = do pokeM shapeInfo
                                      pokeM geoRef

instance Storable (Shape GeoReference) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance StorableM (Tile (Slice (Shape GeoReference), Int)) where
  sizeOfM _ = do sizeOfM (undefined :: Box PixelSpace)
                 sizeOfM (undefined :: CShort)
                 sizeOfM (undefined :: CShort)
                 sizeOfM (undefined :: CInt)
                 sizeOfM (undefined :: Slice (Shape GeoReference))
  alignmentM _ = do alignmentM (undefined :: Box PixelSpace)
                    alignmentM (undefined :: CShort)
                    alignmentM (undefined :: CShort)
                    alignmentM (undefined :: CInt)
                    alignmentM (undefined :: Slice (Shape GeoReference))
  peekM = do box    <- peekM
             hDepth :: CShort <- peekM
             vDepth :: CShort <- peekM
             columnAllocation :: CInt <- peekM
             slice  <- peekM
             return (Tile box (fromIntegral hDepth) (fromIntegral vDepth) (slice, fromIntegral columnAllocation))
  pokeM (Tile box hDepth vDepth (slice, columnAllocation)) =
          do pokeM box
             pokeM (fromIntegral hDepth :: CShort)
             pokeM (fromIntegral vDepth :: CShort)
             pokeM (fromIntegral columnAllocation :: CInt)
             pokeM slice

instance Storable (Tile (Slice (Shape GeoReference), Int)) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance NFData rep => NFData (Tile rep) where
  rnf (Tile a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()
