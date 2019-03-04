{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.Gudni.Raster.Types
  ( Group         (..)
  , Shaper        (..)
  , Shape         (..)
  , shShapeInfo, shRep
  , Combiner      (..)
  , coCombineType, coRep
  , NumShapes     (..)
  , GeoReference  (..)
  , PrimEntry(..)
  , primGeoRef
  , primStrandCount
  , primBox
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

-- | Group is just a newtype for "list of"
newtype Group t = Group
    { unGroup :: [t]
    } deriving (Show)

instance Boxable t => Boxable (Group t) where
  getBoundingBox (Group ts) = getBoundingBox $ map getBoundingBox ts

instance Functor Group where
  fmap f (Group vs) = Group (fmap f vs)

-- | Shaper is just a pairing of ShapeInfo with some representation of reference to a shape.
data Shaper t = Shaper
    { _shShapeInfo :: ShapeInfo
    , _shRep      :: t
    } deriving (Show)
makeLenses ''Shaper

instance Functor Shaper where
  fmap f (Shaper s t) = Shaper s (f t)

instance NFData t => NFData (Shaper t) where
  rnf (Shaper a b) = a `deepseq` b `deepseq` ()

-- | Combiner is just a pairing of CombineType with some representation of reference to a shape.
data Combiner t = Combiner
    { _coCombineType :: CombineType
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

------------------ GeoReference -------------------------
-- a GeoReference is not a slice, it points to the first piece of data in memory and gives the number of strands
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

data PrimEntry = PrimEntry
    { _primGeoRef :: GeoReference
    , _primStrandCount :: NumStrands
    , _primBox :: BoundingBox
    } deriving (Show)
makeLenses ''PrimEntry

-------------------------- Shape ------------------------------

type Shape = Shaper GeoReference

instance StorableM Shape where
  sizeOfM _ = do sizeOfM (undefined :: ShapeInfo)
                 sizeOfM (undefined :: GeoReference   )
  alignmentM _ = do alignmentM (undefined :: ShapeInfo)
                    alignmentM (undefined :: GeoReference   )
  peekM = do shapeInfo <- peekM
             geoRef <- peekM
             return (Shaper shapeInfo geoRef)
  pokeM (Shaper shapeInfo geoRef) = do pokeM shapeInfo
                                       pokeM geoRef

instance Storable Shape where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
