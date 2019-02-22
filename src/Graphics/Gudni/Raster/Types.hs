{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.Gudni.Raster.Types
  ( Block (..)
  , NumShapes      (..)
  , GeoReference  (..)
  , Shape         (..)
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Raster.Primitive

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

------------------ Block -------------------
-- a block of tiles overlapped by a boundingBox
type Block = Box IntSpace

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

-------------------------- Shape ------------------------------

type Shape = (Primitive, GeoReference)

instance StorableM Shape where
  sizeOfM _ = do sizeOfM (undefined :: Primitive)
                 sizeOfM (undefined :: GeoReference   )
  alignmentM _ = do alignmentM (undefined :: Primitive)
                    alignmentM (undefined :: GeoReference   )
  peekM = do prim <- peekM
             geoRef <- peekM
             return (prim, geoRef)
  pokeM (prim, geoRef) = do pokeM prim
                            pokeM geoRef

instance Storable Shape where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
