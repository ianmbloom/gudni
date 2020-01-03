{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.GeoReference
  ( GeoReference(..)
  , geoStart
  , geoNumStrands
  )
where

import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM


import Foreign.C.Types(CChar)
import Control.Lens
import Control.DeepSeq

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

instance Storable GeoReference where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
