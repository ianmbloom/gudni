{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.Thresholds.StrandReference
  ( StrandReference(..)
  , unStrandRef
  )
where

import Graphics.Gudni.Raster.Thresholds.Enclosure

import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM

import Foreign.C.Types(CChar)
import Control.Lens
import Control.DeepSeq

-- | A GeoReference is not a slice, it points to the first piece of data in memory and gives the number of strands
-- the header of the strand gives the actual size of geometric data in memory.
type StrandReference_ = Reference CChar
newtype StrandReference = StrandRef { _unStrandRef :: StrandReference_}
makeLenses ''StrandReference

instance NFData StrandReference where
  rnf (StrandRef s) = s `deepseq` ()

instance Show StrandReference where
  show (StrandRef s) = "StrRef[" ++ show s ++ "]"

instance StorableM StrandReference where
  sizeOfM _ = do sizeOfM (undefined :: StrandReference_)
  alignmentM _ = do alignmentM (undefined :: StrandReference_)
  peekM = StrandRef <$> peekM
  pokeM (StrandRef s ) = pokeM s

instance Storable StrandReference where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
