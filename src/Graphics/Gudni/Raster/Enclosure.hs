{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Raster.Enclosure
  ( Enclosure  (..)
  , NumStrands (..)
  , enclose
  )
where

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Outline
import Graphics.Gudni.Raster.Strand
import Graphics.Gudni.Raster.StrandLookupTable
import Graphics.Gudni.Util.StorableM

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Control.DeepSeq

type NumStrands_ = CUInt
newtype NumStrands = NumStrands {unNumStrands :: NumStrands_} deriving (Eq, Ord, Num)

instance Show NumStrands where
  show = show . unNumStrands
  
data Enclosure = Enclosure
 { enclosureNumStrands   :: !NumStrands
 , enclosureTrees        :: [Strand]
 } deriving (Show)

enclose :: CurveTable
        -> Int
        -> [Outline DisplaySpace]
        -> Enclosure
enclose curveTable sectionSize curves =
  let trees = concatMap (shapeToStrands curveTable sectionSize) curves
      numStrands = length trees
  in  Enclosure { enclosureNumStrands    = fromIntegral numStrands
                , enclosureTrees         = trees
                }

------------------ Instances --------------------------

instance StorableM Enclosure where
  sizeOfM (Enclosure _ items) = mapM_ sizeOfM items
  alignmentM (Enclosure _ items) = mapM_ alignmentM items
  peekM = error "no peek for Enclosure"
  pokeM (Enclosure _ items) = mapM_ pokeM items

instance Storable Enclosure where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance NFData NumStrands where
  rnf (NumStrands i) = i `deepseq` ()

instance Storable NumStrands where
  sizeOf    _ = sizeOf    (undefined::NumStrands_)
  alignment _ = alignment (undefined::NumStrands_)
  poke ptr  (NumStrands i) = poke (castPtr ptr) i
  peek ptr = NumStrands <$> peek (castPtr ptr)

instance NFData Enclosure where
  rnf (Enclosure n ss) = n `deepseq` ss `deepseq` ()
