{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Enclosure
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions turning lists of outlines into enclosed lists of horizontal strands that can
-- be sent to the rasterizer.

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
import Graphics.Gudni.Raster.ReorderTable
import Graphics.Gudni.Util.StorableM

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Control.DeepSeq
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

type NumStrands_ = CUInt
-- | Wrapper for the strand count.
newtype NumStrands = NumStrands {unNumStrands :: NumStrands_} deriving (Eq, Ord, Num)

instance Show NumStrands where
  show = show . unNumStrands

-- | An enclosure is a group of outlines turned into a group of strands∘
-- A strand is sequence of pairs of onCurve and offCurve points and a terminating onCurve point where the x coordinates of each point
-- are in order and increasing (except in specific circumstances). These sequences of points are transformed into complete binary trees that the rasterizer parses to determine if and where each
-- column of pixels interacts with each strand. An enclosure includes all of the geometry needed for the rasterizer to determine whether a particular part of a pixel is inside or outside of
-- the shape, hence the name enclosure. Outside of the bounding box of the enclosure, the rasterizer can be sure there is no interaction
-- with the shape∘
-- enclosureNumStrands should be equivalent to length . enclosureEnclosureStrands
data Enclosure = Enclosure
    { enclosureNumStrands   :: !NumStrands
    , enclosureStrands      :: [Strand]
    } deriving (Show)

-- | Convert a list of outlines into an enclosure.
enclose :: ReorderTable
        -> Int
        -> V.Vector (Outline SubSpace)
        -> Enclosure
enclose curveTable maxSectionSize outlines =
  let -- Convert the list of outlines into a list of strands.
      strands = V.toList . join . fmap (outlineToStrands curveTable maxSectionSize) $ outlines
      -- Count the strands.
      numStrands = length strands :: Int
  in  Enclosure { enclosureNumStrands = fromIntegral numStrands
                , enclosureStrands    = strands
                }

------------------ Instances --------------------------

instance NFData NumStrands where
  rnf (NumStrands i) = i `deepseq` ()

instance Storable NumStrands where
  sizeOf    _ = sizeOf    (undefined::NumStrands_)
  alignment _ = alignment (undefined::NumStrands_)
  poke ptr  i = poke (castPtr ptr) . unNumStrands $ i
  peek ptr    = NumStrands <$> peek (castPtr ptr)

instance NFData Enclosure where
  rnf (Enclosure n ss) = n `deepseq` ss `deepseq` ()
