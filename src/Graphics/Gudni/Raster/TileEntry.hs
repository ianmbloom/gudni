{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Gudni.Raster.TileEntry
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Storable Tile Record

module Graphics.Gudni.Raster.TileEntry
  ( Tile (..)
  , tileBox
  , tileHDepth
  , tileVDepth
  , tileRep
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.GeoReference
import Graphics.Gudni.Raster.ItemInfo

import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM

import Control.Lens
import Control.DeepSeq
import Foreign.C.Types(CShort,CInt)

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

instance StorableM (Tile (Slice ItemTag, Int)) where
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

instance Storable (Tile (Slice ItemTag, Int)) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance NFData rep => NFData (Tile rep) where
  rnf (Tile a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()
