{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Constants
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructors for attaching metadata to shapes∘

module Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Constants
    ( StorageId_
    , PrimTag_
    , pRIMtAGtYPEbITMASK
    , pRIMtAGiSbEZIER
    , pRIMtAGiSfACET
    , pRIMtAGiSrECTANGLE
    , pRIMtAGiSeLIPSE
    , pRIMtAGsTORAGEiDbITMASK
    , pRIMtAGsTORAGEiDsHIFT
    , pRIMtAGfABRICiDbITMASK
    )
where

import Foreign.C.Types (CInt, CUInt, CULong)

-- Prim Tag Bit Layout
-- Bits | 4 bit    | 30 bit      | 30 bit      |
-- Type | int      | uint        | uint        |
-- Desc | type     | storageId   | fabricTagId |
--      |          |             |             |

type StorageId_ = CUInt
type PrimTag_   = CULong

-- Bit 63 - 60
pRIMtAGtYPEbITMASK           = 0xF000000000000000 :: CULong -- & with this to determine the itemTag type.
pRIMtAGiSbEZIER              = 0x0000000000000000 :: CULong -- & with this to determine if the itemTage is a bezier.
pRIMtAGiSfACET               = 0x1000000000000000 :: CULong -- & with this to determine if the shapetag is for a facet.
pRIMtAGiSrECTANGLE           = 0x2000000000000000 :: CULong
pRIMtAGiSeLIPSE              = 0x3000000000000000 :: CULong

-- Bit 59 - 30
pRIMtAGsTORAGEiDbITMASK      = 0x0FFFFFFFC0000000 :: CULong
pRIMtAGsTORAGEiDsHIFT        = 30 :: Int

-- Bit 29 - 0
pRIMtAGfABRICiDbITMASK       = 0x000000003FFFFFFF :: CULong
