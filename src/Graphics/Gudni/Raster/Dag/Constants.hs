{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Constants
-- Copyright   :  (c) Ian Bloom 2020
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructors for attaching metadata to shapes∘

module Graphics.Gudni.Raster.Dag.Constants
    ( ShapeId_
    , nULLsHAPEiD
    , StorageId_
    , PrimTag_
    , pRIMtAGtYPEbITMASK
    , pRIMtAGiSbEZIER
    , pRIMtAGiSfACET
    , pRIMtAGiSrECTANGLE
    , pRIMtAGiSeLIPSE
    , pRIMtAGsTORAGEiDbITMASK
    , pRIMtAGsTORAGEiDsHIFT
    , pRIMtAGfABRICiDbITMASK

    , FabricTag_
    , nULLfABRICtAGiD
    , fABRICtAGtYPEbITMASK
    , fABRICiStREE
    , fABRICiStRANSFORMaFFINE
    , fABRICiStRANSFORMfACET
    , fABRICiStRANSFORMfILTER
    , fABRICiStRANSFORMcONVOLVE
    , fABRICiSsUBSTANCE
    , fABRICiScOMPOSITE
    , fABRICiSmULT
    , fABRICiSaDD
    , fABRICiSfLOAToR
    , fABRICiSfLOATxOR

    , fABRICtAGdATAbITMASK
    , fABRICtAGhIGHiDbITMASK
    , fABRICtAGhIGHiDsHIFT
    , fABRICtAGlOWiDbITMASK
    , SubstanceTag_
    , sUBSTANCEtAGtYPEbITMASK
    , sUBSTANCEiSgEOMETRY
    , sUBSTANCEiScONSTANT
    , sUBSTANCEiStEXTURE
    , sUBSTANCEiSlINEAR
    , sUBSTANCEiSqUADRANCE
    , sUBSTANCEdATArEFbITMASK
    )
where

import Foreign.C.Types (CUInt, CULong)

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

instance Out CULong where
    doc x = text . show $ x
    docPrec _ = doc

type ShapeId_ = CUInt

nULLsHAPEiD = 0xFFFFFFFF :: ShapeId_

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

-- Fabric Tag Bit Layout
-- Depending on Type there are two layouts
-- Bits | 4 bit    | 30 bit      | 30 bit     |
-- Type | int      | uint        | uint       |
-- Desc | type     | high        | low        |
--      |          |             |            |

-- Bits | 4 bit    | 60 bit                   |
-- Type | int      | uint                     |
-- Desc | type     | data                     |
--      |          |                          |

type FabricTag_   = CULong

-- Bit 63 - 60
fABRICtAGtYPEbITMASK      = 0xF000000000000000 :: CULong -- & with this to get the fabric type
fABRICiStREE              = 0x0000000000000000 :: CULong
fABRICiSsUBSTANCE         = 0x1000000000000000 :: CULong
fABRICiStRANSFORMaFFINE   = 0x2000000000000000 :: CULong
fABRICiStRANSFORMfACET    = 0x3000000000000000 :: CULong
fABRICiStRANSFORMfILTER   = 0x4000000000000000 :: CULong
fABRICiStRANSFORMcONVOLVE = 0x5000000000000000 :: CULong

fABRICiScOMPOSITE         = 0x8000000000000000 :: CULong
fABRICiSmULT              = 0x9000000000000000 :: CULong
fABRICiSaDD               = 0xA000000000000000 :: CULong
fABRICiSfLOAToR           = 0xB000000000000000 :: CULong
fABRICiSfLOATxOR          = 0xC000000000000000 :: CULong

-- Bit 59 - 0
fABRICtAGdATAbITMASK      = 0x0FFFFFFFFFFFFFFF :: CULong -- & with this to get the whole data section of the tag

-- Bit 59 - 30
fABRICtAGhIGHiDbITMASK    = 0x0FFFFFFFC0000000 :: CULong
fABRICtAGhIGHiDsHIFT      = 30 :: Int

-- Bit 29 - 0
fABRICtAGlOWiDbITMASK     = 0x000000003FFFFFFF :: CULong

nULLfABRICtAGiD           = 0x3FFFFFFF :: CUInt
-- Substance Tag Bit Layout
-- This fits inside of a FabricTag
-- Bits | 4 bit     | 4 bit | 56 bit                   |
-- Type | used by   | uint  | substance data refernce  |
-- Desc | fabricTag | type  |                          |
--      |           |       |                          |

type SubstanceTag_ = FabricTag_

-- Bit 59 - 56
sUBSTANCEtAGtYPEbITMASK   = 0x0F00000000000000 :: CULong -- & with this to get the fabric type
sUBSTANCEiSgEOMETRY       = 0x0000000000000000 :: CULong
sUBSTANCEiScONSTANT       = 0x0100000000000000 :: CULong
sUBSTANCEiStEXTURE        = 0x0200000000000000 :: CULong
sUBSTANCEiSlINEAR         = 0x0300000000000000 :: CULong
sUBSTANCEiSqUADRANCE      = 0x0400000000000000 :: CULong

-- Bit 55 - 0
sUBSTANCEdATArEFbITMASK   = 0x00FFFFFFFFFFFFFF :: CULong
