{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Constants
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Global constants that paramaterize the rasterizer.

module Graphics.Gudni.Raster.Constants
    ( UINT4(..)
    , THRESHOLDTYPE(..)
    , HEADERTYPE(..)
    , tAXICABfLATNESS
    , sTOCHASTICfACTOR
    , mINtILEsIZE
    , mAXsECTIONsIZE
    , mAXlAYERS
    , mAXtHRESHOLDS
    , mAXfACETiD
    , lAYERfLAGSsECTIONS
    , rANDOMFIELDsIZE
    , sOURCEfILEpADDING
    , iNITgEOMETRYpILEsIZE
    , nULLtILE
    , iTEMtAGiSfACETbITMASK
    , iTEMtAGiSsHAPE
    , iTEMtAGiSfACET
    , iTEMtAGcOMPOUNDtYPEbITMASK
    , iTEMtAGcOMPOUNDtYPEaDD
    , iTEMtAGcOMPOUNDtYPEsUBTRACT
    , iTEMtAGsUBSTANCEIDbITMASK
    , iTEMtAGsUBSTANCEIDsHIFT
    , iTEMtAGiTEMrEFbITMASK
    , sUBSTANCEtAGtYPEbITmASK
    , sUBSTANCEtAGtYPEsOLIDcOLOR
    , sUBSTANCEtAGtYPEtEXTURE
    , sUBSTANCEtAGrEFbITMASK
    , nOsUBSTANCEiD
    )
where

import Graphics.Gudni.Figure
import Control.Lens
import Foreign.C.Types (CInt, CUInt, CULong, CFloat, CDouble)
import Data.Word
import Linear.V4

type UINT4 = V4 CUInt
type THRESHOLDTYPE = V4 CFloat
type HEADERTYPE    = CULong

tAXICABfLATNESS    = 0.25 :: Float -- minimum taxicab distance between (relative to the pixel size) where curve tesselation terminates
sIZEoFlAYERiD         = 4 :: Int -- sizof(uint)
sIZEoFlOCALsUBSTANCEiD = 4 :: Int -- sizof(uint)
sIZEoFlAYEReNTRY   = 2 :: Int -- sizeof(ushort)
sIZEoFsUBSTANCETAG = 8 :: Int -- sizeof(ulong)
sIZEoFfACETiD = 4 :: Int -- sizeof(uint)
lAYERfLAGSsECTIONS = 8 :: Int
sIZEoFsUBSTANCEiD  = 4 :: Int --sizeof(int)
sIZEoFlAYERfLAGSsECTION = 8 :: Int -- sizeof(ulong)
sTOCHASTICfACTOR = 0.0 :: Float -- relative amount of variability in an edge.
mAXfACETiD       = (2^20) :: Int -- 1048576 total possible number of facets per scene

mAXlAYERS        = 256 :: Int -- a hard limit on the number of shapes based on the empirical testing of the timeout period of the GPU.
mAXtHRESHOLDS    = 256 :: Int

mINtILEsIZE      = Point2 16 32 :: Point2 PixelSpace
mAXsECTIONsIZE   = 32   :: Int
rANDOMFIELDsIZE  = 4096 :: Int -- must be a power of 2

sOURCEfILEpADDING = 40 :: Int -- number of lines at the head of the openCL source file reserved to be replaced by haskell generated preprocessor defines

iNITgEOMETRYpILEsIZE = 65536 :: Int

nULLtILE = V4 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF :: UINT4

-- Item Tag Bit Layout
-- Bits | 1 bit   | 1 bit        | 30 bit      | 32 bit
-- Type | bool    | bool         | uint        | geoId
-- Desc | true =  | true =       | substanceId | or
--      | facet   | additive     |             | facetId
--      | false = | false =      |             |
--      | shape   | subtractive  |             |

-- Bit 63
iTEMtAGiSfACETbITMASK          = 0x8000000000000000 :: CULong -- & with this to determine if the shapetag is for a facet.
iTEMtAGiSsHAPE                 = 0x0000000000000000 :: CULong -- & with this to determine if the shapetag is for a facet.
iTEMtAGiSfACET                 = 0x8000000000000000 :: CULong -- & with this to determine if the shapetag is for a facet.
-- Bit 62
iTEMtAGcOMPOUNDtYPEbITMASK     = 0x4000000000000000 :: CULong -- & with this to isolate the compound type bit.
iTEMtAGcOMPOUNDtYPEaDD         = 0x4000000000000000 :: CULong
iTEMtAGcOMPOUNDtYPEsUBTRACT    = 0x0000000000000000 :: CULong

-- Bits 61 - 32
iTEMtAGsUBSTANCEIDbITMASK      = 0x3FFFFFFF00000000 :: CULong -- & with this to isolate the substanceId
iTEMtAGsUBSTANCEIDsHIFT        = 32 :: Int

-- Bits 31 - 0
iTEMtAGiTEMrEFbITMASK           = 0x00000000FFFFFFFF :: CULong

-- Substance Tag Bit Layout
-- Bits | 8 bit          | 56 bit                           |
-- Type | enumerated     | uint                             |
-- Desc | substancetype  | substance info record offset     |

-- Bits 63 - 56
sUBSTANCEtAGtYPEbITmASK    = 0xFF00000000000000 :: CULong -- & with this to determine if the shapetag is for mask colorWith shape or a picture.
sUBSTANCEtAGtYPEsOLIDcOLOR = 0x0000000000000000 :: CULong -- & with this to determine if the shapetag is additive.
sUBSTANCEtAGtYPEtEXTURE    = 0x0100000000000000 :: CULong

-- Bits 55 - 0
sUBSTANCEtAGrEFbITMASK     = 0x00FFFFFFFFFFFFFF :: CULong -- & with this to get the reference for the substance tag

nOsUBSTANCEiD              = 0xFFFFFFFF :: CUInt
