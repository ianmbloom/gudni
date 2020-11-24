{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Thresholds.Constants
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Global constants that paramaterize the rasterizer.

module Graphics.Gudni.Raster.Thresholds.Constants
    ( UINT4(..)
    , THRESHOLDTYPE(..)
    , HEADERTYPE(..)
    , tAXICABfLATNESS
    , sTOCHASTICfACTOR
    , rANDOMFIELDsIZE
    , sHAPEsTACKsIZE
    , sOURCEfILEpADDING

    , mAXsTRANDsIZE
    , mAXlAYERS
    , mAXtHRESHOLDS
    , mAXfACETiD
    , lAYERfLAGSsECTIONS

    , nULLtILE
    , iTEMtAGsLOPEbITMASK
    , iTEMtAGsLOPEpOSITVE
    , iTEMtAGsLOPEnEGATIVE
    , iTEMtAGpERSISTbITMASK
    , iTEMtAGpERSISTANT
    , iTEMtAGnONPERSISTANT
    , iTEMtAGtYPEbITMASK
    , iTEMtAGiSbEZIER
    , iTEMtAGiSfACET
    , iTEMtAGcOMPOUNDbITMASK
    , iTEMtAGcOMPOUNDaDD
    , iTEMtAGcOMPOUNDsUBTRACT
    , iTEMtAGsUBSTANCEIDbITMASK
    , iTEMtAGsUBSTANCEIDsHIFT
    , iTEMtAGrEFERENCEbITMASK
    , nOiTEMtAG
    , sUBSTANCEtAGtYPEbITmASK
    , sUBSTANCEtAGtYPEsOLIDcOLOR
    , sUBSTANCEtAGtYPEtEXTURE
    , sUBSTANCEtAGtYPElINEARgRADIENT
    , sUBSTANCEtAGtYPErADIALgRADIENT
    , sUBSTANCEtAGrEFbITMASK
    , nOsUBSTANCEtAG
    , nULLrEFERENCE
    )
where

--import Graphics.Gudni.Figure
import Control.Lens
import Foreign.C.Types (CInt, CUInt, CULong, CFloat, CDouble)
import Data.Word
import Linear.V4

type UINT4 = V4 CUInt
type THRESHOLDTYPE = V4 CFloat
type HEADERTYPE    = CULong

sTOCHASTICfACTOR = 0.0  :: Float -- relative amount of variability in an edge.
tAXICABfLATNESS  = 0.25 :: Float -- minimum taxicab distance between (relative to the pixel size) where curve tesselation terminates
rANDOMFIELDsIZE  = 4096 :: Int -- must be a power of 2
sHAPEsTACKsIZE   = 64   :: Int

sOURCEfILEpADDING = 40 :: Int -- number of lines at the head of the openCL source file reserved to be replaced by haskell generated preprocessor defines

sIZEoFlAYERiD         = 4 :: Int -- sizof(uint)
sIZEoFlOCALsUBSTANCEiD = 4 :: Int -- sizof(uint)
sIZEoFlAYEReNTRY   = 2 :: Int -- sizeof(ushort)
sIZEoFsUBSTANCETAG = 8 :: Int -- sizeof(ulong)
sIZEoFfACETiD = 4 :: Int -- sizeof(uint)
lAYERfLAGSsECTIONS = 8 :: Int
sIZEoFsUBSTANCEiD  = 4 :: Int --sizeof(int)
sIZEoFlAYERfLAGSsECTION = 8 :: Int -- sizeof(ulong)
mAXfACETiD       = (2^20) :: Int -- 1048576 total possible number of facets per scene
mAXlAYERS        = 256 :: Int -- a hard limit on the number of shapes based on the empirical testing of the timeout period of the GPU.
mAXtHRESHOLDS    = 256 :: Int
mAXsTRANDsIZE   = 32   :: Int

nULLtILE = V4 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF :: UINT4

-- Item Tag Bit Layout
-- Bits | 1 bit    | 1 bit       | 1 bit     | 1 bit           | 28 bit      | 32 bit    |
-- Type | bool     | bool        | bool      | bool            | uint        |           |
-- Desc | reserved | reserved    | 0 = shape | 0 = subtractive | substanceId | strandRef |
--      | for      | for         | 1 = facet | 1 = additive    |             | or        |
--      | slope    | persistence |           |                 |             | facetRef  |
--      |          |             |           |                 |             |           |

-- Bit 63
iTEMtAGsLOPEbITMASK          = 0x8000000000000000 :: CULong -- & with this to determine if the shapetag is for a facet.
iTEMtAGsLOPEpOSITVE          = 0x8000000000000000 :: CULong -- & with this to determine if the shapetag is for a facet.
iTEMtAGsLOPEnEGATIVE         = 0x0000000000000000 :: CULong

-- Bit 62
iTEMtAGpERSISTbITMASK        = 0x4000000000000000 :: CULong
iTEMtAGpERSISTANT            = 0x4000000000000000 :: CULong
iTEMtAGnONPERSISTANT         = 0x0000000000000000 :: CULong

-- Bit 61
iTEMtAGtYPEbITMASK           = 0x2000000000000000 :: CULong -- & with this to determine the itemTag type.
iTEMtAGiSbEZIER              = 0x0000000000000000 :: CULong -- & with this to determine if the itemTage is a bezier.
iTEMtAGiSfACET               = 0x2000000000000000 :: CULong -- & with this to determine if the shapetag is for a facet.



-- Bit 60
iTEMtAGcOMPOUNDbITMASK       = 0x1000000000000000 :: CULong -- & with this to isolate the compound type bit.
iTEMtAGcOMPOUNDaDD           = 0x1000000000000000 :: CULong
iTEMtAGcOMPOUNDsUBTRACT      = 0x0000000000000000 :: CULong

-- Bits 59 - 32
iTEMtAGsUBSTANCEIDbITMASK    = 0x0FFFFFFF00000000 :: CULong -- & with this to isolate the substanceId
iTEMtAGsUBSTANCEIDsHIFT      = 32 :: Int

-- Bits 31 - 0
iTEMtAGrEFERENCEbITMASK      = 0x00000000FFFFFFFF :: CULong

nOiTEMtAG                    = 0xFFFFFFFFFFFFFFFF :: CULong
-- Substance Tag Bit Layout for parents
-- Bits | 1 bit                | 7 bit         | 56 bit                           |
-- Type | bool                 | enumerated    | uint                             |
-- Desc | true = additive      | substancetype | substance info record offset     |
--      | false = subtractive  |               | or parent substanceId            |

-- Bit 62 - 56
sUBSTANCEtAGtYPEbITmASK        = 0x7F00000000000000 :: CULong -- & with this to determine if the shapetag is for mask withColor shape or a picture.
sUBSTANCEtAGtYPEsOLIDcOLOR     = 0x0000000000000000 :: CULong -- & with this to determine if the shapetag is additive.
sUBSTANCEtAGtYPEtEXTURE        = 0x0100000000000000 :: CULong
sUBSTANCEtAGtYPElINEARgRADIENT = 0x0200000000000000 :: CULong
sUBSTANCEtAGtYPErADIALgRADIENT = 0x0300000000000000 :: CULong


-- Bits 55 - 0
sUBSTANCEtAGrEFbITMASK     = 0x00FFFFFFFFFFFFFF :: CULong -- & with this to get the reference for the substance tag

nOsUBSTANCEtAG             = 0xFFFFFFFFFFFFFFFF :: CULong

nULLrEFERENCE              = 0xFFFFFFFF :: CUInt
