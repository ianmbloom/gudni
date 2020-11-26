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
    ( tAXICABfLATNESS
    , cROSSsPLITlIMIT
    , oPAQUEtHRESHOLD
    , rANDOMFIELDsIZE
    , bEZIERsTACKsIZE
    , sHAPEsTACKsIZE
    , fABRICsTACKsIZE
    , cOLORsTACKsIZE
    , cONFINEtREEsTACKsIZE
    , bEZIERsIZEiNfLOATS
    , fACETsIZEiNfLOATS
    , bOXsIZEiNfLOATS
    , sOURCEfILEpADDING
    , dEBUG0
    , dEBUG1

    , ShapeId_
    , nULLrEFERENCE
    , nULLdECOtADiD
    , nULLcONFINEtAGiD
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
    , SubstanceTag_
    , fABRICtYPEbITMASK
    , fABRICiSlEAF
    , fABRICiSuNARYpRE
    , fABRICiSuNARYpOST
    , fABRICiSbINARY
    , fABRICsUBtYPEbITMASK
    , fABRICiStREE
    , fABRICiStRANSFORMaFFINE
    , fABRICiStRANSFORMfACET
    , fABRICiStRANSFORMcONVOLVE
    , fABRICiSsQRT
    , fABRICiSiNVERT
    , fABRICiScOS
    , fABRICiSsIN
    , fABRICiScLAMP
    , fABRICiScOMPOSITE
    , fABRICiSmULT
    , fABRICiSaDD
    , fABRICiSfLOAToR
    , fABRICiSfLOATxOR
    , fABRICiSmIN
    , fABRICiSmAX
    , fABRICiShSVaDJUST
    , fABRICiStRANSPARENT
    , fABRICiScONSTANT
    , fABRICiStEXTURE
    , fABRICiSlINEAR
    , fABRICiSqUADRANCE
    , fABRICtAGdATAbITMASK
    , fABRICtAGhIGHiDsHIFT
    , fABRICtAGhIGHiDbITMASK
    , fABRICtAGlOWiDbITMASK
    , nULLfABRICtAGiD
    )
where

import Foreign.C.Types (CUInt, CULong)

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

tAXICABfLATNESS      = (1/16384) :: Float -- minimum taxicab distance between (relative to the pixel size) where curve tesselation terminates
cROSSsPLITlIMIT      = (1/16384) :: Float
oPAQUEtHRESHOLD      = (1/256)   :: Float
rANDOMFIELDsIZE      = 4096      :: Int   -- must be a power of 2
bEZIERsTACKsIZE      = 4         :: Int
sHAPEsTACKsIZE       = 64        :: Int
fABRICsTACKsIZE      = 64        :: Int
cOLORsTACKsIZE       = 64        :: Int
cONFINEtREEsTACKsIZE = 64        :: Int
bEZIERsIZEiNfLOATS   = 6         :: Int
fACETsIZEiNfLOATS    = 18        :: Int
bOXsIZEiNfLOATS      = 4         :: Int
sOURCEfILEpADDING    = 90        :: Int   -- number of lines at the head of the openCL source file reserved to be replaced by haskell generated preprocessor defines
dEBUG0               = 400       :: Int
dEBUG1               = 400       :: Int

instance Out CULong where
    doc x = text . show $ x
    docPrec _ = doc

type ShapeId_ = CUInt

nULLrEFERENCE    = 0xFFFFFFFF :: CUInt
nULLdECOtADiD    = 0xFFFFFFFF :: CUInt
nULLcONFINEtAGiD = 0xFFFFFFFF :: CUInt
nULLsHAPEiD      = 0xFFFFFFFF :: ShapeId_

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
-- Bits | 2 bit | 6 bit    | 28 bit      | 28 bit     |
-- Type | int   | int      | uint        | uint       |
-- Desc | type  | subtype  | high        | low        |
--      |       |          |             |            |

type FabricTag_   = CULong
type SubstanceTag_ = FabricTag_

-- Bit 63 - 60
fABRICtYPEbITMASK         = 0xC000000000000000 :: CULong -- & with this to get the fabric type
-- Basic node types


fABRICiSlEAF              = 0x0000000000000000 :: CULong
fABRICiSuNARYpRE          = 0x4000000000000000 :: CULong
fABRICiSuNARYpOST         = 0x8000000000000000 :: CULong
fABRICiSbINARY            = 0xC000000000000000 :: CULong

-- Node Subtypes
fABRICsUBtYPEbITMASK  = 0x3F00000000000000 :: CULong

-- Unary Pre
fABRICiStREE              = 0x0000000000000000 :: CULong
fABRICiStRANSFORMaFFINE   = 0x0100000000000000 :: CULong
fABRICiStRANSFORMfACET    = 0x0200000000000000 :: CULong
fABRICiStRANSFORMcONVOLVE = 0x0300000000000000 :: CULong

-- Unary Post
fABRICiSsQRT              = 0x0000000000000000 :: CULong
fABRICiSiNVERT            = 0x0100000000000000 :: CULong
fABRICiScOS               = 0x0200000000000000 :: CULong
fABRICiSsIN               = 0x0300000000000000 :: CULong
fABRICiScLAMP             = 0x0400000000000000 :: CULong

-- Binary
fABRICiScOMPOSITE         = 0x0000000000000000 :: CULong
fABRICiSmULT              = 0x0100000000000000 :: CULong
fABRICiSaDD               = 0x0200000000000000 :: CULong
fABRICiSfLOAToR           = 0x0300000000000000 :: CULong
fABRICiSfLOATxOR          = 0x0400000000000000 :: CULong
fABRICiSmIN               = 0x0500000000000000 :: CULong
fABRICiSmAX               = 0x0600000000000000 :: CULong
fABRICiShSVaDJUST         = 0x0700000000000000 :: CULong
fABRICiStRANSPARENT       = 0x0800000000000000 :: CULong

-- Leaves
fABRICiScONSTANT          = 0x0000000000000000 :: CULong
fABRICiStEXTURE           = 0x0100000000000000 :: CULong
fABRICiSlINEAR            = 0x0200000000000000 :: CULong
fABRICiSqUADRANCE         = 0x0300000000000000 :: CULong

-- Bit 59 - 0
fABRICtAGdATAbITMASK      = 0x00FFFFFFFFFFFFFF :: CULong -- & with this to get the whole data section of the tag

-- Bit 59 - 30
fABRICtAGhIGHiDsHIFT      = 28 :: Int
fABRICtAGhIGHiDbITMASK    = 0x00FFFFFFF0000000 :: CULong
-- Bit 29 - 0
fABRICtAGlOWiDbITMASK     = 0x000000000FFFFFFF :: CULong

nULLfABRICtAGiD           = 0x0FFFFFFF :: CUInt
