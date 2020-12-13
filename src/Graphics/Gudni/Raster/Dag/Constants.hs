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
    , bEZIERsTACKsIZE
    , fABRICsTACKsIZE
    , aNSWERsTACKsIZE
    , cONFINEsTACKsIZE
    , bEZIERsIZEiNfLOATS
    , fACETsIZEiNfLOATS
    , bOXsIZEiNfLOATS
    , sOURCEfILEpADDING
    , dEBUG0
    , dEBUG1


    , nULLrEFERENCE
    , nULLdECOtAGiD
    , nULLcONFINEtAGiD

    , CodeCounter_
    , FabricTag_
    , TransformId_
    , SubstanceTag_
    , fABRICtYPEbITMASK
    , fABRICiSrETURN
    , fABRICiScONSTANT
    , fABRICiStEXTURE
    , fABRICiSfUNCTION
    , fABRICiSbINARY
    , fABRICiSuNARYpOST
    , fABRICiSdECOtREE
    , fABRICiScONFINEtREE
    , fABRICiSsTACKER
    , fABRICiSaFFINE
    , fABRICiSfACET
    , fABRICiScONVOLVE
    , fABRICiScOMPOSITE
    , fABRICiSmULT
    , fABRICiSaDD
    , fABRICiSfLOAToR
    , fABRICiSfLOATxOR
    , fABRICiSmIN
    , fABRICiSmAX
    , fABRICiShSVaDJUST
    , fABRICiStRANSPARENT
    , fABRICiSsQRT
    , fABRICiSiNVERT
    , fABRICiScOS
    , fABRICiSsIN
    , fABRICiScLAMP
    , fABRICiSlINEAR
    , fABRICiSqUADRANCE
    , fABRICtAGdATAbITMASK
    , nULLfABRICtAGiD
    )
where

import Foreign.C.Types (CInt, CUInt, CULong)

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

tAXICABfLATNESS  = (1/4) :: Float -- minimum taxicab distance between (relative to the pixel size) where curve tesselation terminates
cROSSsPLITlIMIT  = (1/268435456) :: Float
oPAQUEtHRESHOLD  = (1/256)   :: Float

bEZIERsTACKsIZE  = 4  :: Int -- this should be 2 for quadratic and 4 for cubic beziers
fABRICsTACKsIZE  = 8  :: Int
aNSWERsTACKsIZE  = 8  :: Int
cONFINEsTACKsIZE = 64 :: Int

bEZIERsIZEiNfLOATS =  6 :: Int
fACETsIZEiNfLOATS  = 18 :: Int
bOXsIZEiNfLOATS    =  4 :: Int
sOURCEfILEpADDING  = 90 :: Int   -- number of lines at the head of the openCL source file reserved to be replaced by haskell generated preprocessor defines
dEBUG0             =  4 :: Int
dEBUG1             =  4 :: Int

instance Out CULong where
    doc x = text . show $ x
    docPrec _ = doc

type CodeCounter_ = CInt

nULLrEFERENCE    = 0xFFFFFFFF :: CUInt
nULLdECOtAGiD    = 0xFFFFFFFF :: CUInt
nULLcONFINEtAGiD = 0xFFFFFFFF :: CUInt

-- Fabric Tag Bit Layout
-- Depending on Type there are two layouts
-- Bits | 2 bit | 6 bit    | 28 bit      | 28 bit     |
-- Type | int   | int      | uint        | uint       |
-- Desc | type  | subtype  | high        | low        |
--      |       |          |             |            |

type FabricTag_    = CUInt
type TransformId_  = FabricTag_
type SubstanceTag_ = FabricTag_

-- Bit 63 - 60
fABRICtYPEbITMASK   = 0xF0000000 :: CUInt -- & with this to get the fabric type
-- Basic fabric types
fABRICiSrETURN      = 0x00000000 :: CUInt
fABRICiScONSTANT    = 0x10000000 :: CUInt
fABRICiStEXTURE     = 0x20000000 :: CUInt
fABRICiSfUNCTION    = 0x30000000 :: CUInt
fABRICiSbINARY      = 0x40000000 :: CUInt
fABRICiSdECOtREE    = 0x50000000 :: CUInt
fABRICiScONFINEtREE = 0x60000000 :: CUInt
fABRICiSsTACKER     = 0x70000000 :: CUInt
fABRICiSaFFINE      = 0x80000000 :: CUInt
fABRICiSfACET       = 0x90000000 :: CUInt
fABRICiScONVOLVE    = 0xA0000000 :: CUInt
fABRICiSuNARYpOST   = 0xB0000000 :: CUInt

-- Binary
fABRICiScOMPOSITE   = 0x00000000 :: CUInt
fABRICiSmULT        = 0x00000001 :: CUInt
fABRICiSaDD         = 0x00000002 :: CUInt
fABRICiSfLOAToR     = 0x00000003 :: CUInt
fABRICiSfLOATxOR    = 0x00000004 :: CUInt
fABRICiSmIN         = 0x00000005 :: CUInt
fABRICiSmAX         = 0x00000006 :: CUInt
fABRICiShSVaDJUST   = 0x00000007 :: CUInt
fABRICiStRANSPARENT = 0x00000008 :: CUInt

-- Unary Post
fABRICiSsQRT        = 0x00000000 :: CUInt
fABRICiSiNVERT      = 0x00000001 :: CUInt
fABRICiScOS         = 0x00000002 :: CUInt
fABRICiSsIN         = 0x00000003 :: CUInt
fABRICiScLAMP       = 0x00000004 :: CUInt

-- Function Substances
fABRICiSlINEAR      = 0x00000000 :: CUInt
fABRICiSqUADRANCE   = 0x00000001 :: CUInt

-- Bit 59 - 0
fABRICtAGdATAbITMASK = 0x0FFFFFFF :: CUInt -- & with this to get the whole data section of the tag

nULLfABRICtAGiD      = fABRICtAGdATAbITMASK
