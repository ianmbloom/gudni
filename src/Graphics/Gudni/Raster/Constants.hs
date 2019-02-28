{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.Constants
    ( sTOCHASTICfACTOR
    , mAXtILEsIZE
    , mINtILEsIZE
    , dEFAULTNUMMASKS
    , sECTIONsIZE
    , mAXsHAPE
    , mAXtHRESHOLDS
    , cONTINUATIONaLIGN
    , rANDOMFIELDsIZE
    , sHAPETAGsUBSTANCEtYPEbITmASK
    , sHAPETAGsUBSTANCEtYPEsOLIDcOLOR
    , sHAPETAGsUBSTANCEtYPEpICTURE
    , sHAPETAGcOMBINEtYPEbITmASK
    , sHAPETAGcOMBINEtYPEcONTINUE
    , sHAPETAGcOMBINEtYPEaDD
    , sHAPETAGcOMBINEtYPEsUBTRACT
    , sHAPEIDBITMASK
    )
where

import Graphics.Gudni.Figure
import Control.Lens
import Foreign.C.Types
import Data.Word

sTOCHASTICfACTOR = 0.3 :: Float -- relative amount of variability in an edge.
mAXtHRESHOLDS    = 512 :: Int -- the size of the threshold header and threshold geometry buffers (must be a power of 2)
mAXsHAPE         = 511 :: Int-- total number of shapes per build. must be one less than the number of bits available.
mAXtILEsIZE      = Point2 512 512 :: Point2 IntSpace
mINtILEsIZE      = Point2 1 1 :: Point2 IntSpace


dEFAULTNUMMASKS :: Int
dEFAULTNUMMASKS = 16

sECTIONsIZE :: Int
sECTIONsIZE = 16

cONTINUATIONaLIGN :: CInt
cONTINUATIONaLIGN = 40

rANDOMFIELDsIZE :: Int
rANDOMFIELDsIZE = 4096 -- must be a power of 2

-- Bits 31 - 30
sHAPETAGsUBSTANCEtYPEbITmASK    = 0xC000000000000000 :: Word64
sHAPETAGsUBSTANCEtYPEsOLIDcOLOR = 0x8000000000000000 :: Word64
sHAPETAGsUBSTANCEtYPEpICTURE    = 0x4000000000000000 :: Word64

-- Bits 29 - 28
sHAPETAGcOMBINEtYPEbITmASK      = 0x3000000000000000 :: Word64
sHAPETAGcOMBINEtYPEcONTINUE     = 0x1000000000000000 :: Word64
sHAPETAGcOMBINEtYPEaDD          = 0x2000000000000000 :: Word64
sHAPETAGcOMBINEtYPEsUBTRACT     = 0x3000000000000000 :: Word64

-- Bits 27 - 0
sHAPEIDBITMASK                  = 0x0FFFFFFFFFFFFFFF :: Word64
