{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.Constants
    ( mAXtILESpERcALL
    , mAXsTRANDpERtILE
    , sTOCHASTICfACTOR
    , cOMPUTEsIZE
    , mAXtILEsIZE
    , mINtILEsIZE
    , mAXsECTIONsIZE
    , mAXsHAPE
    , mAXtHRESHOLDS
    , rANDOMFIELDsIZE
    , sOURCEfILEpADDING
    , sHAPETAGsUBSTANCEtYPEbITmASK
    , sHAPETAGsUBSTANCEtYPEsOLIDcOLOR
    , sHAPETAGsUBSTANCEtYPEpICTURE
    , sHAPETAGcOMBINEtYPEbITmASK
    , sHAPETAGcOMBINEtYPEcONTINUE
    , sHAPETAGcOMBINEtYPEaDD
    , sHAPETAGcOMBINEtYPEsUBTRACT
    , sHAPEiDbITMASK
    )
where

import Graphics.Gudni.Figure
import Control.Lens
import Foreign.C.Types (CInt, CULong)
import Data.Word

mAXtILESpERcALL  = 512 :: Int -- The maximum number of tiles per kernel call.
mAXsTRANDpERtILE = mAXtHRESHOLDS

sTOCHASTICfACTOR = 0.3 :: Float -- relative amount of variability in an edge.
mAXtHRESHOLDS    = 512 :: Int   -- the size of the threshold header and threshold geometry buffers (must be a power of 2)
mAXsHAPE         = 511 :: Int   -- total number of shapes per build. must be one less than the number of bits available.
cOMPUTEsIZE      = 512 :: Int
mAXtILEsIZE      = Point2 512 512 :: Point2 IntSpace
mINtILEsIZE      = Point2 16  16  :: Point2 IntSpace
mAXsECTIONsIZE   = 16   :: Int
rANDOMFIELDsIZE  = 4096 :: Int -- must be a power of 2

sOURCEfILEpADDING = 32 :: Int -- number of lines at the head of the openCL source file reserved to be replaced by haskell generated preprocessor defines

-- ShapeTag layout
-- Bits 31 - 30
sHAPETAGsUBSTANCEtYPEbITmASK    = 0xC000000000000000 :: CULong
sHAPETAGsUBSTANCEtYPEsOLIDcOLOR = 0x8000000000000000 :: CULong
sHAPETAGsUBSTANCEtYPEpICTURE    = 0x4000000000000000 :: CULong

-- Bits 29 - 28
sHAPETAGcOMBINEtYPEbITmASK      = 0x3000000000000000 :: CULong
sHAPETAGcOMBINEtYPEcONTINUE     = 0x1000000000000000 :: CULong
sHAPETAGcOMBINEtYPEaDD          = 0x2000000000000000 :: CULong
sHAPETAGcOMBINEtYPEsUBTRACT     = 0x3000000000000000 :: CULong

-- Bits 27 - 0
sHAPEiDbITMASK                  = 0x0FFFFFFFFFFFFFFF :: CULong
