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
    ( mAXtILESpERcALL
    , mAXsTRANDpERtILE
    , sTOCHASTICfACTOR
    , mAXtILEsIZE
    , mINtILEsIZE
    , mAXsECTIONsIZE
    , mAXsHAPE
    , mAXtHRESHOLDS
    , rANDOMFIELDsIZE
    , sOURCEfILEpADDING
    , iNITgEOMETRYpILEsIZE
    , sHAPETAGsUBSTANCEtYPEbITmASK
    , sHAPETAGsUBSTANCEtYPEsOLIDcOLOR
    , sHAPETAGsUBSTANCEtYPEpICTURE
    , sHAPETAGsUBSTANCETYPEsHIFT
    , sHAPETAGcOMPOUNDtYPEbITmASK
    , sHAPETAGcOMPOUNDtYPEcONTINUE
    , sHAPETAGcOMPOUNDtYPEaDD
    , sHAPETAGcOMPOUNDtYPEsUBTRACT
    , sHAPETAGcOMPOUNDtYPEsHIFT
    , sHAPETAGsUBSTANCEIDbITMASK
    )
where

import Graphics.Gudni.Figure
import Control.Lens
import Foreign.C.Types (CInt, CUInt, CULong)
import Data.Word

mAXtILESpERcALL  = 512 :: Int -- The maximum number of tiles per kernel call.
mAXsTRANDpERtILE = mAXtHRESHOLDS -- The maximum number of strands per tile, before it splits.

sTOCHASTICfACTOR = 0.3 :: Float -- relative amount of variability in an edge.
mAXtHRESHOLDS    = 1024 :: Int -- the size of the threshold header and threshold geometry buffers (must be a power of 2)
mAXsHAPEbITS     = 511 :: Int -- total number of shapes per build based on the number of bits in the stack must be one less than the number of bits available.
sHAPElIMIT       = 127 :: Int -- a hard limit on the number of shapes based on the empirical testing of the timeout period of the GPU.
mAXsHAPE         = min mAXsHAPEbITS sHAPElIMIT
mAXtILEsIZE      = Point2 512 512 :: Point2 PixelSpace
mINtILEsIZE      = Point2 16  16  :: Point2 PixelSpace
mAXsECTIONsIZE   = 32   :: Int
rANDOMFIELDsIZE  = 4096 :: Int -- must be a power of 2

sOURCEfILEpADDING = 40 :: Int -- number of lines at the head of the openCL source file reserved to be replaced by haskell generated preprocessor defines

iNITgEOMETRYpILEsIZE = 65536 :: Int

-- ShapeTag layout
-- Bits 31 - 30
sHAPETAGsUBSTANCEtYPEbITmASK    = 0xC000000000000000 :: CULong -- bit mask for isolating the substance type in a shape tag.
sHAPETAGsUBSTANCEtYPEsOLIDcOLOR = 0x8000000000000000 :: CULong -- flag for solid color substance type.
sHAPETAGsUBSTANCEtYPEpICTURE    = 0x4000000000000000 :: CULong -- flag for picture substance type.
sHAPETAGsUBSTANCETYPEsHIFT      = 30 :: Int

-- Bits 29 - 28
sHAPETAGcOMPOUNDtYPEbITmASK   = 0x3000000000000000 :: CULong -- bit mask for isolating the compounding type of a shape in a shape tag.
sHAPETAGcOMPOUNDtYPEcONTINUE  = 0x1000000000000000 :: CULong -- flag treating a shape as a nuetral concatenation of outlines to the previous shape.
sHAPETAGcOMPOUNDtYPEaDD       = 0x2000000000000000 :: CULong -- flag for adding the shape to the shape below it.
sHAPETAGcOMPOUNDtYPEsUBTRACT  = 0x3000000000000000 :: CULong -- flag for subtracting the shape from the shape below it.
sHAPETAGcOMPOUNDtYPEsHIFT     = 28 :: Int

-- Bits 27 - 0
sHAPETAGsUBSTANCEIDbITMASK    = 0x0FFFFFFFFFFFFFFF :: CULong -- bit mask for isolating the shape id from the shape tag.
