{-# LANGUAGE TemplateHaskell     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for embedding OpenCL source in the compiled Haskell library.

module Graphics.Gudni.OpenCL.EmbeddedOpenCLSource
  ( openCLSourceWithDefines
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.FileEmbed
import Graphics.Gudni.OpenCL.CppDefines
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Util.Debug

-- | List of definition pragmas to be added to the beggining of the Kernels.cl file.
cppDefines =
  [Cpp "STOCHASTIC_FACTOR"                 (CppFloat sTOCHASTICfACTOR                )
  ,Cpp "MAXTHRESHOLDS"                     (CppInt   mAXtHRESHOLDS                   )
  ,Cpp "MAXSHAPE"                          (CppInt   mAXsHAPE                        )
  ,Cpp "RANDOMFIELDSIZE"                   (CppInt   rANDOMFIELDsIZE                 )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_BITMASK"    (CppHex64 sHAPETAGsUBSTANCEtYPEbITmASK    )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_SOLIDCOLOR" (CppHex64 sHAPETAGsUBSTANCEtYPEsOLIDcOLOR )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_PICTURE"    (CppHex64 sHAPETAGsUBSTANCEtYPEpICTURE    )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_SHIFT"      (CppInt   sHAPETAGsUBSTANCETYPEsHIFT      )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_BITMASK"     (CppHex64 sHAPETAGcOMPOUNDtYPEbITmASK     )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_CONTINUE"    (CppHex64 sHAPETAGcOMPOUNDtYPEcONTINUE    )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_ADD"         (CppHex64 sHAPETAGcOMPOUNDtYPEaDD         )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_SUBTRACT"    (CppHex64 sHAPETAGcOMPOUNDtYPEsUBTRACT    )
  ,Cpp "SHAPETAG_COMPOUNDTYPE_SHIFT"       (CppInt   sHAPETAGcOMPOUNDtYPEsHIFT       )
  ,Cpp "SHAPETAG_SUBSTANCEID_BITMASK"      (CppHex64 sHAPETAGsUBSTANCEIDbITMASK      )
  ]

-- | Raw embedded source from the Kernels.cl file.
embeddedOpenCLSource :: BS.ByteString
embeddedOpenCLSource = $(embedFile "src/Graphics/Gudni/OpenCL/Kernels.cl")

-- | Embedded source with implanted definition pragmas.
openCLSourceWithDefines :: String
openCLSourceWithDefines = appendCppDefines sOURCEfILEpADDING cppDefines (BS.unpack embeddedOpenCLSource)
