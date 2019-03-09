{-# LANGUAGE TemplateHaskell     #-}

module Graphics.Gudni.OpenCL.EmbeddedOpenCLSource
  ( openCLSourceWithDefines
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.FileEmbed
import Graphics.Gudni.OpenCL.CppDefines
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Util.Debug

cppDefines =
  [Cpp "STOCHASTIC_FACTOR"                 (CppFloat sTOCHASTICfACTOR                )
  ,Cpp "MAXTHRESHOLDS"                     (CppInt   mAXtHRESHOLDS                   )
  ,Cpp "MAXSHAPE"                          (CppInt   mAXsHAPE                        )
  ,Cpp "RANDOMFIELDSIZE"                   (CppInt   rANDOMFIELDsIZE                 )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_BITMASK"    (CppHex64 sHAPETAGsUBSTANCEtYPEbITmASK    )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_SOLIDCOLOR" (CppHex64 sHAPETAGsUBSTANCEtYPEsOLIDcOLOR )
  ,Cpp "SHAPETAG_SUBSTANCETYPE_PICTURE"    (CppHex64 sHAPETAGsUBSTANCEtYPEpICTURE    )
  ,Cpp "SHAPETAG_COMBINETYPE_BITMASK"      (CppHex64 sHAPETAGcOMBINEtYPEbITmASK      )
  ,Cpp "SHAPETAG_COMBINETYPE_CONTINUE"     (CppHex64 sHAPETAGcOMBINEtYPEcONTINUE     )
  ,Cpp "SHAPETAG_COMBINETYPE_ADD"          (CppHex64 sHAPETAGcOMBINEtYPEaDD          )
  ,Cpp "SHAPETAG_COMBINETYPE_SUBTRACT"     (CppHex64 sHAPETAGcOMBINEtYPEsUBTRACT     )
  ,Cpp "SHAPEIDBITMASK"                    (CppHex64 sHAPEiDbITMASK                  )
  ]

embeddedOpenCLSource :: BS.ByteString
embeddedOpenCLSource =      $(embedFile "src/Graphics/Gudni/OpenCL/Kernels.cl")

openCLSourceWithDefines :: String
openCLSourceWithDefines = appendCppDefines sOURCEfILEpADDING cppDefines (BS.unpack embeddedOpenCLSource)
