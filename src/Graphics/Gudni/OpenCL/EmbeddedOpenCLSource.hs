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
  [Cpp "MAXTHRESHOLDS" (CppInt mAXtHRESHOLDS)
  ,Cpp "MAXSHAPE"      (CppInt mAXsHAPE)
  ]

embeddedOpenCLSource :: BS.ByteString
embeddedOpenCLSource = $(embedFile "src/Graphics/Gudni/OpenCL/Kernels.cl")

openCLSourceWithDefines = trWith id "openCLSourceWithDefines" $ appendCppDefines 10 cppDefines (BS.unpack embeddedOpenCLSource)
