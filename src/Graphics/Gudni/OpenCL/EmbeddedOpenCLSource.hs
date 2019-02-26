{-# LANGUAGE TemplateHaskell     #-}

module Graphics.Gudni.OpenCL.EmbeddedOpenCLSource
  ( embeddedOpenCLSource
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.FileEmbed

embeddedOpenCLSource :: BS.ByteString
embeddedOpenCLSource =   $(embedFile "src/Graphics/Gudni/OpenCL/Kernels.cl")
