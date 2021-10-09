{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for embedding OpenCL source in the compiled Haskell library.

module Graphics.Gudni.Raster.OpenCL.EmbeddedOpenCLSource
  ( embeddedOpenCLSource
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.FileEmbed

-- | Raw embedded source from the Kernels.cl file.
embeddedOpenCLSource :: BS.ByteString
embeddedOpenCLSource = $(embedFile "src/Graphics/Gudni/Raster/OpenCL/Kernels.cl")
