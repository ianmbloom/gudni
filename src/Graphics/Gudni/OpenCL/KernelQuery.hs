{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TemplateHaskell     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.KernelQuery
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Boilerplate galore for querying OpenCL compiled kernel specifications.

module Graphics.Gudni.OpenCL.KernelQuery
  ( initCLKenrelDetail
  , dumpKernelDetail
  )
where

import CLUtil.CL
import Control.Parallel.OpenCL.Program
import Control.Exception (handle, throw)
import Control.Monad
import Control.Parallel.OpenCL
import Foreign.C.Types (CSize)
import Graphics.Gudni.Util.Util

import Data.List
import Data.Maybe (catMaybes)
import Control.Lens

data CLKernelDetail = CLKernelDetail
    { _clKernelName                    :: String
    , _clKernelWorkgroupSize           :: CSize
    , _clKernelCompileWorkGroupSize    :: [CSize]
    , _clKernelLocalMemSize            :: CLulong
    } deriving (Show)
makeLenses ''CLKernelDetail

-- | Create a CLDeviceDetail record by querying an openCL device.
initCLKenrelDetail :: CLKernel -> CLDeviceID -> IO CLKernelDetail
initCLKenrelDetail kernel deviceId =
  do  kernelName                       <- clGetKernelFunctionName          kernel
      kernelWorkgroupSize              <- clGetKernelWorkGroupSize         kernel deviceId
      kernelCompileWorkGroupSize       <- clGetKernelCompileWorkGroupSize  kernel deviceId
      kernelLocalMemSize               <- clGetKernelLocalMemSize          kernel deviceId
      return CLKernelDetail
        { _clKernelName                 = kernelName
        , _clKernelWorkgroupSize        = kernelWorkgroupSize
        , _clKernelCompileWorkGroupSize = kernelCompileWorkGroupSize
        , _clKernelLocalMemSize         = kernelLocalMemSize
        }

dumpKernelDetail :: CLKernelDetail -> String
dumpKernelDetail kernelDetail =
      (titleLine (show (kernelDetail ^. clKernelName)) ++) $
      foldl1 (++) $ map infoLine $
        [ (    "       KernelWorkgroupSize", show $ kernelDetail ^. clKernelWorkgroupSize            )
        , (    "KernelCompileWorkGroupSize", show $ kernelDetail ^. clKernelCompileWorkGroupSize     )
        , (    "        KernelLocalMemSize", show $ kernelDetail ^. clKernelLocalMemSize             )
        ]