-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.Setup
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for setting up an OpenCL platform to run the rasterizer kernel.

module Graphics.Gudni.OpenCL.Setup
  ( setupOpenCL
  )
where

import Graphics.Gudni.OpenCL.KernelLibrary
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.Interface.GLInterop
import Graphics.Gudni.OpenCL.CppDefines

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Util.Debug

import Control.Parallel.OpenCL
import Control.Lens
import CLUtil.KernelArgs
import CLUtil


import qualified Data.ByteString.Char8 as BS

instance OpenCLSource BS.ByteString where
  prepSource = BS.unpack

-- | List of definition pragmas to be added to the beggining of the Kernels.cl file.
cppDefines :: RasterSpec -> [CppDefinition]
cppDefines spec =
  [Cpp "STOCHASTIC_FACTOR"                 (CppFloat sTOCHASTICfACTOR                )
  ,Cpp "RANDOMFIELDSIZE"                   (CppInt   rANDOMFIELDsIZE                 )
  ,Cpp "MAXTHRESHOLDS"                     (CppInt   $ spec ^. specMaxThresholds   )
  ,Cpp "MAXSHAPE"                          (CppInt   $ spec ^. specMaxShapes       )
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
  --,Cpp "DEBUG_OUTPUT"                      (CppNothing) -- uncomment this to turn on simple debugging output
  --,Cpp "DEBUG_TRACE"                       (CppNothing) -- uncomment this to turn on parsable debugging output
  ]

-- | Embedded source with implanted definition pragmas.
addDefinesToSource :: RasterSpec -> BS.ByteString -> String
addDefinesToSource spec src = appendCppDefines sOURCEfILEpADDING (cppDefines spec) (BS.unpack src)

-- | This function determines the basic paramters of the rasterizer based on
determineRasterSpec :: CLDeviceID -> IO RasterSpec
determineRasterSpec device =
  do  computeUnits  <- clGetDeviceMaxComputeUnits       device
      maxGroupSize  <- clGetDeviceMaxWorkGroupSize      device
      localMemSize  <- clGetDeviceLocalMemSize          device
      maxBufferSize <- clGetDeviceMaxConstantBufferSize device
      globalMemSize <- clGetDeviceGlobalMemSize         device
      return RasterSpec { _specMaxTileSize     = fromIntegral maxGroupSize
                        , _specThreadsPerTile  = fromIntegral maxGroupSize
                        , _specMaxTilesPerCall = fromIntegral maxGroupSize
                        , _specMaxThresholds   = mAXtHRESHOLDS
                        , _specMaxShapes       = mAXsHAPE
                        }

setupOpenCL :: Bool -> Bool -> BS.ByteString -> IO RasterDevice
setupOpenCL enableProfiling useCLGLInterop src =
  do
      -- List all platforms and all devices.
      queryOpenCL CL_DEVICE_TYPE_ALL
      --let deviceCriteria = deviceNameContains "Iris Pro"  -- select the first Iris Pro GPU
      --let deviceCriteria = deviceNameContains "AMD"       -- select the first AMD GPU
      let deviceCriteria = return . const True -- select the first available GPU
      putStrLn $ "Initializing OpenCL device."
      mState <- if useCLGLInterop
               then Just <$> initFromGL CL_DEVICE_TYPE_ALL -- initialize an OpenCL state for the device that is available for CL-GL-Interop
               else clDeviceSelect CL_DEVICE_TYPE_GPU deviceCriteria -- initialize an OpenCL state for the first GPU that meets the criteria.
      case mState of
        Nothing -> error "Could not initialize openCL device."
        Just state ->
          do -- Output all of the device information to the terminal.
             putStrLn $ "Finished initializing OpenCL device."
             dumpOpenCLDeviceInfo . clDevice $ state
             -- Compiler options for OpenCL compiler.
             let options = [ CLFastRelaxedMath
                           , CLStrictAliasing
                           --, CLWarningIntoError
                           ]
             -- Get metadata from the openCL device.
             let device = clDevice state
             rasterSpec <- determineRasterSpec device
             let modifiedSrc = addDefinesToSource rasterSpec src
             -- Compile the source.
             putStrLn $ "Starting OpenCL kernel compile"
             program <- loadProgramWOptions options state modifiedSrc
             putStrLn $ "Finished OpenCL kernel compile"
             -- get the rasterizer kernel.
             rasterKernel <- program "multiTileRaster"

             -- Return a Library constructor with relevant information about the device for the rasterizer.
             return RasterDevice { _clState = state
                                 , _multiTileRasterCL = rasterKernel
                                 , _clUseGLInterop = useCLGLInterop
                                 , _clSpec = rasterSpec
                                 }
