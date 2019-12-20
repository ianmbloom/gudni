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
-- Including macro definitions that are implanted into OpenCL source code.

module Graphics.Gudni.OpenCL.Setup
  ( setupOpenCL
  )
where

import Graphics.Gudni.OpenCL.Rasterizer
import Graphics.Gudni.OpenCL.DeviceQuery
import Graphics.Gudni.Interface.GLInterop
import Graphics.Gudni.OpenCL.CppDefines

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Util.Debug

import Control.Parallel.OpenCL
import Control.Lens
import CLUtil.KernelArgs
import CLUtil
import CLUtil.Initialization

--import Graphics.Gudni.OpenCL.DeviceQuery

import Data.List
import Data.Maybe
import Foreign.Storable

import qualified Data.ByteString.Char8 as BS

instance OpenCLSource BS.ByteString where
  prepSource = BS.unpack

-- | List of definition pragmas to be added to the beggining of the Kernels.cl file.
cppDefines :: RasterSpec -> [CppDefinition]
cppDefines spec =
  [Cpp "STOCHASTIC_FACTOR"                 (CppFloat sTOCHASTICfACTOR                )
  ,Cpp "RANDOMFIELDSIZE"                   (CppInt   rANDOMFIELDsIZE                 )
  ,Cpp "MAXTHRESHOLDS"                     (CppInt   $ spec ^. specMaxThresholds     )
  ,Cpp "MAXSHAPE"                          (CppInt   $ spec ^. specMaxShapes         )
  ,Cpp "SHAPESTACKSECTIONS"                (CppInt   $ sHAPEsTACKsECTIONS            )
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
      maxMemAllocSize <- clGetDeviceMaxMemAllocSize     device
      -- The maximum number of threads that each tile can store is the maximum allocation size
      let maxThresholds = fromIntegral maxMemAllocSize `div` ((fromIntegral maxGroupSize ^ 2) * sizeOf (undefined :: THRESHOLDTYPE))
      return RasterSpec { _specMaxTileSize     = fromIntegral maxGroupSize
                        , _specThreadsPerTile  = fromIntegral maxGroupSize
                        , _specMaxTilesPerCall = fromIntegral maxGroupSize
                        , _specMaxThresholds   = tr "maxThresholds" $ maxThresholds
                        , _specMaxStrandsPerTile = tr "maxStrandsPerTile" $ maxThresholds
                        , _specMaxShapes       = mAXsHAPE
                        }

-- | Order Devices based on the number of compute units
orderGPU :: CLDeviceDetail -> CLDeviceDetail -> Ordering
orderGPU gpu1 gpu2 = compare (gpu2 ^. clDeviceMaxComputeUnits) (gpu1 ^. clDeviceMaxComputeUnits)

-- | Determine if the Device Name of a particular device contains a specific string.
deviceNameContains :: String -> CLDeviceDetail -> Bool
deviceNameContains selector deviceInfo = isInfixOf selector (deviceInfo ^. clDeviceName)

-- | Select a device the best device among a set of qualified.
deviceSelect :: (CLDeviceDetail -> Bool) -> (CLDeviceDetail -> CLDeviceDetail -> Ordering) -> [CLDeviceDetail] -> Maybe CLDeviceDetail
deviceSelect qualified order details = listToMaybe . sortBy order . filter qualified $ details

-- | Create a Rasterizer by setting up an OpenCL device.
setupOpenCL :: Bool -> Bool -> BS.ByteString -> IO Rasterizer
setupOpenCL enableProfiling useCLGLInterop src =
  do
      putStrLn $ "Initializing OpenCL device."
      -- Create Detail Records for every available device.
      details <- queryOpenCL CL_DEVICE_TYPE_GPU
      -- List all platforms and all devices.
      --mapM (putStrLn . show) details
      -- Filter function for qualified devices to select.
      --let deviceFilter = deviceNameContains "Iris Pro"  -- select the first Iris Pro GPU
      --let deviceFilter = deviceNameContains "Vega"        -- select the first AMD GPU
      let deviceFilter = const True -- all GPUs qualify
      -- Try to select the best qualified device.
      case deviceSelect deviceFilter orderGPU details of
        Nothing -> error "No GPU qualifies based on filter criteria."
        Just detail ->
          do  state <- if useCLGLInterop
                       then initFromGL CL_DEVICE_TYPE_ALL -- initialize an OpenCL state for the device that is available for CL-GL-Interop
                       else clStateInit (detail ^. clDeviceDeviceId) -- initialize an OpenCL state for the first GPU that meets the criteria.
              putStrLn $ "Finished initializing OpenCL device."
              putStrLn $ dumpDeviceDetail detail
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
              generateThresholdsKernel <- program "generateThresholds"
              renderThresholdsKernel   <- program "renderThresholds"
              -- Return a Library constructor with relevant information about the device for the rasterizer.
              return Rasterizer { _rasterClState  = state
                                , _rasterGenerateThresholdsKernel = generateThresholdsKernel
                                , _rasterRenderThresholdsKernel   = renderThresholdsKernel
                                , _rasterUseGLInterop = useCLGLInterop
                                , _rasterSpec = rasterSpec
                                }
