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
  [Cpp "STOCHASTIC_FACTOR"              (CppFloat sTOCHASTICfACTOR            )
  ,Cpp "TAXICAB_FLATNESS"               (CppFloat tAXICABfLATNESS             )
  ,Cpp "RANDOMFIELDSIZE"                (CppInt   rANDOMFIELDsIZE             )
  ,Cpp "MAXTHRESHOLDS"                  (CppInt   $ spec ^. specMaxThresholds )
  ,Cpp "MAXLAYERS"                      (CppInt   $ spec ^. specMaxLayers     )
  ,Cpp "LAYERFLAGSSECTIONS"             (CppInt   $ lAYERfLAGSsECTIONS        )
  ,Cpp "ITEMTAG_ISFACET_BITMASK"        (CppHex64 iTEMtAGiSfACETbITMASK       )
  ,Cpp "ITEMTAG_ISSHAPE"                (CppHex64 iTEMtAGiSsHAPE              )
  ,Cpp "ITEMTAG_ISFACET"                (CppHex64 iTEMtAGiSfACET              )
  ,Cpp "ITEMTAG_COMPOUNDTYPE_BITMASK"   (CppHex64 iTEMtAGcOMPOUNDtYPEbITMASK  )
  ,Cpp "ITEMTAG_COMPOUNDTYPE_ADD"       (CppHex64 iTEMtAGcOMPOUNDtYPEaDD      )
  ,Cpp "ITEMTAG_COMPOUNDTYPE_SUBTRACT"  (CppHex64 iTEMtAGcOMPOUNDtYPEsUBTRACT )
  ,Cpp "ITEMTAG_SUBSTANCE_ID_BITMASK"   (CppHex64 iTEMtAGsUBSTANCEIDbITMASK   )
  ,Cpp "ITEMTAG_SUBSTANCE_ID_SHIFT"     (CppInt   iTEMtAGsUBSTANCEIDsHIFT     )
  ,Cpp "ITEMTAG_ITEM_ID_BITMASK"        (CppHex64 iTEMtAGiTEMiDbITMASK        )
  ,Cpp "SUBSTANCETAG_TYPE_BITMASK"      (CppHex64 sUBSTANCEtAGtYPEbITmASK     )
  ,Cpp "SUBSTANCETAG_TYPE_SOLID_COLOR"  (CppHex64 sUBSTANCEtAGtYPEsOLIDcOLOR  )
  ,Cpp "SUBSTANCETAG_TYPE_TEXTURE"      (CppHex64 sUBSTANCEtAGtYPEtEXTURE     )
  ,Cpp "SUBSTANCETAG_REF_BITMASK"       (CppHex64 sUBSTANCEtAGrEFbITMASK      )
  ,Cpp "DEBUG_OUTPUT"                   (CppNothing) -- uncomment this to turn on simple debugging output
--,Cpp "DEBUG_TRACE"                    (CppNothing) -- uncomment this to turn on parsable debugging output
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
                        , _specMaxStrandsPerTile = tr "maxStrandsPerTile" $ maxThresholds - 2
                        , _specMaxLayers         = mAXlAYERS
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
              sortThresholdsKernel     <- program "sortThresholds"
              renderThresholdsKernel   <- program "renderThresholds"
              -- Return a Library constructor with relevant information about the device for the rasterizer.
              return Rasterizer { _rasterClState  = state
                                , _rasterGenerateThresholdsKernel = generateThresholdsKernel
                                , _rasterSortThresholdsKernel     = sortThresholdsKernel
                                , _rasterRenderThresholdsKernel   = renderThresholdsKernel
                                , _rasterUseGLInterop = useCLGLInterop
                                , _rasterSpec = rasterSpec
                                }
