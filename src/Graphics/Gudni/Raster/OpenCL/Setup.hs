-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.OpenCL.Setup
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for setting up an OpenCL platform to run the rasterizer kernel.
-- Including macro definitions that are implanted into OpenCL source code.

module Graphics.Gudni.Raster.OpenCL.Setup
  ( setupOpenCL
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Substance.Color
import Graphics.Gudni.Raster.ConfineTree.Primitive.Constants
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.OpenCL.Rasterizer
import Graphics.Gudni.Raster.OpenCL.Util.DeviceQuery
import Graphics.Gudni.Raster.OpenCL.Util.KernelQuery
import Graphics.Gudni.Raster.OpenCL.Util.CppDefines
import Graphics.Gudni.Interface.GLInterop

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Parallel.OpenCL
import Control.Lens
import CLUtil.KernelArgs
import CLUtil
import CLUtil.Initialization
import CLUtil.CL

--import Graphics.Gudni.Raster.OpenCL.Util.DeviceQuery

import Data.List
import Data.Maybe
import Foreign.Storable
import Linear.V4

import qualified Data.ByteString.Char8 as BS

instance OpenCLSource BS.ByteString where
  prepSource = BS.unpack

cLEARbLACK  :: V4 Float
cLEARbLACK  = fmap realToFrac . view unColor $ (clearBlack  :: Color SubSpace)
oPAQUEwHITE :: V4 Float
oPAQUEwHITE = fmap realToFrac . view unColor $ (opaqueWhite :: Color SubSpace)

-- | List of definition pragmas to be added to the beggining of the Kernels.cl file.
cppDefines :: DeviceSpec -> [CppDefinition]
cppDefines spec =
  [ Cpp "tAXICABfLATNESS"         (CppFloat  tAXICABfLATNESS         )
  , Cpp "cROSSsPLITlIMIT"         (CppFloat  cROSSsPLITlIMIT         )
  , Cpp "oPAQUEtHRESHOLD"         (CppFloat  oPAQUEtHRESHOLD         )
  , Cpp "bEZIERsTACKsIZE"         (CppInt    bEZIERsTACKsIZE         )
  , Cpp "fABRICsTACKsIZE"         (CppInt    fABRICsTACKsIZE         )
  , Cpp "aNSWERsTACKsIZE"         (CppInt    aNSWERsTACKsIZE         )
  , Cpp "cONFINEsTACKsIZE"        (CppInt    cONFINEsTACKsIZE        )
  , Cpp "bEZIERsIZEiNfLOATS"      (CppInt    bEZIERsIZEiNfLOATS      )
  , Cpp "fACETsIZEiNfLOATS"       (CppInt    fACETsIZEiNfLOATS       )
  , Cpp "bOXsIZEiNfLOATS"         (CppInt    bOXsIZEiNfLOATS         )
  , Cpp "cLEARbLACK"              (CppFloat4 cLEARbLACK              )
  , Cpp "oPAQUEwHITE"             (CppFloat4 oPAQUEwHITE             )
  , Cpp "nULLdECOtAGiD"           (CppHex32  nULLdECOtAGiD           )
  , Cpp "nULLcONFINEtAGiD"        (CppHex32  nULLcONFINEtAGiD        )
  , Cpp "pRIMtAGtYPEbITMASK"      (CppHex64  pRIMtAGtYPEbITMASK      )
  , Cpp "pRIMtAGiSbEZIER"         (CppHex64  pRIMtAGiSbEZIER         )
  , Cpp "pRIMtAGiSfACET"          (CppHex64  pRIMtAGiSfACET          )
  , Cpp "pRIMtAGiSrECTANGLE"      (CppHex64  pRIMtAGiSrECTANGLE      )
  , Cpp "pRIMtAGiSeLIPSE"         (CppHex64  pRIMtAGiSeLIPSE         )
  , Cpp "pRIMtAGsTORAGEiDbITMASK" (CppHex64  pRIMtAGsTORAGEiDbITMASK )
  , Cpp "pRIMtAGsTORAGEiDsHIFT"   (CppInt    pRIMtAGsTORAGEiDsHIFT   )
  , Cpp "pRIMtAGfABRICiDbITMASK"  (CppHex64  pRIMtAGfABRICiDbITMASK  )
  , Cpp "fABRICtYPEbITMASK"       (CppHex32  fABRICtYPEbITMASK       )
  , Cpp "fABRICiSrETURN"          (CppHex32  fABRICiSrETURN          )
  , Cpp "fABRICiScONSTANT"        (CppHex32  fABRICiScONSTANT        )
  , Cpp "fABRICiStEXTURE"         (CppHex32  fABRICiStEXTURE         )
  , Cpp "fABRICiSfUNCTION"        (CppHex32  fABRICiSfUNCTION        )
  , Cpp "fABRICiSbINARY"          (CppHex32  fABRICiSbINARY          )
  , Cpp "fABRICiSuNARYpOST"       (CppHex32  fABRICiSuNARYpOST       )
  , Cpp "fABRICiSdECOtREE"        (CppHex32  fABRICiSdECOtREE        )
  , Cpp "fABRICiScONFINEtREE"     (CppHex32  fABRICiScONFINEtREE     )
  , Cpp "fABRICiSsTACKER"         (CppHex32  fABRICiSsTACKER         )
  , Cpp "fABRICiSaFFINE"          (CppHex32  fABRICiSaFFINE          )
  , Cpp "fABRICiSfACET"           (CppHex32  fABRICiSfACET           )
  , Cpp "fABRICiScONVOLVE"        (CppHex32  fABRICiScONVOLVE        )
  , Cpp "fABRICiScOMPOSITE"       (CppHex32  fABRICiScOMPOSITE       )
  , Cpp "fABRICiSmULT"            (CppHex32  fABRICiSmULT            )
  , Cpp "fABRICiSaDD"             (CppHex32  fABRICiSaDD             )
  , Cpp "fABRICiSfLOAToR"         (CppHex32  fABRICiSfLOAToR         )
  , Cpp "fABRICiSfLOATxOR"        (CppHex32  fABRICiSfLOATxOR        )
  , Cpp "fABRICiSmIN"             (CppHex32  fABRICiSmIN             )
  , Cpp "fABRICiSmAX"             (CppHex32  fABRICiSmAX             )
  , Cpp "fABRICiShSVaDJUST"       (CppHex32  fABRICiShSVaDJUST       )
  , Cpp "fABRICiStRANSPARENT"     (CppHex32  fABRICiStRANSPARENT     )
  , Cpp "fABRICiSsQRT"            (CppHex32  fABRICiSsQRT            )
  , Cpp "fABRICiSiNVERT"          (CppHex32  fABRICiSiNVERT          )
  , Cpp "fABRICiScOS"             (CppHex32  fABRICiScOS             )
  , Cpp "fABRICiSsIN"             (CppHex32  fABRICiSsIN             )
  , Cpp "fABRICiScLAMP"           (CppHex32  fABRICiScLAMP           )
  , Cpp "fABRICiSlINEAR"          (CppHex32  fABRICiSlINEAR          )
  , Cpp "fABRICiSqUADRANCE"       (CppHex32  fABRICiSqUADRANCE       )
  , Cpp "fABRICtAGdATAbITMASK"    (CppHex32  fABRICtAGdATAbITMASK    )
  , Cpp "nULLfABRICtAGiD"         (CppHex32  nULLfABRICtAGiD         )
  , Cpp "dEBUGoUTPUT"             (CppNothing) -- uncomment this to turn on simple debugging output
  , Cpp "dEBUG0"                  (CppInt dEBUG0 ) -- determines the column for DEBUG_IF macro
  , Cpp "dEBUG1"                  (CppInt dEBUG1 ) -- determines the row    for DEBUG_IF macro
  ]
-- | Embedded source with implanted definition pragmas.
addDefinesToSource :: DeviceSpec -> BS.ByteString -> String
addDefinesToSource spec src = appendCppDefines sOURCEfILEpADDING (cppDefines spec) (BS.unpack src)

-- | This function determines the basic paramters of the rasterizer based on
determineRasterSpec :: CLDeviceID -> IO DeviceSpec
determineRasterSpec device =
  do  -- | Compute units are the number of wavefront processors on the GPU
      computeUnits  <- clGetDeviceMaxComputeUnits device
      -- | Maximum group dimension of a kernel call based on the device.
      maxGroupSize  <- fromIntegral <$> clGetDeviceMaxWorkGroupSize device
      -- | Maximum local memory size of the device.
      localMemSize  <- clGetDeviceLocalMemSize device
      -- | Maximum constant buffer size of the device.
      maxBufferSize <- clGetDeviceMaxConstantBufferSize device
      -- | Total global memory size of the device.
      globalMemSize <- clGetDeviceGlobalMemSize device
      -- | Maximum memory size that can be allocated for each global memory buffer
      maxMemAllocSize <- clGetDeviceMaxMemAllocSize device
      -- The maximum number of threads that each tile can store is the maximum allocation size
      let computeSize  = maxGroupSize :: Int
          computeDepth = adjustedLog computeSize
      return DeviceSpec { _specMaxTileSize = fromIntegral computeSize
                        , _specComputeSize = computeSize
                        , _specComputeDepth = adjustedLog computeSize
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

getKernelAndDump device program name =
  do kernel <- program name
     kernelDetail <- initCLKernelDetail kernel device
     putStrLn $ dumpKernelDetail kernelDetail
     return kernel

-- | Create a RasterState by setting up an OpenCL device.
setupOpenCL :: Bool -> Bool -> BS.ByteString -> IO DagOpenCLState
setupOpenCL enableProfiling useCLGLInterop src =
  do
      putStrLn $ "Initializing OpenCL device."
      -- Create Detail Records for every available device.
      details <- queryOpenCL CL_DEVICE_TYPE_GPU
      -- List all platforms and all devices.
      --mapM (putStrLn . show) details
      -- Filter function for qualified devices to select.
      --let deviceFilter = deviceNameContains "Iris Pro"  -- select the first Iris Pro GPU
      --let deviceFilter = deviceNameContains "AMD"        -- select the first AMD GPU
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
              deviceSpec <- determineRasterSpec device
              let modifiedSrc  = addDefinesToSource deviceSpec src
              -- Compile the source.
              putStrLn $ "Starting OpenCL kernel compile"
              program <- loadProgramWOptions options state modifiedSrc
              putStrLn $ "Finished OpenCL kernel compile"
              -- get the rasterizer kernel.
              traverseDagKernel <- getKernelAndDump device program "traverseDagKernel"
              -- Return a Library constructor with relevant information about the device for the rasterizer.
              return $ DagOpenCLState
                  { -- | The OpenCL state
                    _dagOpenCLState = state
                    -- | The rasterizer kernels.
                  , _dagOpenCLTraverseDagKernel = traverseDagKernel
                    -- | Flag for if OpenCL-OpenGL interop should be used to render the drawing target.
                  , _dagOpenCLUseGLInterop = useCLGLInterop
                  , _dagOpenCLDeviceSpec   = deviceSpec
                  }
