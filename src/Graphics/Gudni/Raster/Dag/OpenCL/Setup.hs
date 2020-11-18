-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.OpenCL.Setup
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for setting up an OpenCL platform to run the rasterizer kernel.
-- Including macro definitions that are implanted into OpenCL source code.

module Graphics.Gudni.Raster.Dag.OpenCL.Setup
  ( setupOpenCL
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Substance.Color
import Graphics.Gudni.Raster.Dag.Constants
import Graphics.Gudni.Raster.Dag.OpenCL.RasterState
import Graphics.Gudni.Raster.OpenCL.DeviceQuery
import Graphics.Gudni.Raster.OpenCL.KernelQuery
import Graphics.Gudni.Raster.OpenCL.CppDefines
import Graphics.Gudni.Interface.GLInterop

import Graphics.Gudni.Util.RandomField
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Parallel.OpenCL
import Control.Lens
import CLUtil.KernelArgs
import CLUtil
import CLUtil.Initialization
import CLUtil.CL

--import Graphics.Gudni.Raster.OpenCL.DeviceQuery

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
  [ Cpp "RANDOMFIELDSIZE"             (CppInt    rANDOMFIELDsIZE          )
  , Cpp "SHAPESTACKSIZE"              (CppInt    sHAPEsTACKsIZE           )
  , Cpp "FABRICSTACKSIZE"             (CppInt    fABRICsTACKsIZE          )
  , Cpp "COLORSTACKSIZE"              (CppInt    cOLORsTACKsIZE           )
  , Cpp "RAYSTACKSIZE"                (CppInt    rAYsTACKsIZE             )
  , Cpp "CLEARBLACK"                  (CppFloat4 cLEARbLACK               )
  , Cpp "OPAQUEWHITE"                 (CppFloat4 oPAQUEwHITE              )

  , Cpp "NULLSHAPEID"                 (CppHex32 nULLsHAPEiD               )
  , Cpp "PRIMTAGTYPEBITMASK"          (CppHex64 pRIMtAGtYPEbITMASK        )
  , Cpp "PRIMTAGISBEZIER"             (CppHex64 pRIMtAGiSbEZIER           )
  , Cpp "PRIMTAGISFACET"              (CppHex64 pRIMtAGiSfACET            )
  , Cpp "PRIMTAGISRECTANGLE"          (CppHex64 pRIMtAGiSrECTANGLE        )
  , Cpp "PRIMTAGISELIPSE"             (CppHex64 pRIMtAGiSeLIPSE           )
  , Cpp "PRIMTAGSTORAGEIDBITMASK"     (CppHex64 pRIMtAGsTORAGEiDbITMASK   )
  , Cpp "PRIMTAGSTORAGEIDSHIFT"       (CppInt   pRIMtAGsTORAGEiDsHIFT     )
  , Cpp "PRIMTAGFABRICIDBITMASK"      (CppHex64 pRIMtAGfABRICiDbITMASK    )

  , Cpp "FABRICNODETYPEBITMASK"       (CppHex64 fABRICnODEtYPEbITMASK     )
  , Cpp "FABRICISLEAF"                (CppHex64 fABRICiSlEAF              )
  , Cpp "FABRICISUNARYPRE"            (CppHex64 fABRICiSuNARYpRE          )
  , Cpp "FABRICISUNARYPOST"           (CppHex64 fABRICiSuNARYpOST         )
  , Cpp "FABRICISBINARY"              (CppHex64 fABRICiSbINARY            )
  , Cpp "FABRICNODESUBTYPEBITMASK"    (CppHex64 fABRICnODEsUBtYPEbITMASK  )
  , Cpp "FABRICISTREE"                (CppHex64 fABRICiStREE              )
  , Cpp "FABRICISTRANSFORMAFFINE"     (CppHex64 fABRICiStRANSFORMaFFINE   )
  , Cpp "FABRICISTRANSFORMFACET"      (CppHex64 fABRICiStRANSFORMfACET    )
  , Cpp "FABRICISTRANSFORMCONVOLVE"   (CppHex64 fABRICiStRANSFORMcONVOLVE )
  , Cpp "FABRICISSQRT"                (CppHex64 fABRICiSsQRT              )
  , Cpp "FABRICISINVERT"              (CppHex64 fABRICiSiNVERT            )
  , Cpp "FABRICISCOS"                 (CppHex64 fABRICiScOS               )
  , Cpp "FABRICISSIN"                 (CppHex64 fABRICiSsIN               )
  , Cpp "FABRICISCOMPOSITE"           (CppHex64 fABRICiScOMPOSITE         )
  , Cpp "FABRICISMULT"                (CppHex64 fABRICiSmULT              )
  , Cpp "FABRICISADD"                 (CppHex64 fABRICiSaDD               )
  , Cpp "FABRICISFLOATOR"             (CppHex64 fABRICiSfLOAToR           )
  , Cpp "FABRICISFLOATXOR"            (CppHex64 fABRICiSfLOATxOR          )
  , Cpp "FABRICISMIN"                 (CppHex64 fABRICiSmIN               )
  , Cpp "FABRICISMAX"                 (CppHex64 fABRICiSmAX               )
  , Cpp "FABRICISSATURATE"            (CppHex64 fABRICiSsATURATE          )
  , Cpp "SUBSTANCEISCONSTANT"         (CppHex64 fABRICiScONSTANT          )
  , Cpp "SUBSTANCEISTEXTURE"          (CppHex64 fABRICiStEXTURE           )
  , Cpp "SUBSTANCEISLINEAR"           (CppHex64 fABRICiSlINEAR            )
  , Cpp "SUBSTANCEISQUADRANCE"        (CppHex64 fABRICiSqUADRANCE         )
  , Cpp "FABRICTAGDATABITMASK"        (CppHex64 fABRICtAGdATAbITMASK      )
  , Cpp "FABRICTAGHIGHIDSHIFT"        (CppInt   fABRICtAGhIGHiDsHIFT      )
  , Cpp "FABRICTAGHIGHIDBITMASK"      (CppHex64 fABRICtAGhIGHiDbITMASK    )
  , Cpp "FABRICTAGLOWIDBITMASK"       (CppHex64 fABRICtAGlOWiDbITMASK     )
  , Cpp "NULLFABRICTAGID"             (CppHex32 nULLfABRICtAGiD           )

  , Cpp "DEBUG_OUTPUT"                (CppNothing) -- uncomment this to turn on simple debugging output
  --, Cpp "DEBUG_TRACE"                    (CppNothing) -- uncomment this to turn on parsable debugging output
  , Cpp "DEBUGCOLUMNTHREAD"           (CppInt 5)   -- determines the column for DEBUG_IF macro
  , Cpp "DEBUGINDEX"                  (CppInt 0)   -- determines the index for DEBUG_IF macro
  ]
-- | Embedded source with implanted definition pragmas.
addDefinesToSource :: DeviceSpec -> BS.ByteString -> String
addDefinesToSource spec src = appendCppDefines sOURCEfILEpADDING (cppDefines spec) (BS.unpack src)

-- | This function determines the basic paramters of the rasterizer based on
determineRasterSpec :: CLDeviceID -> IO DeviceSpec
determineRasterSpec device =
  do  -- | Compute units are the number of wavefront processors on the GPU
      computeUnits  <- clGetDeviceMaxComputeUnits       device
      -- | Maximum group dimension of a kernel call based on the device.
      maxGroupSize  <- fromIntegral <$> clGetDeviceMaxWorkGroupSize      device
      -- | Maximum local memory size of the device.
      localMemSize  <- clGetDeviceLocalMemSize          device
      -- | Maximum constant buffer size of the device.
      maxBufferSize <- clGetDeviceMaxConstantBufferSize device
      -- | Total global memory size of the device.
      globalMemSize <- clGetDeviceGlobalMemSize         device
      -- | Maximum memory size that can be allocated for each global memory buffer
      maxMemAllocSize <- clGetDeviceMaxMemAllocSize     device
      -- The maximum number of threads that each tile can store is the maximum allocation size
      let computeSize       = maxGroupSize :: Int
          computeDepth      = adjustedLog computeSize
      return DeviceSpec { _specMaxTileSize       = fromIntegral computeSize
                        , _specColumnsPerBlock   = computeSize
                        , _specColumnDepth       = adjustedLog computeSize
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
setupOpenCL :: Bool -> Bool -> BS.ByteString -> IO RasterState
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
              -- Generate a random field for the stochastic aliasing of the rasterizer.
              randomField <- makeRandomField rANDOMFIELDsIZE
              -- Return a Library constructor with relevant information about the device for the rasterizer.
              return $ RasterState
                  { -- | The OpenCL state
                    _rasterClState = state
                    -- | The rasterizer kernels.
                  , _rasterTraverseDagKernel = traverseDagKernel
                    -- | Flag for if OpenCL-OpenGL interop should be used to render the drawing target.
                  , _rasterUseGLInterop = useCLGLInterop
                  , _rasterDeviceSpec   = deviceSpec
                  , _rasterRandomField  = randomField
                  }
