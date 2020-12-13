{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TemplateHaskell     #-}

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
-- Boilerplate galore for querying OpenCL device specifications.

module Graphics.Gudni.Raster.OpenCL.Util.DeviceQuery
  ( CLDeviceExtension(..)
  , CLDeviceProfile(..)
  , CLDeviceDetail(..)
  , clDeviceName
  , clDeviceDeviceId
  , clDevicePlatformDetail
  , clDeviceExecutionCapabilities
  , clDeviceAddressBits
  , clDeviceAvailable
  , clDeviceCompilerAvailable
  , clDeviceEndianLittle
  , clDeviceErrorCorrectionSupport
  , clDeviceExtensions
  , clDeviceGlobalMemCacheSize
  , clDeviceGlobalMemCachelineSize
  , clDeviceGlobalMemSize
  , clDeviceImageSupport
  , clDeviceImage2DMaxHeight
  , clDeviceImage2DMaxWidth
  , clDeviceImage3DMaxDepth
  , clDeviceImage3DMaxHeight
  , clDeviceImage3DMaxWidth
  , clDeviceLocalMemSize
  , clDeviceMaxClockFrequency
  , clDeviceMaxComputeUnits
  , clDeviceMaxConstantArgs
  , clDeviceMaxConstantBufferSize
  , clDeviceMaxMemAllocSize
  , clDeviceMaxParameterSize
  , clDeviceMaxReadImageArgs
  , clDeviceMaxSamplers
  , clDeviceMaxWorkGroupSize
  , clDeviceMaxWorkItemDimensions
  , clDeviceMaxWorkItemSizes
  , clDeviceMaxWriteImageArgs
  , clDeviceMemBaseAddrAlign
  , clDeviceMinDataTypeAlignSize
  , clDevicePreferredVectorWidthChar
  , clDevicePreferredVectorWidthShort
  , clDevicePreferredVectorWidthInt
  , clDevicePreferredVectorWidthLong
  , clDevicePreferredVectorWidthFloat
  , clDevicePreferredVectorWidthDouble
  , clDeviceProfile
  , clDeviceProfilingTimerResolution
  , clDeviceVendor
  , clDeviceVendorID
  , clDeviceVersion
  , clDeviceDriverVersion
  , clDeviceSingleFPConfig
  , clDeviceDoubleFPConfig
  --, clDeviceHalfFPConfig
  , clDeviceLocalMemType
  , clDeviceGlobalMemCacheType
  , clDeviceQueueProperties
  , clDeviceType
  , CLPlatformDetail(..)
  , clPlatformName
  , clPlatformProfile
  , clPlatformVersion
  , clPlatformVendor
  , clPlatformExtensions
  , queryOpenCL
  , dumpDeviceDetail
  )
where

import CLUtil.CL
import Control.Exception (handle, throw)
import Control.Monad
import Control.Parallel.OpenCL
import Foreign.C.Types (CSize)
import Graphics.Gudni.Util.Util

import Data.List
import Data.Maybe (catMaybes)
import Control.Lens

data CLDeviceExtension
  = CL_KHR_FP64
  | CL_KHR_SELECT_FPROUNDING_MODE
  | CL_KHR_GLOBAL_INT32_BASE_ATOMICS
  | CL_KHR_GLOBAL_INT32_EXTENDED_ATOMICS
  | CL_KHR_LOCAL_INT32_BASE_ATOMICS
  | CL_KHR_LOCAL_INT32_EXTENDED_ATOMICS
  | CL_KHR_INT64_BASE_ATOMICS
  | CL_KHR_INT64_EXTENDED_ATOMICS
  | CL_KHR_3D_IMAGE_WRITES
  | CL_KHR_BYTE_ADDRESSABLE_STORE
  | CL_KHR_FP16
  deriving (Show, Eq)

clExtensionMapping :: [(String, CLDeviceExtension)]
clExtensionMapping =
  [ ("cl_khr_fp64"                         , CL_KHR_FP64                         )
  , ("cl_khr_select_fprounding_mode"       , CL_KHR_SELECT_FPROUNDING_MODE       )
  , ("cl_khr_global_int32_base_atomics"    , CL_KHR_GLOBAL_INT32_BASE_ATOMICS    )
  , ("cl_khr_global_int32_extended_atomics", CL_KHR_GLOBAL_INT32_EXTENDED_ATOMICS)
  , ("cl_khr_local_int32_base_atomics"     , CL_KHR_LOCAL_INT32_BASE_ATOMICS     )
  , ("cl_khr_local_int32_extended_atomics" , CL_KHR_LOCAL_INT32_EXTENDED_ATOMICS )
  , ("cl_khr_int64_base_atomics"           , CL_KHR_INT64_BASE_ATOMICS           )
  , ("cl_khr_int64_extended_atomics"       , CL_KHR_INT64_EXTENDED_ATOMICS       )
  , ("cl_khr_3d_image_writes"              , CL_KHR_3D_IMAGE_WRITES              )
  , ("cl_khr_byte_addressable_store"       , CL_KHR_BYTE_ADDRESSABLE_STORE       )
  , ("cl_khr_fp16"                         , CL_KHR_FP16                         )
  ]

search :: Eq a => [(a,b)] -> a -> Maybe b
search list a = fmap snd . find ((== a) . fst) $ list

getExtensionList :: String -> [CLDeviceExtension]
getExtensionList = catMaybes . map (search clExtensionMapping) . words

data CLDeviceProfile
  = CL_FULL_PROFILE
  | CL_EMBEDDED_PROFILE
  deriving (Show, Eq)

determineClDeviceProfile :: String -> CLDeviceProfile
determineClDeviceProfile rawString =
  if rawString == "FULL_PROFILE"
  then CL_FULL_PROFILE
  else CL_EMBEDDED_PROFILE

data CLPlatformDetail = CLPlatformDetail
    { _clPlatformName       :: String
    , _clPlatformProfile    :: String
    , _clPlatformVersion    :: String
    , _clPlatformVendor     :: String
    , _clPlatformExtensions :: String
    } deriving (Show)
makeLenses ''CLPlatformDetail

-- | Create a CLPlatformInfo record by querying openCL
initCLPlatformInfo :: CLPlatformID -> IO CLPlatformDetail
initCLPlatformInfo platformID =
  do  name       <- clGetPlatformInfo platformID CL_PLATFORM_NAME
      profile    <- clGetPlatformInfo platformID CL_PLATFORM_PROFILE
      version    <- clGetPlatformInfo platformID CL_PLATFORM_VERSION
      vendor     <- clGetPlatformInfo platformID CL_PLATFORM_VENDOR
      extensions <- clGetPlatformInfo platformID CL_PLATFORM_EXTENSIONS
      return CLPlatformDetail
        { _clPlatformName       = name
        , _clPlatformProfile    = profile
        , _clPlatformVersion    = version
        , _clPlatformVendor     = vendor
        , _clPlatformExtensions = extensions
        }

data CLDeviceDetail = CLDeviceDetail
    { _clDeviceName                       :: String
    , _clDeviceDeviceId                   :: CLDeviceID
    , _clDevicePlatformDetail             :: CLPlatformDetail
    , _clDeviceExecutionCapabilities      :: [CLDeviceExecCapability]
    , _clDeviceAddressBits                :: CLuint
    , _clDeviceAvailable                  :: Bool
    , _clDeviceCompilerAvailable          :: Bool
    , _clDeviceEndianLittle               :: Bool
    , _clDeviceErrorCorrectionSupport     :: Bool
    , _clDeviceExtensions                 :: [CLDeviceExtension]
    , _clDeviceGlobalMemCacheSize         :: CLulong
    , _clDeviceGlobalMemCachelineSize     :: CLuint
    , _clDeviceGlobalMemSize              :: CLulong
    , _clDeviceImageSupport               :: Bool
    , _clDeviceImage2DMaxHeight           :: CSize
    , _clDeviceImage2DMaxWidth            :: CSize
    , _clDeviceImage3DMaxDepth            :: CSize
    , _clDeviceImage3DMaxHeight           :: CSize
    , _clDeviceImage3DMaxWidth            :: CSize
    , _clDeviceLocalMemSize               :: CLulong
    , _clDeviceMaxClockFrequency          :: CLuint
    , _clDeviceMaxComputeUnits            :: CLuint
    , _clDeviceMaxConstantArgs            :: CLuint
    , _clDeviceMaxConstantBufferSize      :: CLulong
    , _clDeviceMaxMemAllocSize            :: CLulong
    , _clDeviceMaxParameterSize           :: CSize
    , _clDeviceMaxReadImageArgs           :: CLuint
    , _clDeviceMaxSamplers                :: CLuint
    , _clDeviceMaxWorkGroupSize           :: CSize
    , _clDeviceMaxWorkItemDimensions      :: CLuint
    , _clDeviceMaxWorkItemSizes           :: [CSize]
    , _clDeviceMaxWriteImageArgs          :: CLuint
    , _clDeviceMemBaseAddrAlign           :: CLuint
    , _clDeviceMinDataTypeAlignSize       :: CLuint
    , _clDevicePreferredVectorWidthChar   :: CLuint
    , _clDevicePreferredVectorWidthShort  :: CLuint
    , _clDevicePreferredVectorWidthInt    :: CLuint
    , _clDevicePreferredVectorWidthLong   :: CLuint
    , _clDevicePreferredVectorWidthFloat  :: CLuint
    , _clDevicePreferredVectorWidthDouble :: CLuint
    , _clDeviceProfile                    :: CLDeviceProfile
    , _clDeviceProfilingTimerResolution   :: CSize
    , _clDeviceVendor                     :: String
    , _clDeviceVendorID                   :: CLuint
    , _clDeviceVersion                    :: String
    , _clDeviceDriverVersion              :: String
    , _clDeviceSingleFPConfig             :: [CLDeviceFPConfig]
    , _clDeviceDoubleFPConfig             :: [CLDeviceFPConfig]
  --, _clDeviceHalfFPConfig               :: [CLDeviceFPConfig]
    , _clDeviceLocalMemType               :: CLDeviceLocalMemType
    , _clDeviceGlobalMemCacheType         :: CLDeviceMemCacheType
    , _clDeviceQueueProperties            :: [CLCommandQueueProperty]
    , _clDeviceType                       :: [CLDeviceType]
    } deriving (Show)
makeLenses ''CLDeviceDetail

-- | Create a CLDeviceDetail record by querying an openCL device.
initCLDeviceDetail :: CLPlatformID -> CLDeviceID -> IO CLDeviceDetail
initCLDeviceDetail platformId deviceId =
  do  deviceName                       <- clGetDeviceName                       deviceId
      platformInfo                     <- initCLPlatformInfo platformId
      deviceExecutionCapabilities      <- clGetDeviceExecutionCapabilities      deviceId
      deviceAddressBits                <- clGetDeviceAddressBits                deviceId
      deviceAvailable                  <- clGetDeviceAvailable                  deviceId
      deviceCompilerAvailable          <- clGetDeviceCompilerAvailable          deviceId
      deviceEndianLittle               <- clGetDeviceEndianLittle               deviceId
      deviceErrorCorrectionSupport     <- clGetDeviceErrorCorrectionSupport     deviceId
      deviceExtensions                 <- clGetDeviceExtensions                 deviceId
      deviceGlobalMemCacheSize         <- clGetDeviceGlobalMemCacheSize         deviceId
      deviceGlobalMemCachelineSize     <- clGetDeviceGlobalMemCachelineSize     deviceId
      deviceGlobalMemSize              <- clGetDeviceGlobalMemSize              deviceId
      deviceImageSupport               <- clGetDeviceImageSupport               deviceId
      deviceImage2DMaxHeight           <- clGetDeviceImage2DMaxHeight           deviceId
      deviceImage2DMaxWidth            <- clGetDeviceImage2DMaxWidth            deviceId
      deviceImage3DMaxDepth            <- clGetDeviceImage3DMaxDepth            deviceId
      deviceImage3DMaxHeight           <- clGetDeviceImage3DMaxHeight           deviceId
      deviceImage3DMaxWidth            <- clGetDeviceImage3DMaxWidth            deviceId
      deviceLocalMemSize               <- clGetDeviceLocalMemSize               deviceId
      deviceMaxClockFrequency          <- clGetDeviceMaxClockFrequency          deviceId
      deviceMaxComputeUnits            <- clGetDeviceMaxComputeUnits            deviceId
      deviceMaxConstantArgs            <- clGetDeviceMaxConstantArgs            deviceId
      deviceMaxConstantBufferSize      <- clGetDeviceMaxConstantBufferSize      deviceId
      deviceMaxMemAllocSize            <- clGetDeviceMaxMemAllocSize            deviceId
      deviceMaxParameterSize           <- clGetDeviceMaxParameterSize           deviceId
      deviceMaxReadImageArgs           <- clGetDeviceMaxReadImageArgs           deviceId
      deviceMaxSamplers                <- clGetDeviceMaxSamplers                deviceId
      deviceMaxWorkGroupSize           <- clGetDeviceMaxWorkGroupSize           deviceId
      deviceMaxWorkItemDimensions      <- clGetDeviceMaxWorkItemDimensions      deviceId
      deviceMaxWorkItemSizes           <- clGetDeviceMaxWorkItemSizes           deviceId
      deviceMaxWriteImageArgs          <- clGetDeviceMaxWriteImageArgs          deviceId
      deviceMemBaseAddrAlign           <- clGetDeviceMemBaseAddrAlign           deviceId
      deviceMinDataTypeAlignSize       <- clGetDeviceMinDataTypeAlignSize       deviceId
      devicePreferredVectorWidthChar   <- clGetDevicePreferredVectorWidthChar   deviceId
      devicePreferredVectorWidthShort  <- clGetDevicePreferredVectorWidthShort  deviceId
      devicePreferredVectorWidthInt    <- clGetDevicePreferredVectorWidthInt    deviceId
      devicePreferredVectorWidthLong   <- clGetDevicePreferredVectorWidthLong   deviceId
      devicePreferredVectorWidthFloat  <- clGetDevicePreferredVectorWidthFloat  deviceId
      devicePreferredVectorWidthDouble <- clGetDevicePreferredVectorWidthDouble deviceId
      deviceProfile                    <- clGetDeviceProfile                    deviceId
      deviceProfilingTimerResolution   <- clGetDeviceProfilingTimerResolution   deviceId
      deviceVendor                     <- clGetDeviceVendor                     deviceId
      deviceVendorID                   <- clGetDeviceVendorID                   deviceId
      deviceVersion                    <- clGetDeviceVersion                    deviceId
      deviceDriverVersion              <- clGetDeviceDriverVersion              deviceId
      deviceSingleFPConfig             <- clGetDeviceSingleFPConfig             deviceId
      deviceDoubleFPConfig             <- clGetDeviceDoubleFPConfig             deviceId
    --deviceHalfFPConfig               <- clGetDeviceHalfFPConfig               deviceId
      deviceLocalMemType               <- clGetDeviceLocalMemType               deviceId
      deviceGlobalMemCacheType         <- clGetDeviceGlobalMemCacheType         deviceId
      deviceQueueProperties            <- clGetDeviceQueueProperties            deviceId
      deviceType                       <- clGetDeviceType                       deviceId
      return CLDeviceDetail
          { _clDeviceName                       = deviceName
          , _clDeviceDeviceId                   = deviceId
          , _clDevicePlatformDetail             = platformInfo
          , _clDeviceExecutionCapabilities      = deviceExecutionCapabilities
          , _clDeviceAddressBits                = deviceAddressBits
          , _clDeviceAvailable                  = deviceAvailable
          , _clDeviceCompilerAvailable          = deviceCompilerAvailable
          , _clDeviceEndianLittle               = deviceEndianLittle
          , _clDeviceErrorCorrectionSupport     = deviceErrorCorrectionSupport
          , _clDeviceExtensions                 = getExtensionList $ deviceExtensions
          , _clDeviceGlobalMemCacheSize         = deviceGlobalMemCacheSize
          , _clDeviceGlobalMemCachelineSize     = deviceGlobalMemCachelineSize
          , _clDeviceGlobalMemSize              = deviceGlobalMemSize
          , _clDeviceImageSupport               = deviceImageSupport
          , _clDeviceImage2DMaxHeight           = deviceImage2DMaxHeight
          , _clDeviceImage2DMaxWidth            = deviceImage2DMaxWidth
          , _clDeviceImage3DMaxDepth            = deviceImage3DMaxDepth
          , _clDeviceImage3DMaxHeight           = deviceImage3DMaxHeight
          , _clDeviceImage3DMaxWidth            = deviceImage3DMaxWidth
          , _clDeviceLocalMemSize               = deviceLocalMemSize
          , _clDeviceMaxClockFrequency          = deviceMaxClockFrequency
          , _clDeviceMaxComputeUnits            = deviceMaxComputeUnits
          , _clDeviceMaxConstantArgs            = deviceMaxConstantArgs
          , _clDeviceMaxConstantBufferSize      = deviceMaxConstantBufferSize
          , _clDeviceMaxMemAllocSize            = deviceMaxMemAllocSize
          , _clDeviceMaxParameterSize           = deviceMaxParameterSize
          , _clDeviceMaxReadImageArgs           = deviceMaxReadImageArgs
          , _clDeviceMaxSamplers                = deviceMaxSamplers
          , _clDeviceMaxWorkGroupSize           = deviceMaxWorkGroupSize
          , _clDeviceMaxWorkItemDimensions      = deviceMaxWorkItemDimensions
          , _clDeviceMaxWorkItemSizes           = deviceMaxWorkItemSizes
          , _clDeviceMaxWriteImageArgs          = deviceMaxWriteImageArgs
          , _clDeviceMemBaseAddrAlign           = deviceMemBaseAddrAlign
          , _clDeviceMinDataTypeAlignSize       = deviceMinDataTypeAlignSize
          , _clDevicePreferredVectorWidthChar   = devicePreferredVectorWidthChar
          , _clDevicePreferredVectorWidthShort  = devicePreferredVectorWidthShort
          , _clDevicePreferredVectorWidthInt    = devicePreferredVectorWidthInt
          , _clDevicePreferredVectorWidthLong   = devicePreferredVectorWidthLong
          , _clDevicePreferredVectorWidthFloat  = devicePreferredVectorWidthFloat
          , _clDevicePreferredVectorWidthDouble = devicePreferredVectorWidthDouble
          , _clDeviceProfile                    = determineClDeviceProfile deviceProfile
          , _clDeviceProfilingTimerResolution   = deviceProfilingTimerResolution
          , _clDeviceVendor                     = deviceVendor
          , _clDeviceVendorID                   = deviceVendorID
          , _clDeviceVersion                    = deviceVersion
          , _clDeviceDriverVersion              = deviceDriverVersion
          , _clDeviceSingleFPConfig             = deviceSingleFPConfig
          , _clDeviceDoubleFPConfig             = deviceDoubleFPConfig
        --, _clDeviceHalfFPConfig               = deviceHalfFPConfig
          , _clDeviceLocalMemType               = deviceLocalMemType
          , _clDeviceGlobalMemCacheType         = deviceGlobalMemCacheType
          , _clDeviceQueueProperties            = deviceQueueProperties
          , _clDeviceType                       = deviceType
          }

dumpPlatformDetauk :: CLPlatformDetail -> String
dumpPlatformDetauk platformInfo =
      (titleLine ("Platform " ++ show (platformInfo ^. clPlatformName)) ++) $
      foldl1 (++) $ map infoLine $
        [ (    "Profile", show $ platformInfo ^. clPlatformProfile    )
        , (    "Version", show $ platformInfo ^. clPlatformVersion    )
        , (     "Vendor", show $ platformInfo ^. clPlatformVendor     )
        , ( "Extensions", show $ platformInfo ^. clPlatformExtensions )
        ]

dumpDeviceDetail :: CLDeviceDetail -> String
dumpDeviceDetail deviceDetail =
      (titleLine (show (deviceDetail ^. clDeviceName)) ++) $
      foldl1 (++) $ map infoLine $
        [ (    "ExecutionCapabilities" , show $ deviceDetail ^. clDeviceExecutionCapabilities     )
        , (         "DeviceAddressBits", show $ deviceDetail ^. clDeviceAddressBits               )
        , (                 "Available", show $ deviceDetail ^. clDeviceAvailable                 )
        , (         "CompilerAvailable", show $ deviceDetail ^. clDeviceCompilerAvailable         )
        , (              "EndianLittle", show $ deviceDetail ^. clDeviceEndianLittle              )
        , (    "ErrorCorrectionSupport", show $ deviceDetail ^. clDeviceErrorCorrectionSupport    )
        , (                "Extensions", show $ deviceDetail ^. clDeviceExtensions                )
        , (        "GlobalMemCacheSize", show $ deviceDetail ^. clDeviceGlobalMemCacheSize        )
        , (    "GlobalMemCachelineSize", show $ deviceDetail ^. clDeviceGlobalMemCachelineSize    )
        , (             "GlobalMemSize", show $ deviceDetail ^. clDeviceGlobalMemSize             )
        , (              "ImageSupport", show $ deviceDetail ^. clDeviceImageSupport              )
        , (          "Image2DMaxHeight", show $ deviceDetail ^. clDeviceImage2DMaxHeight          )
        , (           "Image2DMaxWidth", show $ deviceDetail ^. clDeviceImage2DMaxWidth           )
        , (           "Image3DMaxDepth", show $ deviceDetail ^. clDeviceImage3DMaxDepth           )
        , (          "Image3DMaxHeight", show $ deviceDetail ^. clDeviceImage3DMaxHeight          )
        , (           "Image3DMaxWidth", show $ deviceDetail ^. clDeviceImage3DMaxWidth           )
        , (              "LocalMemSize", show $ deviceDetail ^. clDeviceLocalMemSize              )
        , (         "MaxClockFrequency", show $ deviceDetail ^. clDeviceMaxClockFrequency         )
        , (           "MaxComputeUnits", show $ deviceDetail ^. clDeviceMaxComputeUnits           )
        , (           "MaxConstantArgs", show $ deviceDetail ^. clDeviceMaxConstantArgs           )
        , (     "MaxConstantBufferSize", show $ deviceDetail ^. clDeviceMaxConstantBufferSize     )
        , (           "MaxMemAllocSize", show $ deviceDetail ^. clDeviceMaxMemAllocSize           )
        , (          "MaxParameterSize", show $ deviceDetail ^. clDeviceMaxParameterSize          )
        , (          "MaxReadImageArgs", show $ deviceDetail ^. clDeviceMaxReadImageArgs          )
        , (               "MaxSamplers", show $ deviceDetail ^. clDeviceMaxSamplers               )
        , (          "MaxWorkGroupSize", show $ deviceDetail ^. clDeviceMaxWorkGroupSize          )
        , (     "MaxWorkItemDimensions", show $ deviceDetail ^. clDeviceMaxWorkItemDimensions     )
        , (          "MaxWorkItemSizes", show $ deviceDetail ^. clDeviceMaxWorkItemSizes          )
        , (         "MaxWriteImageArgs", show $ deviceDetail ^. clDeviceMaxWriteImageArgs         )
        , (          "MemBaseAddrAlign", show $ deviceDetail ^. clDeviceMemBaseAddrAlign          )
        , (      "MinDataTypeAlignSize", show $ deviceDetail ^. clDeviceMinDataTypeAlignSize      )
        , (  "PreferredVectorWidthChar", show $ deviceDetail ^. clDevicePreferredVectorWidthChar  )
        , ( "PreferredVectorWidthShort", show $ deviceDetail ^. clDevicePreferredVectorWidthShort )
        , (   "PreferredVectorWidthInt", show $ deviceDetail ^. clDevicePreferredVectorWidthInt   )
        , (  "PreferredVectorWidthLong", show $ deviceDetail ^. clDevicePreferredVectorWidthLong  )
        , ( "PreferredVectorWidthFloat", show $ deviceDetail ^. clDevicePreferredVectorWidthFloat )
        , ("PreferredVectorWidthDouble", show $ deviceDetail ^. clDevicePreferredVectorWidthDouble)
        , (                   "Profile", show $ deviceDetail ^. clDeviceProfile                   )
        , (  "ProfilingTimerResolution", show $ deviceDetail ^. clDeviceProfilingTimerResolution  )
        , (                    "Vendor", show $ deviceDetail ^. clDeviceVendor                    )
        , (                  "VendorID", show $ deviceDetail ^. clDeviceVendorID                  )
        , (                   "Version", show $ deviceDetail ^. clDeviceVersion                   )
        , (             "DriverVersion", show $ deviceDetail ^. clDeviceDriverVersion             )
        , (            "SingleFPConfig", show $ deviceDetail ^. clDeviceSingleFPConfig            )
        , (            "DoubleFPConfig", show $ deviceDetail ^. clDeviceDoubleFPConfig            )
    --  , (              "HalfFPConfig", show $ deviceDetail ^. clDeviceHalfFPConfig              )
        , (              "LocalMemType", show $ deviceDetail ^. clDeviceLocalMemType              )
        , (        "GlobalMemCacheType", show $ deviceDetail ^. clDeviceGlobalMemCacheType        )
        , (           "QueueProperties", show $ deviceDetail ^. clDeviceQueueProperties           )
        , (                      "Type", show $ deviceDetail ^. clDeviceType                      )
        ]

-- | Query of all information about a every available OpenCL platform and
-- every device on that platform and return a CLDeviceDetail structure for each one.
queryOpenCL :: CLDeviceType -> IO [CLDeviceDetail]
queryOpenCL deviceType =
  do  platformIDs <- clGetPlatformIDs
      concat <$> mapM (\platformID -> do
        platformName <- clGetPlatformInfo platformID CL_PLATFORM_NAME
        putStrLn ("Found platform: " ++ platformName)
        deviceIDs <- clGetDeviceIDs platformID deviceType
            & handle (\err -> if err == CL_DEVICE_NOT_FOUND then return [] else throw err)
        putStrLn ("Found " ++ show (length deviceIDs) ++ " devices")
        deviceInfos <- mapM (initCLDeviceDetail platformID) deviceIDs
        return deviceInfos) platformIDs
