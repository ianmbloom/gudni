{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}

module Graphics.Gudni.OpenCL.DeviceQuery
  ( deviceNameContains
  , queryOpenCL
  , dumpOpenCLDeviceInfo
  )
where

import Control.Monad
import Control.Parallel.OpenCL
import CLUtil.CL
import Graphics.Gudni.Util.Util

import Data.List

-- | Determine if the Device Name of a particular device contains a specific string.
deviceNameContains selector deviceID =
  do  name <- clGetDeviceName deviceID
      return (isInfixOf selector name)

-- | Pad a device query line for aligned display.
showQuery deviceID (title, f) =
  do answer <- f deviceID
     putStrLn $ (lpad 40 title) ++ ": " ++ answer

-- | Output a query of all information about a every available OpenCL platform and
-- every device on that platform.
queryOpenCL deviceType =
  do  platformIDs <- clGetPlatformIDs
      forM platformIDs $ \platformID ->
        do
         name <- clGetPlatformInfo platformID CL_PLATFORM_NAME
         putStrLn $ "================= " ++ name ++ "======================"
         platformInfos <- mapM (clGetPlatformInfo platformID) [ CL_PLATFORM_PROFILE, CL_PLATFORM_VERSION, CL_PLATFORM_VENDOR, CL_PLATFORM_EXTENSIONS]
         putStrLn $ unlines platformInfos
         deviceIDs <- clGetDeviceIDs platformID deviceType
         forM deviceIDs dumpOpenCLDeviceInfo

-- | Output a query of all information about a particular OpenCL device.
dumpOpenCLDeviceInfo deviceID =
  do  name <- clGetDeviceName deviceID
      putStrLn $ "======================== " ++ name ++ " ========================"
      mapM (showQuery deviceID) $
        [ (    "ExecutionCapabilities" , fmap show . clGetDeviceExecutionCapabilities     )
        , (         "DeviceAddressBits", fmap show . clGetDeviceAddressBits               )
        , (                 "Available", fmap show . clGetDeviceAvailable                 )
        , (         "CompilerAvailable", fmap show . clGetDeviceCompilerAvailable         )
        , (              "EndianLittle", fmap show . clGetDeviceEndianLittle              )
        , (    "ErrorCorrectionSupport", fmap show . clGetDeviceErrorCorrectionSupport    )
        , (                "Extensions", fmap show . clGetDeviceExtensions                )
        , (        "GlobalMemCacheSize", fmap show . clGetDeviceGlobalMemCacheSize        )
        , (    "GlobalMemCachelineSize", fmap show . clGetDeviceGlobalMemCachelineSize    )
        , (             "GlobalMemSize", fmap show . clGetDeviceGlobalMemSize             )
        , (              "ImageSupport", fmap show . clGetDeviceImageSupport              )
        , (          "Image2DMaxHeight", fmap show . clGetDeviceImage2DMaxHeight          )
        , (           "Image2DMaxWidth", fmap show . clGetDeviceImage2DMaxWidth           )
        , (           "Image3DMaxDepth", fmap show . clGetDeviceImage3DMaxDepth           )
        , (          "Image3DMaxHeight", fmap show . clGetDeviceImage3DMaxHeight          )
        , (           "Image3DMaxWidth", fmap show . clGetDeviceImage3DMaxWidth           )
        , (              "LocalMemSize", fmap show . clGetDeviceLocalMemSize              )
        , (         "MaxClockFrequency", fmap show . clGetDeviceMaxClockFrequency         )
        , (           "MaxComputeUnits", fmap show . clGetDeviceMaxComputeUnits           )
        , (           "MaxConstantArgs", fmap show . clGetDeviceMaxConstantArgs           )
        , (     "MaxConstantBufferSize", fmap show . clGetDeviceMaxConstantBufferSize     )
        , (           "MaxMemAllocSize", fmap show . clGetDeviceMaxMemAllocSize           )
        , (          "MaxParameterSize", fmap show . clGetDeviceMaxParameterSize          )
        , (          "MaxReadImageArgs", fmap show . clGetDeviceMaxReadImageArgs          )
        , (               "MaxSamplers", fmap show . clGetDeviceMaxSamplers               )
        , (          "MaxWorkGroupSize", fmap show . clGetDeviceMaxWorkGroupSize          )
        , (     "MaxWorkItemDimensions", fmap show . clGetDeviceMaxWorkItemDimensions     )
        , (          "MaxWorkItemSizes", fmap show . clGetDeviceMaxWorkItemSizes          )
        , (         "MaxWriteImageArgs", fmap show . clGetDeviceMaxWriteImageArgs         )
        , (          "MemBaseAddrAlign", fmap show . clGetDeviceMemBaseAddrAlign          )
        , (      "MinDataTypeAlignSize", fmap show . clGetDeviceMinDataTypeAlignSize      )
        , (  "PreferredVectorWidthChar", fmap show . clGetDevicePreferredVectorWidthChar  )
        , ( "PreferredVectorWidthShort", fmap show . clGetDevicePreferredVectorWidthShort )
        , (   "PreferredVectorWidthInt", fmap show . clGetDevicePreferredVectorWidthInt   )
        , (  "PreferredVectorWidthLong", fmap show . clGetDevicePreferredVectorWidthLong  )
        , ( "PreferredVectorWidthFloat", fmap show . clGetDevicePreferredVectorWidthFloat )
        , ("PreferredVectorWidthDouble", fmap show . clGetDevicePreferredVectorWidthDouble)
        , (                   "Profile", fmap show . clGetDeviceProfile                   )
        , (  "ProfilingTimerResolution", fmap show . clGetDeviceProfilingTimerResolution  )
        , (                    "Vendor", fmap show . clGetDeviceVendor                    )
        , (                  "VendorID", fmap show . clGetDeviceVendorID                  )
        , (                   "Version", fmap show . clGetDeviceVersion                   )
        , (             "DriverVersion", fmap show . clGetDeviceDriverVersion             )
        , (            "SingleFPConfig", fmap show . clGetDeviceSingleFPConfig            )
        , (            "DoubleFPConfig", fmap show . clGetDeviceDoubleFPConfig            )
    --  , (              "HalfFPConfig", fmap show . clGetDeviceHalfFPConfig              )
        , (              "LocalMemType", fmap show . clGetDeviceLocalMemType              )
        , (        "GlobalMemCacheType", fmap show . clGetDeviceGlobalMemCacheType        )
        , (           "QueueProperties", fmap show . clGetDeviceQueueProperties           )
        , (                      "Type", fmap show . clGetDeviceType                      )
        ]
