module Graphics.Gudni.OpenCL.CppDefines
  ( CppDefinition(..)
  , CppValueType(..)
  , appendCppDefines
  )
where

import Numeric
import Data.Word
import Data.Char
import Foreign.C.Types (CULong)

data CppValueType
  = CppHex64 CULong
  | CppHex32 Int
  | CppInt   Int
  | CppFloat Float

data CppDefinition = Cpp
  { cppName  :: String
  , cppValue :: CppValueType
  }

instance Show CppValueType where
  show val = case val of
    CppHex64 i -> map toUpper $ "0x" ++ showHex (fromIntegral i :: Word64) []
    CppHex32 i -> map toUpper $ "0x" ++ showHex (fromIntegral i :: Word32) []
    CppInt   i -> showInt i                          []
    CppFloat f -> showFFloat Nothing f               []

instance Show CppDefinition where
  show (Cpp name val) = "#define " ++ name ++ " " ++ show val

padEnd :: Int -> [String] -> [String]
padEnd len xs =
  let padding = take (max 0 (len - length xs)) $ repeat "// Padding line "
  in  xs ++ padding

generateCppDefines :: [CppDefinition] -> [String]
generateCppDefines cds = map show cds

appendCppDefines :: Int -> [CppDefinition] -> String -> String
appendCppDefines frontSpace definitions source =
  let srcLines = lines source
      cppDefines = padEnd frontSpace $ generateCppDefines definitions
  in  unlines $ cppDefines ++ drop frontSpace srcLines
