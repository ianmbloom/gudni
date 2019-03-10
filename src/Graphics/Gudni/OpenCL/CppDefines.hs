-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.OpenCL.CallKernel
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for implanting preprocessor definitions in an OpenCL source file at runtime before JIT compilation.

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

-- | The type and display method of a value to be defined in a C Preprocessor pragma.
data CppValueType
  = CppHex64 CULong
  | CppHex32 Int
  | CppInt   Int
  | CppFloat Float

-- | A definition for a C Preprocessor #define pragma.
data CppDefinition = Cpp
  { cppName  :: String
  , cppValue :: CppValueType
  }

-- | Convert a CppValue to a string.
instance Show CppValueType where
  show val = case val of
    CppHex64 i -> map toUpper $ "0x" ++ showHex (fromIntegral i :: Word64) []
    CppHex32 i -> map toUpper $ "0x" ++ showHex (fromIntegral i :: Word32) []
    CppInt   i -> showInt i                          []
    CppFloat f -> showFFloat Nothing f               []

-- | Convert a CppDefinition to a string.
instance Show CppDefinition where
  show (Cpp name val) = "#define " ++ name ++ " " ++ show val

-- | Take a list of lines and pad it up to a certain number of lines.
padEnd :: Int -> [String] -> [String]
padEnd len xs =
  let padding = take (max 0 (len - length xs)) $ repeat "// Padding line "
  in  xs ++ padding

-- | Generate lines of preprocessor definitions.
generateCppDefines :: [CppDefinition] -> [String]
generateCppDefines cds = map show cds

-- | Replace the initial lines of a source file with Preprocessor definitions∘
-- The source file would normally begin with the same number of lines of empty comments.
-- This is a hack so that after replacement the line numbers in error messages from the
-- OpenCL compiler remain correct.
appendCppDefines :: Int -> [CppDefinition] -> String -> String
appendCppDefines frontSpace definitions source =
  let srcLines = lines source
      cppDefines = padEnd frontSpace $ generateCppDefines definitions
  in  unlines $ cppDefines ++ drop frontSpace srcLines
