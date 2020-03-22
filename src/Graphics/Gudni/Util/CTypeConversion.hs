module Graphics.Gudni.Util.CTypeConversion
  ( toCBool
  , toCInt
  , fromCInt
  )
where

import Foreign.C.Types

toCBool :: Bool -> CBool
toCBool True  = 1
toCBool False = 0

toCInt :: Int -> CInt
toCInt = fromIntegral

fromCInt :: CInt -> Int
fromCInt = fromIntegral
