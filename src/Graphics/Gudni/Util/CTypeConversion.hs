module Graphics.Gudni.Util.CTypeConversion
  ( toCBool
  , fromCBool
  , toCInt
  , fromCInt
  )
where

import Foreign.C.Types

toCBool :: Bool -> CBool
toCBool True  = 1
toCBool False = 0

fromCBool :: CBool -> Bool
fromCBool 1 = True
fromCBool 0 = False

toCInt :: Int -> CInt
toCInt = fromIntegral

fromCInt :: CInt -> Int
fromCInt = fromIntegral
