{-# LANGUAGE FlexibleInstances #-}

module Graphics.Gudni.Util.Debug
  ( assert
  , assertError
  , tr
  , trHex
  , trM
  , trWith
  , trF
  , tc
  , tcWith
  , trTitle
  , trCList
  , trLength
  , trWhen
  , tcWhen
  , trIfTrue
  , trIfFalse
  , showFl
  , showFl'
  , showFlFixed
  , showFlFixed'
  , trace
  , DebugIdentity (..)
  , debugHead
  , debugFromJust
  )

where

import Data.Word
import Data.Int
import Debug.Trace
import qualified Data.ByteString as B
import Numeric
import Control.Applicative

newtype DebugIdentity a = DebugIdentity { runDebugIdentity :: a }

instance Show a => Show (DebugIdentity a) where
    show = show

instance Functor DebugIdentity where
    fmap f m = DebugIdentity (f (runDebugIdentity m))

instance Applicative DebugIdentity where
    pure a = DebugIdentity a
    m <*> k = DebugIdentity $ (runDebugIdentity m) (runDebugIdentity k)

instance Monad DebugIdentity where
    return = trace "return" . pure
    m >>= k  = trace ">>=" $ k (runDebugIdentity m)

assert message cond x = if cond  then x else trace ("ASSERT " ++ message ++ show x) x
assertError message cond x = if cond then x else error ("ASSERTERROR " ++ message) x

showFl :: Float -> String
showFl = showFl' 3
showFl' numOfDecimals floatNum  = showFFloat (Just numOfDecimals) floatNum ""

showFlFixed :: Float -> String
showFlFixed = showFlFixed' 3 3
showFlFixed' width numOfDecimals floatNum  =
  let floatString = showFFloat (Just numOfDecimals) floatNum ""
      total = width + numOfDecimals + 1
      extra = total - length floatString
  in take extra (repeat ' ') ++ floatString ++ " "


trWith f m x = trace (m++":"++f x) x
tr :: (Show a) => String -> a -> a
tr = trWith show
trM m = fmap (tr m)
trF = trWith (showFl' 3)
trHex :: (Integral a) => String -> a -> a
trHex = trWith (\t -> showHex (fromIntegral t) "")


starLine c x = take x (repeat c)
titleBar m = starLine '>' 15 ++ m ++ starLine '>' 15 ++ "\n"
lowerBar m = starLine '<' 15 ++ m ++ starLine '<' 15 ++ "\n"

tcWith :: (a -> String) -> String -> a -> a
tcWith f m x = trace (m++"-->") $
                  trace ("-->"++m++"   "++(f x)) x

tc :: Show a => String -> a -> a
tc = tcWith show
--tc m x = trace (m++"-->") $ trace ("-->"++m++"   "++(show x)) x

trTitle m x = trace (titleBar m) $ trace (show x ++ "\n" ++ lowerBar m) $ x

--trHeap len m h = tinternal (\y -> (show $ show16and8 0 $ B.unpack $ B.take len y) ++ "...") m h
trCList m x = trace (m++":"++concat (zipWith (\i l ->", " ++ show i ++ ":" ++ show l) [0..] x)) x

trLength :: Show a => String -> [a] -> [a]
trLength = trWith (\x -> show (length x) ++ ":" ++ show x)

trWhen cond m x = if cond then tr m x else x
tcWhen cond m x = if cond then tc m x else x

trIfTrue m x = if x then tr m x else x
trIfFalse m x = if not x then tr m x else x

highAndLow :: Word8 -> Word8 -> Word16
highAndLow h l = (((fromIntegral h)*256+ (fromIntegral l))::Word16)
show16and8 i (h:l:xs) = show i ++":"++ show ((fromIntegral $ highAndLow h l)::Int16) ++ ";" ++ show (highAndLow h l) ++ "(" ++ show ((fromIntegral h)::Word8) ++ "," ++ show ((fromIntegral l)::Word8)++")|" ++ show16and8 (i+2) xs
show16and8 i (h:[])   = show i ++":"++ show (h)
show16and8 i ([])     = ""

debugHead m [] = error $ "debugHead" ++ m
debugHead m (x:_) = x

debugFromJust m Nothing = error $ "debugFromJust" ++ m
debugFromJust m (Just x) = x
