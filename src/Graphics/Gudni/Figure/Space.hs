{-# LANGUAGE ScopedTypeVariables, ConstraintKinds, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, StandaloneDeriving  #-}
module Graphics.Gudni.Figure.Space
  ( Ortho (..)
  , XDimension
  , YDimension
  , toXOrtho
  , toYOrtho
  , orthoganal
  , Convertable (..)
  , Iota (..)
  , DisplaySpace (..)
  , IntSpace (..)

  , fastTruncateDouble
  , fastTruncateSingle
  , fastRoundDouble
  , showBin

  )
where

import Graphics.Gudni.Util.Debug

import Data.Array
import Data.Int
import Data.Word
import Data.Char
import Data.Bits
import Data.Hashable

import GHC.Float (double2Float)

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types (CFloat, CDouble, CInt, CShort)
import Unsafe.Coerce

import Text.Printf

import Control.Monad.Random
import Control.DeepSeq

import System.Random

import Linear.Epsilon

showBin i = printf "The value of %d in hex is: 0x%08x and binary is: %b\n" i i i

type DisplaySpace_ = CFloat
newtype DisplaySpace = DSpace {unDSpace :: DisplaySpace_} deriving (Enum, RealFrac, Fractional, Real, Num, Ord, Eq, RealFloat, Floating)

type IntSpace_ = Int32
newtype IntSpace     = ISpace {unISpace :: IntSpace_ } deriving (Integral, Enum, Ix, Real, Num, Ord, Eq)

------------------- Conversion between spaces ---------------------

class Convertable x y where
  convert :: x -> y

instance Convertable a b => Convertable (Ortho d a) (Ortho d b) where
  {-# INLINE convert #-}
  convert (Ortho a) = Ortho $ convert a

instance Convertable DisplaySpace IntSpace where
  {-# INLINE convert #-}
  convert (DSpace x) = ISpace . round $ x

instance Convertable IntSpace DisplaySpace where
  {-# INLINE convert #-}
  convert (ISpace x) = DSpace . fromIntegral $ x

--------------------- Iota -----------------
-- Iota is an extremely small value used for close enough comparison.

class Iota s where
  iota :: s

instance Iota s => Iota (Ortho d s) where
  iota = Ortho iota

instance Iota DisplaySpace where
  iota = DSpace 0.0001

instance Iota IntSpace where
  iota = ISpace 1

--------------------  Espilon ---------------
instance (Num s, Ord s, Iota s) => Epsilon s where
  nearZero = (<= iota)

--------------------- Random -----------------
instance Random DisplaySpace where
  random = runRand $ do value <- (realToFrac :: Float -> CFloat) <$> getRandomR (-10, 2880); return $ DSpace value
  randomR (DSpace l, DSpace h)= runRand $ do value <- getRandomR (l, h); return $ DSpace value

------------------------ Orthoganal Geometry ---------------------

data XDimension = XDim
data YDimension = YDim

newtype Ortho d a = Ortho {unOrtho::a} deriving (Floating, RealFrac, Real, Fractional, Num, Ord, Eq, Ix, Enum, Integral)

toXOrtho :: a -> Ortho XDimension a
toXOrtho x = Ortho x
toYOrtho :: a -> Ortho YDimension a
toYOrtho y = Ortho y
orthoganal :: Ortho d s -> Ortho d' s
orthoganal (Ortho z) = Ortho z

instance Functor (Ortho d) where
  fmap f (Ortho x) = Ortho (f x)

--------------------- Fast Truncate -----------------------

dOUBLEMAGIC         = 6755399441055744.0   :: CDouble  -- 2^52 * 1.5,  uses limited precisicion to floor
dOUBLEMAGICDELTA    = 1.5e-8               :: CDouble  -- almost .5f =.5f + 1e^(number of exp bit)
dOUBLEMAGICROUNDEPS = 0.5-dOUBLEMAGICDELTA :: CDouble  -- almost .5f =.5f - 1e^(number of exp bit)

{-# INLINE fastTruncateDouble #-}
fastTruncateDouble x = {-trWith showBin "fastTruncateDouble" $-} (unsafeCoerce :: CDouble -> Int) (x - dOUBLEMAGICROUNDEPS + dOUBLEMAGIC) .&. 0x0000FFFF

{-# INLINE fastRoundDouble #-}
fastRoundDouble    x = {-trWith showBin "fastRoundDouble" $-} (unsafeCoerce :: CDouble -> Int) (x + dOUBLEMAGIC) .&. 0x0000FFFF

sINGLEMAGIC = (2 ^ 23) {-*  1.5-} {-6291456.0-} :: CFloat  -- 2^22 * 1.5,  uses limited precisicion to floor
sINGLEMAGICDELTA    = 1.5e-8                    :: CFloat  -- almost .5f =.5f + 1e^(number of exp bit)
sINGLEMAGICROUNDEPS = 0.5-sINGLEMAGICDELTA      :: CFloat  -- almost .5f =.5f - 1e^(number of exp bit)

{-# INLINE fastTruncateSingle #-}
fastTruncateSingle x = {-trWith showBin ("fastTruncateSingle" ++ show x ++ "-->") $-} (unsafeCoerce :: CFloat -> Int) (x - sINGLEMAGICROUNDEPS + sINGLEMAGIC) .&. 0x0000FFFF

showTest = map fastTruncateSingle [4,4.1,4.5,4.7, 4.999, 4.99999]

{-# INLINE fastRoundSingle #-}
fastRoundSingle   x = {-trWith showBin "fastRoundSingle" $-} (unsafeCoerce :: CFloat -> Int) (x + sINGLEMAGIC) .&. 0x0000FFFF

--------------------- Show --------------------------

instance Show a => Show (Ortho XDimension a) where
  show (Ortho x) = "x:"++show x

instance Show a => Show (Ortho YDimension a) where
  show (Ortho y) = "y:"++show y

instance Show DisplaySpace where
  show (DSpace x) = showFl' 5 x --showFl' 2 x ++ "d"

instance Show IntSpace where
  show (ISpace x) = show x

------------------ Storable ------------------------

instance Storable a => Storable (Ortho d a) where
    sizeOf    _ = sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek ptr   = Ortho <$> peekByteOff ptr 0
    poke ptr a = pokeByteOff ptr 0 (unOrtho a)

instance Storable DisplaySpace where
    sizeOf    _         = sizeOf    (undefined :: DisplaySpace_)
    alignment _         = alignment (undefined :: DisplaySpace_)
    peek ptr            = DSpace <$> peek (castPtr ptr)
    poke ptr (DSpace x) = poke (castPtr ptr) x

instance Storable IntSpace where
    sizeOf   _          = sizeOf    (undefined :: IntSpace_)
    alignment _         = alignment (undefined :: IntSpace_)
    peek ptr            = ISpace . fromIntegral <$> (peek (castPtr ptr) :: IO CInt)
    poke ptr (ISpace x) = poke (castPtr ptr) (fromIntegral x :: CInt)
----------- DeepSeq --------------------------------

instance (NFData d, NFData a) => NFData (Ortho d a) where
  rnf (Ortho x) = rnf x

instance NFData DisplaySpace where
  rnf (DSpace x) = x `deepseq` ()

instance NFData IntSpace where
  rnf (ISpace x) = x `deepseq` ()

instance NFData XDimension where
  rnf x = ()

instance NFData YDimension where
  rnf x = ()

------------- Hashable --------------------

instance Hashable s => Hashable (Ortho d s) where
  hashWithSalt s (Ortho v) = s `hashWithSalt` v

instance Hashable DisplaySpace where
  hashWithSalt s (DSpace x) = s `hashWithSalt` x

instance Hashable CFloat where
  hashWithSalt s x = s `hashWithSalt` (realToFrac x :: Float)
