{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.ShapeTree
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers for putting values into particular coordinate space.

module Graphics.Gudni.Figure.Space
  ( HasSpace (..)
  , HasWidth (..)
  , HasHeight(..)
  , Ortho (..)
  , XDimension
  , YDimension
  , toXOrtho
  , toYOrtho
  , orthoganal
  , Iota (..)
  , SubSpace (..)
  , PixelSpace (..)
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

type SubSpace_ = Float
-- | SubSpace is short for subpixel space. A floating point value where one square unit refers to one pixel.
newtype SubSpace = SubSpace {unSubSpace :: SubSpace_} deriving (Enum, RealFrac, Fractional, Real, Num, Ord, Eq, RealFloat, Floating)

type PixelSpace_ = Int32
-- | Pixel space is pixel boundary space. An integer value where one square unit refers to one pixel.
newtype PixelSpace     = ISpace {unISpace :: PixelSpace_ } deriving (Integral, Enum, Ix, Real, Num, Ord, Eq)

class HasSpace a where
  type SpaceOf a

class HasSpace a => HasWidth a where
  widthOf :: a -> Ortho XDimension (SpaceOf a)

class HasSpace a => HasHeight a where
  heightOf :: a -> Ortho YDimension (SpaceOf a)

-- ---- Iota -----------------
-- Iota is an extremely small value used for "close enough" comparison.

class Iota s where
  iota :: s

instance Iota s => Iota (Ortho d s) where
  iota = Ortho iota

instance Iota SubSpace where
  iota = SubSpace 0.0001

instance Iota PixelSpace where
  iota = ISpace 1

--------------------- Random -----------------
instance Random SubSpace where
  random = runRand $ do value <-  getRandomR (-10, 2880); return $ SubSpace value
  randomR (SubSpace l, SubSpace h)= runRand $ do value <- getRandomR (l, h); return $ SubSpace value

-------------------------------------
-- Orthoganal Values
-- This is an easy way to prevent accidentally mixing up x and y values when working with points.

-- | Phantom type for components in the x dimension.
data XDimension = XDim
-- | Phantom type for components in the y dimension.
data YDimension = YDim

-- | Newtype wrapper for values that are an orthagonal component of a vector.
newtype Ortho d a = Ortho {unOrtho::a} deriving (Floating, RealFrac, Real, Fractional, Num, Ord, Eq, Ix, Enum, Integral)

-- | Make a value into an x component.
toXOrtho :: a -> Ortho XDimension a
toXOrtho x = Ortho x
-- | Make a value into a y component.
toYOrtho :: a -> Ortho YDimension a
toYOrtho y = Ortho y
-- | Explicitly switch from one component type to the other.
orthoganal :: Ortho d s -> Ortho d' s
orthoganal (Ortho z) = Ortho z

instance Functor (Ortho d) where
  fmap f (Ortho x) = Ortho (f x)

--------------------- Show --------------------------

instance Show a => Show (Ortho XDimension a) where
  show (Ortho x) = "x:"++show x

instance Show a => Show (Ortho YDimension a) where
  show (Ortho y) = "y:"++show y

instance Show SubSpace where
  show (SubSpace x) = showFl' 5 x --showFl' 2 x ++ "d"

instance Show PixelSpace where
  show (ISpace x) = show x

------------------ Storable ------------------------

instance Storable a => Storable (Ortho d a) where
    sizeOf    _ = sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek ptr   = Ortho <$> peekByteOff ptr 0
    poke ptr a = pokeByteOff ptr 0 (unOrtho a)

instance Storable SubSpace where
    sizeOf    _         = sizeOf    (undefined :: SubSpace_)
    alignment _         = alignment (undefined :: SubSpace_)
    peek ptr            = SubSpace . (realToFrac :: CFloat -> Float) <$> peek (castPtr ptr)
    poke ptr (SubSpace x) = poke (castPtr ptr) $ (realToFrac :: Float -> CFloat) x

instance Storable PixelSpace where
    sizeOf   _          = sizeOf    (undefined :: PixelSpace_)
    alignment _         = alignment (undefined :: PixelSpace_)
    peek ptr            = ISpace . fromIntegral <$> (peek (castPtr ptr) :: IO CInt)
    poke ptr (ISpace x) = poke (castPtr ptr) (fromIntegral x :: CInt)

----------- DeepSeq --------------------------------

instance (NFData d, NFData a) => NFData (Ortho d a) where
  rnf (Ortho x) = rnf x

instance NFData SubSpace where
  rnf (SubSpace x) = x `deepseq` ()

instance NFData PixelSpace where
  rnf (ISpace x) = x `deepseq` ()

instance NFData XDimension where
  rnf x = ()

instance NFData YDimension where
  rnf x = ()

------------- Hashable --------------------

instance Hashable s => Hashable (Ortho d s) where
  hashWithSalt s (Ortho v) = s `hashWithSalt` v

instance Hashable SubSpace where
  hashWithSalt s (SubSpace x) = s `hashWithSalt` x

instance Hashable CFloat where
  hashWithSalt s x = s `hashWithSalt` (realToFrac x :: Float)
