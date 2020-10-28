{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Principle.Space
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers for putting values into particular coordinate space.

module Graphics.Gudni.Figure.Principle.Space
  ( SimpleSpace (..)
  , Space (..)
  , HasSpace (..)
  , Reasonable (..)
  , Iota (..)
  , SubSpace_(..)
  , SubSpace (..)
  , PixelSpace_(..)
  , PixelSpace (..)
  , showBin
  , pixelSpaceToSubSpace
  , floatToSubSpace
  , subSpaceToFloat
  )
where

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Data.Array
import Data.Int
import Data.Word
import Data.Char
import Data.Bits
import Data.Hashable
-- import qualified Data.Vector.Storable as V
import qualified Data.Vector as V

import Data.Kind

import GHC.Float (double2Float)

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types (CFloat, CDouble, CInt, CShort)
import Unsafe.Coerce

import Text.Printf

import Control.Monad.Random
import Control.Monad.Identity

import System.Random

import Linear.Epsilon
import Linear.V2
import Linear.V3
import Linear.V4

import Numeric.Limits

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import GHC.Generics

showBin i = printf "The value of %d in hex is: 0x%08x and binary is: %b\n" i i i

type SimpleSpace s = (Num s, Eq s, Ord s, Bounded s, Show s, Out s)
type Space s = (SimpleSpace s, Floating s, Real s, Fractional s, Epsilon s, Iota s, RealFrac s, Reasonable s)

type SubSpace_ = Float
-- | SubSpace is short for subpixel space. A floating point value where one square unit refers to one pixel.
newtype SubSpace = SubSpace {unSubSpace :: SubSpace_} deriving (Enum, Epsilon, RealFrac, Fractional, Real, Num, Ord, Eq, RealFloat, Floating, Generic)
instance Out SubSpace where
    doc x = text . show $ x
    docPrec _ = doc
instance Bounded SubSpace where { minBound = SubSpace (-maxValue); maxBound = SubSpace maxValue }

floatToSubSpace :: Float -> SubSpace
floatToSubSpace = SubSpace

subSpaceToFloat :: SubSpace -> Float
subSpaceToFloat = unSubSpace

type PixelSpace_ = Int32
-- | Pixel space is pixel boundary space. An integer value where one square unit refers to one pixel.
newtype PixelSpace     = PSpace {unPSpace :: PixelSpace_ } deriving (Bounded, Integral, Enum, Ix, Real, Num, Ord, Eq)
instance Out PixelSpace where
    doc x = text . show $ x
    docPrec _ = doc

class (Space (SpaceOf a)) => HasSpace a where
  type SpaceOf a :: Type

instance HasSpace a => HasSpace (Maybe a) where
  type SpaceOf (Maybe a) = SpaceOf a

instance HasSpace a => HasSpace (Identity a) where
  type SpaceOf (Identity a) = SpaceOf a

instance HasSpace t => HasSpace (V2 t) where
  type SpaceOf (V2 t) = SpaceOf t

instance HasSpace t => HasSpace (V3 t) where
  type SpaceOf (V3 t) = SpaceOf t

instance HasSpace t => HasSpace (V4 t) where
  type SpaceOf (V4 t) = SpaceOf t

class Reasonable s where
  veryLarge :: s
  clampReasonable :: s -> s

instance Reasonable SubSpace where
  veryLarge = 2 ^ 16
  clampReasonable =
      clamp (-veryLarge) veryLarge

instance Reasonable PixelSpace where
  veryLarge = 2 ^ 16
  clampReasonable =
      clamp (-veryLarge) veryLarge

pixelSpaceToSubSpace (PSpace x) = SubSpace (fromIntegral x)
-- ---- Iota -----------------
-- Iota is an extremely small value used for "close enough" comparison.

class Iota s where
  iota :: s

instance Iota SubSpace where
  iota = SubSpace 0.0001

instance Iota PixelSpace where
  iota = PSpace 1

--------------------- Random -----------------
instance Random SubSpace where
  random = runRand $ do value <-  getRandomR (-10, 2880); return $ SubSpace value
  randomR (SubSpace l, SubSpace h)= runRand $ do value <- getRandomR (l, h); return $ SubSpace value

-------------------- Show --------------------------

instance Show SubSpace where
  show x | x == minBound = "(minBound)"
         | x == maxBound = "maxBound"
         | otherwise = (if x < 0 then \string -> "("++string++")" else id) . showFl' 3 . unSubSpace $ x

instance Show PixelSpace where
  show (PSpace x) = show x

------------- Hashable --------------------

instance Hashable SubSpace where
  hashWithSalt s (SubSpace x) = s `hashWithSalt` x

instance Hashable CFloat where
  hashWithSalt s x = s `hashWithSalt` (realToFrac x :: Float)
