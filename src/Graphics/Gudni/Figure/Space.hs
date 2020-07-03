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
-- Module      :  Graphics.Gudni.Figure.Space
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers for putting values into particular coordinate space.

module Graphics.Gudni.Figure.Space
  ( SimpleSpace (..)
  , Space (..)
  , HasSpace (..)
  , Reasonable (..)
  , Iota (..)
  , SubSpace (..)
  , PixelSpace (..)
  , TextureSpace (..)
  , showBin
  , textureSpaceToSubspace
  , pixelSpaceToTextureSpace
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
import Control.DeepSeq

import System.Random

import Linear.Epsilon
import Numeric.Limits

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import GHC.Generics

showBin i = printf "The value of %d in hex is: 0x%08x and binary is: %b\n" i i i

type SimpleSpace s = (Num s, Eq s, Ord s, Bounded s, Show s)
type Space s = (SimpleSpace s, Floating s, Real s, Fractional s, Epsilon s, Iota s, RealFrac s)

type SubSpace_ = Float
-- | SubSpace is short for subpixel space. A floating point value where one square unit refers to one pixel.
newtype SubSpace = SubSpace {unSubSpace :: SubSpace_} deriving (Enum, Epsilon, RealFrac, Fractional, Real, Num, Ord, Eq, RealFloat, Floating, Generic)
instance Out SubSpace where
    doc x = text . show . unSubSpace $ x
    docPrec _ = doc
instance Bounded SubSpace where { minBound = SubSpace (-maxValue); maxBound = SubSpace maxValue }

type PixelSpace_ = Int32
-- | Pixel space is pixel boundary space. An integer value where one square unit refers to one pixel.
newtype PixelSpace     = PSpace {unPSpace :: PixelSpace_ } deriving (Bounded, Integral, Enum, Ix, Real, Num, Ord, Eq)
type TextureSpace_ = Float

newtype TextureSpace = TSpace {unTSpace :: TextureSpace_ } deriving (Enum, Epsilon, RealFrac, Fractional, Real, Num, Ord, Eq, RealFloat, Floating, Generic)
instance Out TextureSpace where
    doc (TSpace x) = text . show $ x
    docPrec _ = doc
instance Bounded TextureSpace where { minBound = (-maxValue); maxBound = maxValue }

class (Space (SpaceOf a)) => HasSpace a where
  type SpaceOf a :: Type

class Reasonable s where
  veryLarge :: s
  clampReasonable :: s -> s

instance Reasonable SubSpace where
  veryLarge = 2 ^ 16
  clampReasonable =
      clamp (-veryLarge) veryLarge

instance (HasSpace a) => HasSpace [a] where
  type SpaceOf [a] = SpaceOf a

instance (HasSpace a) => HasSpace (V.Vector a) where
  type SpaceOf (V.Vector a) = SpaceOf a

textureSpaceToSubspace (TSpace x) = SubSpace x
pixelSpaceToTextureSpace (PSpace x) = TSpace (fromIntegral x)
-- ---- Iota -----------------
-- Iota is an extremely small value used for "close enough" comparison.

class Iota s where
  iota :: s

instance Iota SubSpace where
  iota = SubSpace 0.0001

instance Iota PixelSpace where
  iota = PSpace 1

instance Iota TextureSpace where
  iota = TSpace 0.0001

--------------------- Random -----------------
instance Random SubSpace where
  random = runRand $ do value <-  getRandomR (-10, 2880); return $ SubSpace value
  randomR (SubSpace l, SubSpace h)= runRand $ do value <- getRandomR (l, h); return $ SubSpace value

instance Random TextureSpace where
  random = runRand $ do value <-  getRandomR (-10, 2880); return $ TSpace value
  randomR (TSpace l, TSpace h)= runRand $ do value <- getRandomR (l, h); return $ TSpace value

-------------------- Show --------------------------

instance Show SubSpace where
  show (SubSpace x) = (if x < 0 then \string -> "("++string++")" else id) $ showFl' 5 x

instance Show TextureSpace where
  show (TSpace x) = showFl' 5 x

instance Show PixelSpace where
  show (PSpace x) = show x

------------------ Storable ------------------------

instance Storable SubSpace where
    sizeOf    _         = sizeOf    (undefined :: SubSpace_)
    alignment _         = alignment (undefined :: SubSpace_)
    peek ptr            = SubSpace . (realToFrac :: CFloat -> Float) <$> peek (castPtr ptr)
    poke ptr (SubSpace x) = poke (castPtr ptr) $ (realToFrac :: Float -> CFloat) x

instance Storable TextureSpace where
    sizeOf    _         = sizeOf    (undefined :: TextureSpace_)
    alignment _         = alignment (undefined :: TextureSpace_)
    peek ptr            = TSpace . (realToFrac :: CFloat -> Float) <$> peek (castPtr ptr)
    poke ptr (TSpace x) = poke (castPtr ptr) $ (realToFrac :: Float -> CFloat) x

instance Storable PixelSpace where
    sizeOf   _          = sizeOf    (undefined :: PixelSpace_)
    alignment _         = alignment (undefined :: PixelSpace_)
    peek ptr            = PSpace . fromIntegral <$> (peek (castPtr ptr) :: IO CInt)
    poke ptr (PSpace x) = poke (castPtr ptr) (fromIntegral x :: CInt)

----------- DeepSeq --------------------------------

instance NFData SubSpace where
  rnf (SubSpace x) = x `deepseq` ()

instance NFData TextureSpace where
  rnf (TSpace x) = x `deepseq` ()


instance NFData PixelSpace where
  rnf (PSpace x) = x `deepseq` ()

------------- Hashable --------------------

instance Hashable SubSpace where
  hashWithSalt s (SubSpace x) = s `hashWithSalt` x

instance Hashable TextureSpace where
    hashWithSalt s (TSpace x) = s `hashWithSalt` x


instance Hashable CFloat where
  hashWithSalt s x = s `hashWithSalt` (realToFrac x :: Float)
