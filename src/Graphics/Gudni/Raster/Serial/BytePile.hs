{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Util.Pile
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A pile is an appendable buffer of data that reallocates memory as it grows∘
-- Piles can be reused without reallocating memory.

module Graphics.Gudni.Raster.Serial.BytePile
  ( BytePile (..)
  , AsBytes(..)
  , asBytes
  , bytePileToCharList
  , bytePileToIntList
  , bytePileToShortList
  , bytePileToGeometry
  , bytePileToFloatList
  )
where

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Raster.Thresholds.Constants
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Data.List
import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as VS
import qualified Data.Sequence as S

import Control.Monad
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Lens

import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array(peekArray)

import GHC.Ptr

type BytePile = Pile CChar

newtype AsBytes t = AsBytes {unAsBytes :: t}

instance Show t => Show (AsBytes t) where
  show (AsBytes t) = show t

asBytes = AsBytes

instance (Storable a) => CanPile CChar (AsBytes a) where
    -- | Add any type with an instance of Storable to a pile or bytes.
    addToPile pile@(Pile cursor allocated startPtr) (AsBytes item) =
        let end = cursor + fromIntegral (sizeOf item)
        in
        if end <= allocated
        then do liftIO $ poke (startPtr `plusPtr` fromIntegral cursor) item
                return (Pile end allocated startPtr, Ref . fromIntegral $ cursor)
        else do e <- extendPile pile
                addToPile e (AsBytes item)

instance (Storable a) => CanLoad CChar (AsBytes a) where
    fromPile pile index = AsBytes    <$> (liftIO $ peek (castPtr $ ptrFromIndex pile index (undefined :: CChar)))
    toPile   pile index  (AsBytes item) = liftIO $ poke (castPtr $ ptrFromIndex pile index (undefined :: CChar)) item

-- | Make a bytepile into a list of CChars.
bytePileToCharList :: BytePile -> IO [CChar]
bytePileToCharList (Pile cursor allocated startPtr) = peekArray (fromIntegral cursor) startPtr

-- | Make a bytepile into a list of CInts.
bytePileToIntList :: BytePile -> IO [CInt]
bytePileToIntList (Pile cursor allocated startPtr) = peekArray (fromIntegral cursor `div` sizeOf (undefined::CInt)) (castPtr startPtr :: Ptr CInt)

-- | Make a bytepile into a list of strings to display its contents.
bytePileToShortList :: BytePile -> IO [String]
bytePileToShortList (Pile cursor allocated startPtr) =
  do
    ss <- peekArray (fromIntegral cursor `div` sizeOf (undefined::CShort)) (castPtr startPtr :: Ptr CShort)
    return $ zipWith (\x y -> show x ++ ":" ++ show y) (map (*2) [0..]) ss

-- | Breakdown a list into fixed sections.
section :: Int -> [a] -> [[a]]
section n []   = []
section n list = let (front, rest) = splitAt n list
                 in front:section n rest

-- | Display the contents of a geometry pile in a readable format for debugging
bytePileToGeometry :: BytePile -> IO [String]
bytePileToGeometry (Pile cursor allocated startPtr) =
  do
    ss <- liftIO $ peekArray (fromIntegral cursor `div` sizeOf (undefined::CShort)) (castPtr startPtr :: Ptr CShort)
    let sList = map (concat . intersperse ":") . section 4 . map show $ ss
    sf <- peekArray (fromIntegral cursor `div` sizeOf (undefined::CFloat)) (castPtr startPtr :: Ptr CFloat)
    let fList = map (concat . intersperse ":") . section 2 . map (showFl' 6) $ sf
    return $ zipWith3 (\x y z -> show x ++ ": " ++ show y ++ "|" ++ show z) (map (*2) [0..]) fList sList

-- | Breakdown a bytepile into a list of word alligned floats.
bytePileToFloatList :: Int -> BytePile -> IO [String]
bytePileToFloatList offset (Pile cursor allocated startPtr) =
  do ss <- peekArray (fromIntegral cursor `div` sizeOf (undefined::CFloat)) (castPtr (startPtr `plusPtr` offset) :: Ptr CFloat)
     return $ zipWith (\x y -> show x ++ ":" ++ showFl' 6 y) (map (*2) [0..]) ss
