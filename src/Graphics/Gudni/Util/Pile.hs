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

module Graphics.Gudni.Util.Pile
  ( Reference (..)
  , nullReference
  , Breadth (..)
  , refToBreadth
  , Slice (..)
  , Pile (..)
  , CanPile (..)
  , pileCursor
  , pileBreadth
  , pileToList
  , listToPile
  , extendPile
  , addToPileState
  , newPile
  , newPileSize
  , freePile
  , resetPile
  , isEmptyPile
  , BytePile (..)
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
import Graphics.Gudni.Raster.Constants

import Data.List
import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as VS
import qualified Data.Sequence as S

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Lens

import Foreign.C.Types
import Foreign.Marshal.Utils(copyBytes)
import Foreign.Marshal.Array(peekArray)
import Foreign.Storable
import Foreign.Marshal.Alloc

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import GHC.Ptr

-- Additional amount to reallocate with each step.
dEFAULTpILEaLLOCATION = 1024 :: Int

type Reference_ = CUInt
instance Out Reference_ where
    doc x = text . show $ x
    docPrec _ = doc

nullReference = Ref nULLrEFERENCE

newtype Reference t = Ref {unRef :: Reference_}    deriving (Eq, Ord, Num, Enum, Real, Integral, Generic)
instance Show (Reference t) where
  show (Ref i) = show i
instance Out (Reference t)

instance NFData (Reference t) where
  rnf (Ref i) = i `deepseq` ()

instance Storable (Reference t) where
  sizeOf    _ = sizeOf    (undefined :: Reference_)
  alignment _ = alignment (undefined :: Reference_)
  poke ptr (Ref i) = poke (castPtr ptr) i
  peek ptr = Ref <$> peek (castPtr ptr)

type Breadth_ = CUInt
newtype Breadth t = Breadth {unBreadth :: Breadth_}  deriving (Eq, Ord, Num, Enum, Real, Integral)
instance Show (Breadth t) where
  show (Breadth i) = show i

instance NFData (Breadth t) where
  rnf (Breadth i) = i `deepseq` ()

instance Storable (Breadth t) where
  sizeOf    _ = sizeOf    (undefined :: Breadth_)
  alignment _ = alignment (undefined :: Breadth_)
  poke ptr  (Breadth i) = poke (castPtr ptr) i
  peek ptr = Breadth <$> peek (castPtr ptr)

refToBreadth :: Reference t -> Breadth t
refToBreadth (Ref i) = Breadth i

-- | A slice defines a range in an array with the starting point and the length.
data Slice t = Slice
  { sliceStart   :: !(Reference t)
  , sliceLength  :: !(Breadth t)
  } deriving (Eq)

instance Show (Slice t) where
  show (Slice (Ref a) (Breadth b)) = "(" ++ show a ++ "," ++ show b ++ ")"

instance NFData t => NFData (Slice t) where
  rnf (Slice a b) = a `deepseq` b `deepseq` ()

instance StorableM (Slice t) where
  sizeOfM _ = do sizeOfM (undefined :: Reference t)
                 sizeOfM (undefined :: Breadth t  )
  alignmentM _ = do alignmentM (undefined :: Reference t)
                    alignmentM (undefined :: Breadth t  )
  peekM =
    do start <- peekM
       len   <- peekM
       return (Slice start len)
  pokeM (Slice start len) =
     do pokeM start
        pokeM len

instance Storable (Slice t) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

-- | A pile is an appendable memory buffer that reallocates memory as it grows.
data Pile t = Pile
  { -- The current position of the write head.
    _pileCursor :: !Int
    -- The current total allocated units of t for the buffer.
  , _pileAllocated   :: !Int
    -- The pointer to the start of the buffer.
  , _pileData   :: !(Ptr t)
  } deriving (Show)

-- | Lens for the current position of the write head.
pileCursor :: Lens' (Pile t) (Reference t)
pileCursor plFunc (Pile cursor allocated ptr) = (\(Ref cursor') -> Pile (fromIntegral cursor') allocated ptr) <$> plFunc (Ref . fromIntegral $ cursor)

-- | Lens for the current size of the pile.
pileBreadth :: (Functor f, Contravariant f) => Optic' (->) f (Pile t) (Breadth t)
pileBreadth = pileCursor . to refToBreadth

newPile :: forall t a . (Storable t) => IO (Pile t)
newPile = newPileSize dEFAULTpILEaLLOCATION

-- | Create a new pile.
newPileSize :: forall t a . (Storable t) => Int -> IO (Pile t)
newPileSize allocated =
  do
    ptr <- mallocBytes (allocated * sizeOf (undefined :: t))
    return (Pile 0 allocated ptr)

-- | Free the memory allocated by a pile.
freePile :: Pile a -> IO ()
freePile (Pile _ _ ptr) = free ptr

-- | Reset the start point of a tile as though it were new.
resetPile :: Pile t -> Pile t
resetPile (Pile _ allocated ptr) = Pile 0 allocated ptr

-- | Return true if a pile has any items.
isEmptyPile :: Pile t -> Bool
isEmptyPile (Pile c _ _ ) = c == 0

instance (NFData t) => NFData (Pile t) where
  rnf (Pile a b _ ) = a `deepseq` b `deepseq` ()

-- | Reallocate the pile with new space.
extendPile :: forall t . Storable t => Pile t -> IO (Pile t)
extendPile pile@(Pile cursor allocated startPtr) =
  do
    let itemSize = sizeOf (undefined :: t)
        newSize = allocated * 2
    newPtr <- reallocBytes startPtr (newSize * itemSize)
    return $ Pile cursor newSize newPtr

addToPile' :: forall t. (Show t, Storable t) => Pile t -> t -> IO (Pile t)
addToPile' pile@(Pile cursor allocated startPtr) item =
  if cursor < allocated
  then
    do poke (startPtr `plusPtr` (cursor * sizeOf (undefined :: t))) item
       return $ Pile (cursor + 1) allocated startPtr
  else
    do e <- extendPile pile
       addToPile' e item

class (Show a, Show b, Storable a) => CanPile a b where
  addToPile :: Pile a -> b -> IO (Pile a, Slice a)

-- | Add an item to a pile within a StateT monad transformer with a lens to the pile within the state.
-- updating the state along the way.
addToPileState :: (Storable a, Show a, CanPile a b, MonadState s m, MonadIO m)
               => Lens' s (Pile a)
               -> b
               -> m (Slice a)
addToPileState lens object =
  do pile <- use lens
     (pile', ref) <- liftIO $ addToPile pile object
     lens .= pile'
     return ref

addFoldableToPile :: (Show a, Storable a, Foldable f) => Pile a -> f a -> IO (Pile a, Slice a)
addFoldableToPile pile foldable =
   do let cursor = pile ^. pileCursor
      pile' <- foldM addToPile' pile foldable
      let breadth = pile' ^. pileCursor - cursor
      return (pile', Slice (Ref $ fromIntegral cursor) (Breadth $ fromIntegral breadth))

instance (Show a, Storable a) => CanPile a a where
    addToPile pile@(Pile cursor allocated startPtr) item =
      do pile <- addToPile' pile item
         return (pile, Slice (Ref $ fromIntegral cursor) (Breadth 1))

instance (Show a, Storable a) => CanPile a [a] where
    addToPile = addFoldableToPile

instance (Show a, Storable a) => CanPile a (S.Seq a) where
    addToPile = addFoldableToPile

addToPileFromPtr :: forall t a . (Show t, Storable t) => Pile t -> Ptr a -> Int -> IO (Pile t, Slice t)
addToPileFromPtr destination@(Pile destCursor destAllocated startPtr) sourcePtr sourceSize =
    let
        sourceSizeInBytes = sourceSize * sizeOf (undefined :: t)
        cursorPtr = startPtr `plusPtr` (destCursor * sizeOf (undefined :: t))
        end = destCursor + sourceSize
    in
    if end < destAllocated
    then
      do copyBytes cursorPtr sourcePtr sourceSizeInBytes
         return (destination {_pileCursor = destCursor + sourceSize}, Slice (Ref $ fromIntegral destCursor) (Breadth $ fromIntegral sourceSize))
    else
      do e <- extendPile destination
         addToPileFromPtr e sourcePtr sourceSize

instance (Show a, Storable a) => CanPile a (Pile a) where
    -- | Copy the contents of a pile into another pile.
    addToPile destination source =
       let sourcePtr = _pileData source
           sourceSize = _pileCursor source
       in  addToPileFromPtr destination sourcePtr sourceSize

instance (Show a, Storable a) => CanPile a (VS.Vector a) where
    -- | Add a vector of items to a pile. Return a reference to the beggining.
    addToPile destination vector =
      VS.unsafeWith vector $ \sourcePtr ->
      let sourceSize =  VS.length vector
      in  addToPileFromPtr destination sourcePtr sourceSize

type BytePile = Pile CChar

newtype AsBytes t = AsBytes {unAsBytes :: t}

instance Show t => Show (AsBytes t) where
  show (AsBytes t) = show t

asBytes = AsBytes

instance (Show a, Storable a) => CanPile CChar (AsBytes a) where
    -- | Add any type with an instance of Storable to a pile or bytes.
    addToPile pile@(Pile cursor allocated startPtr) (AsBytes item) =
      let end = cursor + sizeOf item
      in
      if end < allocated
      then
        do --putStrLn $ "aboutToPoke" ++ show pile
           poke (startPtr `plusPtr` cursor) item
           return $ (Pile end allocated startPtr, Slice (Ref . fromIntegral $ cursor) (Breadth . fromIntegral $ sizeOf item))
      else
        do
           e <- extendPile pile
           addToPile e (AsBytes item)

-- | Convert a pile to a list.
pileToList :: forall t . (Storable t, Show t) => Pile t -> IO [t]
pileToList (Pile cursor allocated startPtr) = peekArray cursor startPtr

-- | Convert a list to a pile.
listToPile :: forall t . (Storable t, Show t) => [t] -> IO (Pile t)
listToPile list = do pile <- newPile
                     (pile', _) <- addToPile pile list
                     return pile'

-- | Make a bytepile into a list of CChars.
bytePileToCharList :: BytePile -> IO [CChar]
bytePileToCharList (Pile cursor allocated startPtr) = peekArray cursor startPtr

-- | Make a bytepile into a list of CInts.
bytePileToIntList :: BytePile -> IO [CInt]
bytePileToIntList (Pile cursor allocated startPtr) = peekArray (cursor `div` sizeOf (undefined::CInt)) (castPtr startPtr :: Ptr CInt)

-- | Make a bytepile into a list of strings to display its contents.
bytePileToShortList :: BytePile -> IO [String]
bytePileToShortList (Pile cursor allocated startPtr) =
  do
    ss <- peekArray (cursor `div` sizeOf (undefined::CShort)) (castPtr startPtr :: Ptr CShort)
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
    ss <- peekArray (cursor `div` sizeOf (undefined::CShort)) (castPtr startPtr :: Ptr CShort)
    let sList = map (concat . intersperse ":") . section 4 . map show $ ss
    sf <- peekArray (cursor `div` sizeOf (undefined::CFloat)) (castPtr startPtr :: Ptr CFloat)
    let fList = map (concat . intersperse ":") . section 2 . map (showFl' 6) $ sf
    return $ zipWith3 (\x y z -> show x ++ ": " ++ show y ++ "|" ++ show z) (map (*2) [0..]) fList sList

-- | Breakdown a bytepile into a list of word alligned floats.
bytePileToFloatList :: Int -> BytePile -> IO [String]
bytePileToFloatList offset (Pile cursor allocated startPtr) =
  do ss <- peekArray (cursor `div` sizeOf (undefined::CFloat)) (castPtr (startPtr `plusPtr` offset) :: Ptr CFloat)
     return $ zipWith (\x y -> show x ++ ":" ++ showFl' 6 y) (map (*2) [0..]) ss
