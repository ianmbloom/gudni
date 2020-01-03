{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
-- Module:      Data.Storable
-- Copyright:   (c) Tomas Janousek  2009
-- License:     BSD3
--
-- Maintainer:  tomi@nomi.cz
-- Stability:   experimental
--
-- The module "Data.Storable" provides an extension to the Foreign.Storable
-- type class adding support for variable-sized data types.
--
module Graphics.Gudni.Util.StorableM
  ( StorableM(..)
  , sizeOfV
  , alignmentV
  , peekV
  , pokeV
  , Offset(..)
  , module Foreign.Storable
  )
where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Int
import Data.List
import Data.Monoid
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Graphics.Gudni.Util.Debug


-- | The member functions of this class facilitate writing values of arbitrary
-- (including recursive) data types to raw memory and reading values from
-- blocks of raw memory.  The class, furthermore, includes support for
-- computing the storage requirements and alignment restrictions of storable
-- types.
--
-- This class fills the gap between Foreign.Storable and Data.Binary.  It adds
-- support for marshalling (finite) values of variable-sized data types, like
-- lists or trees, while preserving the performance and memory efficiency one
-- expects from the Storable class.  It also provides a (monadic) syntactic
-- sugar that takes care of alignment restrictions by itself and makes
-- instance deriving easy.
--
-- The primary aim of this class, as opposed to Foreign.Storable, is storing
-- values to raw memory for the purpose of sending them over a network (in a
-- homogeneous distributed environment, no endianness translation is done) or
-- dumping them to external storage.  It was not intended to be used for
-- marshalling structures to/from C, although it may be used for that --
-- you'll need, however, specially crafted instances for compound data types
-- that apply alignment restrictions recursively, not only for elementary
-- Storable values.  These may be provided someday.
--
-- The API used for writing/reading values is provided by the 'sizeOfV',
-- 'alignmentV', 'peekV' and 'pokeV' functions (V standing for value).
--
-- For help on deriving instances see the source of the
-- Data.Storable.Instances module.  For help on usage of the 'Ptr' type, which
-- represents raw memory addresses, see the documentation of Foreign Function
-- Interface (FFI).
--
-- Minimal complete definition: 'sizeOfM', 'alignmentM', 'peekM' and 'pokeM'.
--
class StorableM a where
    sizeOfM    :: a -> SizeOf ()
    alignmentM :: a -> Alignment () -- this must not use the argument value
    peekM      :: Offset a
    pokeM      :: a -> Offset ()

type SizeOf a = State Int a
type Alignment a = Writer (LCM Int) a


----------- Storable-like API -----------

-- | Computes the storage requirements (in bytes) of the argument.
-- The value of the argument _is_ used.
sizeOfV :: (StorableM a) => a -> Int
sizeOfV a = execState (sizeOfM a) 0
{-# INLINE sizeOfV #-}

-- | Computes the alignment constraint of the argument.  An alignment
-- constraint @x@ is fulfilled by any address divisible by @x@.
-- The value of the argument _is_not_ used.
alignmentV :: (StorableM a) => a -> Int
alignmentV = getLCM . execWriter . alignmentM
{-# INLINE alignmentV  #-}

-- | Read a value from the given memory location.
--
--  Note that the peekV and pokeV functions might require properly aligned
--  addresses to function correctly.  This is architecture dependent; thus,
--  portable code should ensure that when peeking or poking values of some
--  type @a@, the alignment constraint for @a@, as given by the function
--  `alignmentV' is fulfilled.
peekV :: (StorableM a) => Ptr a -> IO a
peekV p = runOffset p peekM
{-# INLINE peekV  #-}

-- | Write the given value to the given memory location.  Alignment
-- restrictions might apply; see 'peekV'.
pokeV :: (StorableM a) => Ptr a -> a -> IO ()
pokeV p = runOffset p . pokeM
{-# INLINE pokeV  #-}


----------- The Offset monad -----------

-- | The Offset monad acts like a Reader for the pointer element and like a
-- State for the offset element.  It is used to provide the syntactic sugar
-- for instances.
type Offset a = StateT (Ptr (), Int) IO a
    -- the Int component is the last offset written
    -- the Ptr () component holds the base pointer (const)

-- | Run the offset monad on a given pointer, yielding a value out of it.
runOffset :: Ptr b -> Offset a -> IO a
runOffset p m = evalStateT m (castPtr p, 0)
{-# INLINE runOffset #-}

----------- Helper stuff -----------

-- | Align a given offset using a given alignment constraint.
align :: Int -> Int -> Int
align !o !a = o + case o `rem` a of
                     0 -> 0
                     x -> a - x
{-# INLINE align #-}

-- | Pad a given offset range with zeros.
zeroPad :: Ptr a -> Int -> Int -> IO ()
zeroPad p !o1 !o2 = when (o2 > o1) $
    pokeArray (castPtr p `plusPtr` o1) $ replicate (o2 - o1) (0 :: Int8)
{-# INLINE zeroPad #-}

-- | Monoid under least common multiple.
newtype LCM a = LCM { getLCM :: a }
        deriving (Eq, Ord, Read, Show, Bounded)

instance Integral a => Semigroup (LCM a) where
         LCM x <> LCM y = LCM (x `lcm` y)
         {-# INLINE (<>) #-}

instance Integral a => Monoid (LCM a) where
        mempty = LCM 1
        {-# INLINE mempty #-}

----------- Storable instance -----------

-- The base instance for Storable types.  At the moment, this is the _only_
-- instance driving alignment and padding.  That means structures will be
-- aligned on Storable elements, not in a recursive manner, ie.
-- (5 :: Int16, (2 :: Int8, 1 :: Int32)) is stored as 0500020001000000, not
-- 050000000200000001000000 what may be expected since the alignment
-- constraint for (2 :: Int8, 1 :: Int32) is 4.  This is a feature.
instance {-# OVERLAPPABLE #-} (Storable a) => StorableM a where
    sizeOfM v = do o <- get
                   let o' = align o (alignment (undefined :: a))
                       o'' = o' + sizeOf v -- this used to be (undefined :: a)
                   put $! o''
    {-# INLINE sizeOfM #-}
    alignmentM = tell . LCM . alignment
    {-# INLINE alignmentM #-}
    peekM = do (p, o) <- get
               let o' = align o (alignment (undefined :: a))
               v <- liftIO $ peek (p `plusPtr` o')
               put (p, o' + sizeOf v)
               return v
    {-# INLINE peekM #-}
    pokeM v = do (p, o) <- get
                 let o' = align o (alignment v)
                 liftIO $ zeroPad p o o'
                 liftIO $ poke (p `plusPtr` o') v
                 put (p, o' + sizeOf v)
    {-# INLINE pokeM #-}
