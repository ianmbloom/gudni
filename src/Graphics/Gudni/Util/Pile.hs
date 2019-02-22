{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}

module Graphics.Gudni.Util.Pile
  ( Reference (..)
  , nULLREFERENCE
  , Breadth (..)
  , refToBreadth
  , zEROBREADTH
  , Slice (..)
  , Pile (..)
  , pileCursor
  , pileBreadth
  , pileSize
  , getPileSlice
  , getPileSliceVector
  , getPileItem
  , pileTop
  , replacePileTop
  , pileToList
  , listToPile
  , addToPile
  , extendPile
  , addListToPile
  , addPileToPile
  , addToPileState
  , addListToPileState
  , addVectorToPile
  , newPile
  , newPileSize
  , freePile
  , resetPile
  , isEmptyPile
  , BytePile (..)
  , addToBytePile
  , canAddToBytePile
  , addVectorToBytePile
  , bytePileToCharList
  , bytePileToIntList
  , bytePileToShortList
  , bytePileToGeometry
  , bytePileToFloatList
  )
where

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM

import Data.List
import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as VS

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Class
import Control.Lens

import Foreign.C.Types
import Foreign.Marshal.Utils(copyBytes)
import Foreign.Marshal.Array(peekArray)
import Foreign.Storable
import Foreign.Marshal.Alloc

import GHC.Ptr


type Reference_ = CUInt
newtype Reference t = Ref {unRef :: Reference_}    deriving (Eq, Ord, Num, Enum, Real, Integral)
instance Show (Reference t) where
  show (Ref i) = show i

nULLREFERENCE = Ref (maxBound :: Reference_)

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

zEROBREADTH = Breadth 0

instance NFData (Breadth t) where
  rnf (Breadth i) = i `deepseq` ()

instance Storable (Breadth t) where
  sizeOf    _ = sizeOf    (undefined :: Breadth_)
  alignment _ = alignment (undefined :: Breadth_)
  poke ptr  (Breadth i) = poke (castPtr ptr) i
  peek ptr = Breadth <$> peek (castPtr ptr)

refToBreadth :: Reference t -> Breadth t
refToBreadth (Ref i) = Breadth i

data Slice t = Slice
  { sliceStart   :: !(Reference t)
  , sliceLength  :: !(Breadth t)
  } deriving (Eq, Show)

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

data Pile t = Pile
  { _pileCursor :: !Int
  , _pileSize   :: !Int
  , _pileData   :: !(Ptr t)
  } deriving (Show)

pileCursor :: Lens' (Pile t) (Reference t)
pileCursor plFunc (Pile cursor size ptr) = (\(Ref cursor') -> Pile (fromIntegral cursor') size ptr) <$> plFunc (Ref . fromIntegral $ cursor)

pileBreadth :: (Functor f, Contravariant f) => Optic' (->) f (Pile t) (Breadth t)
pileBreadth = pileCursor . to refToBreadth

pileSize :: (Functor f, Contravariant f) => Optic' (->) f (Pile t) Int
pileSize = pileCursor . to (fromIntegral . unRef)

cHUNKSIZE = (1024 :: Int)

extendPile :: forall t . Storable t => String -> Pile t -> IO (Pile t)
extendPile message pile@(Pile cursor size startPtr) =
  do
    let itemSize = sizeOf (undefined :: t)
    --putStrLn $ "extend " ++ message ++ " " ++ show pile
    newPtr <- reallocBytes startPtr ((size + cHUNKSIZE) * itemSize)
    return $ Pile cursor (size + cHUNKSIZE) newPtr

addToPile' :: forall t. (Show t, Storable t) => String -> Pile t -> t -> IO (Pile t)
addToPile' message pile@(Pile cursor size startPtr) item =
  if cursor < size
  then
    do poke (startPtr `plusPtr` (cursor * sizeOf (undefined :: t))) item
       return $ Pile (cursor + 1) size startPtr
  else
    do e <- extendPile message pile
       addToPile' message e item

addToPile :: forall t. (Show t, Storable t) => String -> Pile t -> t -> IO (Pile t, Reference t)
addToPile message pile@(Pile cursor size startPtr) item =
  do pile <- addToPile' message pile item
     return (pile, Ref $ fromIntegral cursor)

addListToPile :: (Show t, Storable t) => Pile t -> [t] -> IO (Pile t, Slice t)
addListToPile pile list =
  do let cursor = pile ^. pileCursor
     pile' <- foldM (addToPile' "list") pile list
     let breadth = pile' ^. pileCursor - cursor
     return (pile', Slice (Ref $ fromIntegral cursor) (Breadth $ fromIntegral breadth))

addPileToPile :: forall t . (Show t, Storable t) => Pile t -> Pile t -> IO (Pile t)
addPileToPile destination source =
  do let sourceCursor = _pileCursor source
         sourceSize = sourceCursor * sizeOf (undefined :: t)
         destCursor = _pileCursor destination
         destStart  = _pileData destination `plusPtr` (destCursor * sizeOf (undefined :: t))
         destAlloced = _pileSize destination
         end         = destCursor + sourceCursor
     if end > destAlloced
     then
       do e <- extendPile "pileToPile" destination
          addPileToPile e source
     else
       do copyBytes destStart (_pileData source) sourceSize
          return destination {_pileCursor = destCursor + sourceCursor}

addToPileState :: (Storable t, Show t, MonadState s m, Monad m) => (IO (Pile t, Reference t) -> m (Pile t, Reference t)) -> Lens' s (Pile t) -> t -> m (Reference t)
addToPileState lifter lens object =
  do pile <- use lens
     (pile', ref) <- lifter $ addToPile "addToPileState" pile object
     lens .= pile'
     return ref

addListToPileState :: (Storable t, Show t, MonadState s m, Monad m) => (IO (Pile t, Slice t) -> m (Pile t, Slice t)) -> Lens' s (Pile t) -> [t] -> m (Slice t)
addListToPileState lifter lens list =
  do pile <- use lens
     (pile', slice) <- lifter $ addListToPile pile list
     lens .= pile'
     return slice

addVectorToPile :: forall t . Storable t => Pile t -> VS.Vector t -> IO (Pile t, Reference t)
addVectorToPile pile@(Pile cursor size startPtr) vector =
  let vlen =  VS.length vector
      end = cursor + vlen
      vecSize = vlen * sizeOf (undefined :: t)
  in
  if end < size
  then
    do
      let cursorPtr = startPtr `plusPtr` (cursor * sizeOf (undefined :: t))
      VS.unsafeWith vector $ \ptr -> copyBytes cursorPtr ptr vecSize
      return $ (Pile end size startPtr, Ref $ fromIntegral cursor)
  else
    do e <- extendPile "addVectorToPile" pile
       addVectorToPile e vector

pileToList :: forall t . (Storable t, Show t) => Pile t -> IO [t]
pileToList (Pile cursor size startPtr) = peekArray cursor startPtr

listToPile :: forall t . (Storable t, Show t) => [t] -> IO (Pile t)
listToPile list = do pile <- newPile
                     (pile', _) <- addListToPile pile list
                     return pile'

getPileItem :: forall t . (Storable t) => Pile t -> Int -> IO t
getPileItem (Pile cursor size startPtr) index =
  let offset = startPtr `plusPtr` (index * sizeOf (undefined :: t))
  in peek offset

pileTop :: forall t . (Storable t) => Pile t -> IO t
pileTop pile@(Pile cursor _ startPtr) =
  getPileItem pile (cursor - 1)

replacePileTop :: forall t . (Show t, Storable t) => Pile t -> t -> IO (Pile t, Reference t)
replacePileTop (Pile cursor size startPtr) item = addToPile "replacePileTop" (Pile (cursor - 1) size startPtr) item

getPileSlice :: forall t . (Storable t) => Pile t -> Int -> Int -> IO [t]
getPileSlice pile start len = peekArray len (_pileData pile `plusPtr` start)

getPileSliceVector pile start len = VS.fromList <$> getPileSlice pile start len

newPile :: forall t a . (Storable t) => IO (Pile t)
newPile = newPileSize cHUNKSIZE

newPileSize :: forall t a . (Storable t) => Int -> IO (Pile t)
newPileSize size =
  do
    ptr <- mallocBytes (size * sizeOf (undefined :: t))
    return (Pile 0 size ptr)

freePile :: Pile a -> IO ()
freePile (Pile _ _ ptr) = free ptr

resetPile :: Pile t -> Pile t
resetPile (Pile _ size ptr) = Pile 0 size ptr

isEmptyPile :: Pile t -> Bool
isEmptyPile (Pile c _ _ ) = c == 0

instance (NFData t) => NFData (Pile t) where
  rnf (Pile a b _ ) = a `deepseq` b `deepseq` ()

type BytePile = Pile CChar

addToBytePile' :: forall t. (Storable t, Show t) => String -> BytePile -> t -> IO BytePile
addToBytePile' message pile@(Pile cursor size startPtr) item =
  let end = cursor + sizeOf item
  in
  if end < size
  then
    do --putStrLn $ "aboutToPoke" ++ show pile
       poke (startPtr `plusPtr` cursor) item
       return $ Pile end size startPtr
  else
    do
       e <- extendPile message pile
       addToBytePile' message e item

addToBytePile :: forall t. (Storable t, Show t) => String -> BytePile -> t -> IO (BytePile, Int)
addToBytePile message pile@(Pile cursor size startPtr) item =
    do --putStrLn $ "addToBytePile " ++ message ++ show item
       pile' <- addToBytePile' message pile item
       return (pile', cursor)

canAddToBytePile :: (Storable t) => BytePile -> Int -> t -> Bool
canAddToBytePile (Pile cursor _ _) limit item = cursor + sizeOf item < limit

addVectorToBytePile :: forall t . (Storable t, Show t) => BytePile -> VS.Vector t -> IO (BytePile, Reference CChar)
addVectorToBytePile pile@(Pile cursor size startPtr) vector =
  let vlen =  VS.length vector
      end = cursor + vlen
      vecSize = vlen * sizeOf (undefined :: t)
  in
  if end < size
  then do let cursorPtr = startPtr `plusPtr` (cursor * sizeOf (undefined :: t))
          VS.unsafeWith vector $ \ptr -> copyBytes cursorPtr ptr vecSize
          return $ (Pile end size startPtr, Ref $ fromIntegral cursor)
  else do e <- extendPile "addVectorToBytePile" pile
          addVectorToBytePile e vector

bytePileToCharList :: BytePile -> IO [CChar]
bytePileToCharList (Pile cursor size startPtr) = peekArray cursor startPtr

bytePileToIntList :: BytePile -> IO [CInt]
bytePileToIntList (Pile cursor size startPtr) = peekArray (cursor `div` sizeOf (undefined::CInt)) (castPtr startPtr :: Ptr CInt)

bytePileToShortList :: BytePile -> IO [String]
bytePileToShortList (Pile cursor size startPtr) =
  do
    ss <- peekArray (cursor `div` sizeOf (undefined::CShort)) (castPtr startPtr :: Ptr CShort)
    return $ zipWith (\x y -> show x ++ ":" ++ show y) (map (*2) [0..]) ss


section :: Int -> [a] -> [[a]]
section n []   = []
section n list = let (front, rest) = splitAt n list
                 in front:section n rest


bytePileToGeometry :: BytePile -> IO [String]
bytePileToGeometry (Pile cursor size startPtr) =
  do
    ss <- peekArray (cursor `div` sizeOf (undefined::CShort)) (castPtr startPtr :: Ptr CShort)
    let sList = map (concat . intersperse ":") . section 4 . map show $ ss
    sf <- peekArray (cursor `div` sizeOf (undefined::CFloat)) (castPtr startPtr :: Ptr CFloat)
    let fList = map (concat . intersperse ":") . section 2 . map (showFl' 6) $ sf
    return $ zipWith3 (\x y z -> show x ++ ": " ++ show y ++ "|" ++ show z) (map (*2) [0..]) fList sList


bytePileToFloatList :: Int -> BytePile -> IO [String]
bytePileToFloatList offset (Pile cursor size startPtr) =
  do ss <- peekArray (cursor `div` sizeOf (undefined::CFloat)) (castPtr (startPtr `plusPtr` offset) :: Ptr CFloat)
     return $ zipWith (\x y -> show x ++ ":" ++ showFl' 6 y) (map (*2) [0..]) ss
