{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PatternSynonyms     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Strand
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Dividing outlines into strands and encoding them to be parsed by
-- the raster kernel.

module Graphics.Gudni.Raster.Strand
  ( Strand(..)
  , outlineToStrands
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Outline

import Graphics.Gudni.Raster.ReorderTable
import Graphics.Gudni.Raster.Deknob

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM

import Foreign.Marshal.Array
import Foreign.Marshal.Utils(copyBytes)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (sortBy)

import Control.DeepSeq
import Control.Lens hiding (Index)
import Control.Loop
import Control.Monad.State
import Linear

-- |  A strand is a horizontal section of an outline encoded into a special tree that can be quickly parsed by a thread of the rasterizer∘
-- The strand represents a series of onCurve and offCurve points and a terminating on Curve point∘
-- In most cases the x value of an individual point must be less than the x value of the following point with the exception of the first and last
-- points. These two points can be equal to the points before and after them. This allows the rasterizer to handle segments that are exactly vertical gracefully.
-- Strands are in a special order to maximize the rasterizer. They start with the furthest right (terminating point) of the sequence followed by the furthest left opening
-- point of the sequence and it's control point. This allows the raster thread to quickly look at the horizontal range of the strand. These are followed by the middle points
-- of the sequence ordered as a complete binary tree.

newtype Strand =  Strand {unStrand :: Range (Point2 SubSpace)}
instance Show Strand where
  show = show . unStrand

type Length = Int
type Index = Int

-- | Get the first element of the range.
rStart :: Range a -> a
rStart (Range f len) = f 0

-- | Get the last element of the range.
rEnd :: Range a -> a
rEnd   (Range f len) = f (len - 1)

-- | Get the ith element of the range
peekRange :: Range a -> Index -> a
peekRange (Range f len) i = f i

-- | Get the length of the range.
rLength :: Range a -> Length
rLength (Range f len) = len

-- | Create a new range that covers and entire vector.
vectorToRange :: V.Vector a -> Range a
vectorToRange vs = Range ((V.!) vs) (V.length vs)

composeRange :: (Int -> Int) -> Range a -> Range a
composeRange g (Range f len) = Range (f . g) len

(+>) :: Range a -> Int -> Range a
r +> x = composeRange (+ x) r

-- | Combine two ranges.
combineRanges :: Range a -> Range a -> Range a
combineRanges (Range a lenA) (Range b lenB) = Range a (lenA + lenB)

-- | Compare the begining and end of a triple, horizontally. (Assumes its not a knob).
compareHorizontal :: Ord s => Triple s -> Ordering
compareHorizontal (V3 a _ b) = compare (a ^. pX) (b ^. pX)
-- | Compare the begining and end of a triple, vertically.
compareVertical :: Ord s => Triple s -> Ordering
compareVertical   (V3 a _ b) = compare (a ^. pY) (b ^. pY)

-- | Determine if two adjacent triples will be part of the same strand.
-- if both have the same horizontal order but they are not vertical then combine them.
-- Vertical segments are never combined even if they are adjacent.
connectable :: Ord s => Triple s -> Triple s -> Bool
connectable a b =
  let hA = compareHorizontal a
      hB = compareHorizontal b
  in  (hA == hB) && hA /= EQ

splitRanges  :: forall s . (Show s, Storable s, Ord s) => Int -> Range (Triple s) -> [Range (Triple s)]
splitRanges maxLength (Range f rangeLength) = go 0 0
  where
  go :: Index -> Index -> [Range (Triple s)]
  go start cursor =
      let current = f cursor
          next    = f (cursor + 1)
          len     = (cursor - start) + 1
          range   = Range (f . (+ start)) len
      in  if cursor < rangeLength - 1
          then if connectable current next && len < maxLength
               then go start (cursor + 1)
               else range : go (cursor + 1) (cursor + 1)
          else [range]

pairsToTriples :: Range (CurvePair s) -> Range (Triple s)
pairsToTriples (Range f len) =
  let g i = V3 (f i ^. onCurve) (f i ^. offCurve) end
        where end = if i < len - 1
                    then f (i+1) ^. onCurve
                    else f 0     ^. onCurve
  in  Range g len

unTriple :: Show s => Range (Triple s) -> Range (Point2 s)
unTriple (Range triples len) =
  let div2     i = i `div` 2
      part     i = if even i then view _x else view _y
      newlen     = len * 2 + 1
      dec      x = x -1
      getPoint i = if i == newlen - 1
                   then view _z . triples . div2 . dec $ i
                   else part  i . triples . div2       $ i
  in  Range getPoint newlen

reverseIndex :: Length -> Index -> Index
reverseIndex len i = (len - 1) - i

reverseTriple :: Triple s -> Triple s
reverseTriple (V3 a b c) = V3 c b a

reverseRange :: Range (Triple s) -> Range (Triple s)
reverseRange (Range f len) = Range (reverseTriple . f . reverseIndex len) len

flipIfBackwards :: Ord s => Range (Triple s) -> Range (Triple s)
flipIfBackwards range =
    let hDirection = compare (rStart range ^. _x . pX) (rEnd range ^. _z . pX)
        direction = if hDirection /= EQ
                    then hDirection
                    else compare (rStart range ^. _x . pY) (rEnd range ^. _z . pY)
    in  if hDirection == GT
        then reverseRange range
        else range

-- | Split an outline into strands.
splitShape :: ReorderTable
           -> Int
           -> [CurvePair SubSpace]
           -> [Strand]
splitShape table maxSectionSize curvePairs =
    let outlineRange =  replaceKnobs .
                        pairsToTriples .
                        vectorToRange .
                        V.fromList $
                        curvePairs
        ranges = splitRanges maxSectionSize outlineRange
        reorderedRanges = map
                       ( reorder
                       . unTriple
                       . flipIfBackwards
                       ) ranges
        reorder range = Strand
                        -- . tc "strand "
                        . composeRange (fromReorderTable table (rLength range))
                        -- . tr "untripled "
                        $ range
    in  reorderedRanges

-- | Build a list of strands from an outline.
outlineToStrands :: ReorderTable
                 -> Int
                 -> Outline SubSpace
                 -> [Strand]
outlineToStrands table sectionSize (Outline ps) =
    if length ps < 2
    then []
    else splitShape table (sectionSize `div` 2) ps

--  SubSpace (Floating point) Strands are written to memory so that the each point aligns to 64 bit boundaries in GPU memory.
--- The order is designed to facilitate as many 64bit memory accesses as possible and determine the horizontal range of the curve with the minumum loads
--- Because of this there are a lot of unused bits in the first 64 bits.
--  The Size field refers to the number of 64 bits chunks that the loading function must skip ahead to get to the next tree so it includes the header.
--  In memory the format the header should appear this way:
--  | 32 bit             | 32 bit    | 64 bit            | 64 bit          | 64 bit                | 64 bit            | ...
--  | 16 bit   | 16 bit  |           | 32 bit  | 32 bit  | 32 bit | 32bit  | 32 bit    | 32bit     | 32 bit  | 32 bit  | 32 bit          | 32bit           | ...
--  | size + 1 | unused  | unused    | right.x | right.y | left.x | left.y | control.x | control.y | tree0.x | tree0.y | tree0 control.x | tree0 control.y | ...
--  | in 64bit pieces    |           | ending curve point| leftmost curve point and control        | complete binary tree of point control point pairs...

instance StorableM Strand where
  sizeOfM (Strand range) = do sizeOfM (undefined::CUShort) -- size
                              sizeOfM (undefined::CUShort) -- unused
                              sizeOfM (undefined::CUInt  ) -- unused
                              sizeOfM range
  alignmentM _  = do alignmentM (undefined::CUShort) -- size
                     alignmentM (undefined::CUShort) -- unused
                     alignmentM (undefined::CUInt  ) -- unused
                     --alignmentM range
  peekM = error "no peek for Strand."
  pokeM strand@(Strand range) =
    do let size = (fromIntegral $ sizeOfV strand) `div` 8 :: CUShort
       pokeM size         -- size
       pokeM (0::CUShort) -- empty
       pokeM (0::CUInt)   -- empty
       pokeM range

instance Storable a => StorableM (Range a) where
  sizeOfM (Range _ len) = do modify (+(len * sizeOf (undefined :: a)))
                             return ()
  alignmentM _          = do alignmentM (undefined :: a)
  peekM                 = error "peek not implemented for Range"
  pokeM (Range f len)   = numLoop 0 (len-1) (\i -> pokeM (f i))

instance NFData Strand where
  rnf strand = ()
