{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE UndecidableInstances #-}

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

newtype Strand =  Strand {unStrand :: V.Vector (Point2 SubSpace)}

instance Show Strand where
  show = show . unStrand


-- | Compare the begining and end of a triple, horizontally. (Assumes its not a knob).
compareHorizontal :: Ord s => Bezier s -> Ordering
compareHorizontal (Bez a _ b) = compare (a ^. pX) (b ^. pX)
-- | Compare the begining and end of a triple, vertically.
compareVertical :: Ord s => Bezier s -> Ordering
compareVertical   (Bez a _ b) = compare (a ^. pY) (b ^. pY)

-- | Determine if two adjacent beziers will be part of the same strand.
-- If both have the same horizontal order but they are not vertical then combine them.
-- Vertical segments are never combined even if they are adjacent.
connectable :: Ord s => Bezier s -> Bezier s -> Bool
connectable a b =
  let hA = compareHorizontal a
      hB = compareHorizontal b
  in  (hA == hB) && hA /= EQ

-- | Accumulate strands if the are connectable.
accumulateStrands :: Ord s
                  => (V.Vector (Bezier s), V.Vector (V.Vector (Bezier s)))
                  -> Bezier s
                  -> (V.Vector (Bezier s), V.Vector (V.Vector (Bezier s)))
accumulateStrands (acc, strands) triple =
  if V.null acc
  then (V.singleton triple, strands)
  else if connectable (V.last acc) triple
       then (acc `V.snoc` triple, strands)
       else (V.singleton triple, strands `V.snoc` acc)

-- | Assuming all horizontally bulging curves (knobs) have been removed.
-- Divide a loop of curve sections into strands of horizontally adjacent curve sections that go
-- in the same direction.
splitIntoStrands :: Ord s
                 => V.Vector (Bezier s) -> V.Vector (V.Vector (Bezier s))
splitIntoStrands vector =
  let (acc, strands) = V.foldl accumulateStrands (V.empty, V.empty) vector
  in acc `V.cons` strands

-- | Split a vector into a list over vectors with a maximum size.
splitTooLarge :: Int -> V.Vector a -> V.Vector (V.Vector a)
splitTooLarge maxSize vector = if V.length vector > maxSize
                               then let (first, rest) = V.splitAt maxSize vector
                                    in first `V.cons` splitTooLarge maxSize rest
                               else V.singleton vector

-- Make a curve section from two adjacent curve pairs.
makeBezier :: CurvePair s -> CurvePair s -> Bezier s
makeBezier a b = Bez (a ^. onCurve) (a ^. offCurve) (b ^. onCurve)

-- | Reverse the direction of a curve section.
reverseBezier :: Bezier s -> Bezier s
reverseBezier (Bez a b c) = Bez c b a

-- | Zip each element with its next neighbor or the first element.
overNeighbors :: (a -> a -> b) -> V.Vector a -> V.Vector b
overNeighbors f vector =
  let rotated = (V.++) (V.drop 1 vector) (V.take 1 vector)
  in  V.zipWith f vector rotated

-- | Turn a sequence of curve pairs into a sequence of curve sections (called beziers)
pairsToBeziers :: V.Vector (CurvePair s) -> V.Vector (Bezier s)
pairsToBeziers  = overNeighbors makeBezier

-- | Turn a sequence of beziers into a sequence of points
beziersToPoints :: V.Vector (Bezier s) -> V.Vector (Point2 s)
beziersToPoints vector =
  let lastPoint = unBezier (V.last vector) ^. _z
      lastTwo triple = V.singleton (unBezier triple ^. _x) `V.snoc` (unBezier triple ^. _y)
  in  V.concatMap lastTwo vector `V.snoc` lastPoint

-- | Reverse strands if neccessary so that they go from left to right.
reverseIfBackwards :: forall s . Ord s => V.Vector (Bezier s) -> V.Vector (Bezier s)
reverseIfBackwards vector =
    let startPoint = unBezier (V.head vector) ^. _x
        endPoint   = unBezier (V.last vector) ^. _z
    in  if (startPoint ^. pX) > (endPoint ^. pX)
        then V.reverse . V.map reverseBezier $ vector
        else vector

-- | Reorder a vector into a horizontally searchable tree based on a predermined lookup table
-- of tree shapes.
reorder :: Storable a => ReorderTable -> V.Vector a -> V.Vector a
reorder table vector =
  let len = V.length vector
  in  V.generate len ((V.!) vector . fromReorderTable table len)

-- | Split an outline into strands.
splitShape :: ReorderTable
           -> Int
           -> V.Vector (CurvePair SubSpace)
           -> V.Vector Strand
splitShape table maxSectionSize curvePairs =
    -- read this composition from bottom to top
    V.map ( Strand
          . reorder table      -- use a lookup take to turn the strand into a horizontally searchable tree of points
          . beziersToPoints    -- remove additional points making each strand into a list of points.
          . reverseIfBackwards -- make each strands go in order from left to right.
          ) .
    V.concatMap (splitTooLarge maxSectionSize) . -- Divide any long strands into smaller sections.
    splitIntoStrands . -- Divide curves into horizontal strands.
    replaceKnobs .   -- Split curve sections that bulge in the x direction to two curve sections that do not bulge.
    pairsToBeziers $ -- convert each adjacent point pair to a curve section.
    curvePairs

-- | Build a list of strands from an outline.
outlineToStrands :: ReorderTable
                 -> Int
                 -> Outline SubSpace
                 -> V.Vector Strand
outlineToStrands table sectionSize (Outline ps) =
    if length ps < 2
    then V.empty
    else splitShape table (sectionSize `div` 2) ps

--  SubSpace (Floating point) Strands are written to memory so that the each point aligns to 64 bit boundaries in GPU memory.
--  The order is designed to facilitate as many 64bit memory accesses as possible and determine the horizontal range of the curve with the minumum loads
--  Because of this there are a lot of unused bits in the first 64 bits.
--  The Size field refers to the number of 64 bits chunks that the loading function must skip ahead to get to the next tree so it includes the header.
--  In memory the format the header should appear this way:
--  | 32 bit             | 32 bit    | 64 bit            | 64 bit          | 64 bit                | 64 bit            | ...
--  | 16 bit   | 16 bit  |           | 32 bit  | 32 bit  | 32 bit | 32bit  | 32 bit    | 32bit     | 32 bit  | 32 bit  | 32 bit          | 32bit           | ...
--  | size + 1 | unused  | unused    | right.x | right.y | left.x | left.y | control.x | control.y | tree0.x | tree0.y | tree0 control.x | tree0 control.y | ...
--  | in 64bit pieces    |           | ending curve point| leftmost curve point and control        | complete binary tree of point control point pairs...

instance StorableM Strand where
  sizeOfM (Strand vector) =
    do sizeOfM (undefined::CUShort) -- size
       sizeOfM (undefined::CUShort) -- unused
       sizeOfM (undefined::CUInt  ) -- unused
       sizeOfM vector
  alignmentM _  = do alignmentM (undefined::CUShort) -- size
                     alignmentM (undefined::CUShort) -- unused
                     alignmentM (undefined::CUInt  ) -- unused
                     --alignmentM range
  peekM = error "no peek for Strand."
  pokeM strand@(Strand vector) =
    do let size = (fromIntegral $ sizeOfV strand) `div` 8 :: CUShort
       pokeM size         -- size
       pokeM (0::CUShort) -- empty
       pokeM (0::CUInt)   -- empty
       pokeM vector

instance Storable a => StorableM (V.Vector a) where
  sizeOfM vector = do modify (+(V.length vector * sizeOf (undefined :: a)))
                      return ()
  alignmentM _   = do alignmentM (undefined :: a)
  peekM          = error "peek not implemented for Range"
  pokeM vector   = numLoop 0 (V.length vector - 1) (\i -> pokeM ((V.!) vector i))


instance Storable Strand where
  sizeOf    = sizeOfV
  alignment = alignmentV
  peek      = peekV
  poke      = pokeV

instance NFData Strand where
  rnf strand = ()
