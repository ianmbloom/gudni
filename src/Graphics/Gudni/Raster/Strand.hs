{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

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
  ( PointVector(..)
  , Strand(..)
  , outlineToStrands
  , splitRightKnob
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Outline

import Graphics.Gudni.Raster.StrandLookupTable

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM

import Foreign.Marshal.Array
import Foreign.Marshal.Utils(copyBytes)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import qualified Data.Vector.Storable as VS
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (sortBy)

import Control.DeepSeq
import Control.Lens
import Linear.V3

-- | Just a newtype wrapper for a vector of points.
newtype PointVector s = PVect (VS.Vector (Point2 s)) deriving (Eq, Show)

instance NFData s => NFData (PointVector s) where
  rnf (PVect t) = rnf t

instance Storable (PointVector SubSpace) where
  sizeOf (PVect vs) = VS.length vs * sizeOf (undefined::Point2 SubSpace)
  alignment _ = alignment (undefined::SubSpace)
  peek ptr = error "no peek for pointVectors"
  poke ptr pVect@(PVect vs) = VS.unsafeWith vs $ \ sourcePtr -> copyBytes (castPtr ptr) sourcePtr (sizeOf pVect)

-- |  A strand is a horizontal section of an outline encoded into a special tree that can be quickly parsed by a thread of the rasterizer∘
-- The strand represents a series of onCurve and offCurve points and a terminating on Curve point∘
-- In most cases the x value of an individual point must be less than the x value of the following point with the exception of the first and last
-- points. These two points can be equal to the points before and after them. This allows the rasterizer to handle segments that are exactly vertical gracefully.
-- Strands are in a special order to maximize the rasterizer. They start with the furthest right (terminating point) of the sequence followed by the furthest left opening
-- point of the sequence and it's control point. This allows the raster thread to quickly look at the horizontal range of the strand. These are followed by the middle points
-- of the sequence ordered as a complete binary tree.

newtype Strand = Strand {unStrand :: PointVector SubSpace} deriving (Show)
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
  sizeOfM (Strand vector) = do sizeOfM (undefined::CUShort) -- size
                               sizeOfM (undefined::CUShort) -- unused
                               sizeOfM (undefined::CUInt  ) -- unused
                               sizeOfM vector
  alignmentM _  = do alignmentM (undefined::CUShort) -- size
                     alignmentM (undefined::CUShort) -- unused
                     alignmentM (undefined::CUInt  ) -- unused
                     --alignmentM vector
  peekM = error "no peek for PointVector"
  pokeM strand@(Strand vector) =
    do let size = (fromIntegral $ sizeOfV strand) `div` 8 :: CUShort
       pokeM size         -- size
       pokeM (0::CUShort) -- empty
       pokeM (0::CUInt)   -- empty
       pokeM vector

instance NFData Strand where
  rnf = rnf . unStrand
-- * Converion from Outlines to Strands.

-------------------- Clean Shape -----------------------

type Triple p = V3 p

triples vs = triples' (view onCurve . head $ vs) vs

triples' :: Point2 s -> [CurvePair s] -> [Triple (Point2 s)]
triples' h (v0:v1:vs) = V3 (v0 ^. onCurve) (v0 ^. offCurve) (v1 ^. onCurve):triples' h (v1:vs)
triples' h (v0:[]) = [V3 (v0 ^. onCurve) (v0 ^. offCurve) h]
triples' h [] = error "triples encountered end of list"

-- * Remove Knobs
-- Knobs are convex curves on a shapes which is a sequence of an on curve point
-- a control point and another on curve point where the x component of the control point is either
-- less than or greater than both on curve points. In other words the curve bulges out in the x direction∘
-- It must be split into two curves by finding the on curve point that is as close as possible to the verticle tangent line.

-- | Constant for bifercating exactly in half.
sPLIT :: (Fractional s, Num s) => s
sPLIT = 1 / 2

-- | Find the point along the curve parameterized by t.
midPoint :: Num s => s -> Point2 s -> Point2 s -> Point2 s
midPoint t v0 v1 = (v0 ^* (1-t)) ^+^ (v1 ^* t)

-- | Given two onCurve points and a controlPoint. Find two control points and a midway on-curve point.
curvePoint :: Num s => s -> Point2 s -> Point2 s -> Point2 s -> Triple (Point2 s)
curvePoint t left control right =
  let leftMid  = midPoint t left    control
      rightMid = midPoint t control right
      onCurve  = midPoint t leftMid rightMid
  in  (V3 leftMid onCurve rightMid)

-- | Return true if a curve and its control point would be convex in the positive horizontal direction.
leftConvex  :: (Show s, Num s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Bool
leftConvex control onCurve  = control ^. pX < onCurve ^. pX

-- | Return true if a curve and its control point would be convex in the negative horizontal direction.
rightConvex :: (Show s, Num s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Bool
rightConvex control onCurve = onCurve ^. pX < control ^. pX

-- | Find an onCurve point and two new control points to can horizonally divide a curve that
-- is convex to the right (like a right bracket ")")
splitRightKnob ::(Show s, Fractional s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Point2 s -> Triple (Point2 s)
splitRightKnob = splitKnob rightConvex sPLIT -- start halfway

-- | Find an onCurve point and two new control points that horizonally divide a curve that
-- is convex to the left (like a left bracket "(")
splitLeftKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Point2 s -> Triple (Point2 s)
splitLeftKnob = splitKnob leftConvex sPLIT -- start halfway

-- | Find a new onCurve point and two new control points that divide the curve based on the convex function.
splitKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => (Point2 s -> Point2 s -> Bool) -> s -> Point2 s -> Point2 s -> Point2 s -> Triple (Point2 s)
splitKnob = splitKnob' 0.0 1.0

-- | Given a range of parameters along the curve determine if the are close enough to split the curve.
splitKnob' :: (Show s, Fractional s, Ord s, Num s, Iota s) => s -> s -> (Point2 s -> Point2 s -> Bool) -> s -> Point2 s -> Point2 s -> Point2 s -> Triple (Point2 s)
splitKnob' bottom top convex t v0 control v1
  -- So if the top and bottom parameters are close enough, return the points to divide the curve.
  | top - bottom <= iota = V3 leftMid onCurve rightMid
  -- Otherwise if the leftMid control point is still convex move the paramters to toward the top parameter and split again.
  | convex rightMid onCurve =
        splitKnob' t top convex (t + ((top - t) / 2)) v0 control v1 -- search closer to v0
  -- Otherwise if the rightMid control point is still convex move the paramters to toward the bottom parameter and split again.
  | convex leftMid  onCurve = -- if the rightMid control point is still convex.
        splitKnob' bottom t convex (bottom + ((t - bottom) / 2)) v0 control v1 -- search closer to v1
  -- Otherwise it's not convex anymore so split it
  | otherwise = V3 leftMid onCurve rightMid
  where (V3 leftMid onCurve rightMid) = curvePoint t v0 control v1

-- | If a curve is a knob, split it.
checkKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => Triple (Point2 s) -> [Triple (Point2 s)]
checkKnob (V3 a b c) =
    -- If both sides are convex in the left direction.
    if leftConvex b a && leftConvex b c
    then
      -- Split the knob based on it being left convex.
      let (V3 leftMid onCurve rightMid) = splitLeftKnob a b c
      -- And return the two resulting curves.
      in [V3 a leftMid onCurve, V3 onCurve rightMid c]
    else
      -- Else if both sides are convex in the right direction
      if rightConvex b a && rightConvex b c
      then
        -- Split the know based on it being right convex.
        let (V3 leftMid  onCurve  rightMid) = splitRightKnob a b c
        -- And return the two resulting curves.
        in [V3 a leftMid onCurve, V3 onCurve rightMid c]
      else
        -- Otherwise return the curve unharmed.
        [V3 a b c]

-- | A larger close-enough value.
smidge :: (Num s, Iota s) => s
smidge = iota * 100

-- | Return a horizontal order for two points
compareHorizontal :: Ord s => Point2 s -> Point2 s -> Ordering
compareHorizontal a b = compare (a ^. pX) (b ^. pX)

-- | Label a curve (that is not horizontally convex with a horizontal direction.
direction :: Ord s => Triple (Point2 s) -> (Ordering, [Triple (Point2 s)])
direction (V3 a control b) = (compareHorizontal a b, [V3 a control b])

-- | Combine labeled curves that have the same horizontal order.
groupDirections :: [(Ordering , [a])] -> [(Ordering, [a])]
groupDirections ((ac, as):(bc, bs):ss)= if ac == bc
                                        then groupDirections ((ac, as ++ bs):ss)
                                        else (ac, as):groupDirections((bc, bs):ss)
groupDirections ss                    = ss

-- | If a direction group spans the loop of the curve move it from the end to the begining of the sequence
connectLoop :: Show a => [(Ordering, [a])] -> [(Ordering, [a])]
connectLoop ss = let ((ac, as), mid, (bc, bs)) = takeFirstLast ss
                 in  if ac == bc && ac /= EQ
                     then (ac, bs ++ as):mid
                     else ss

-- | Remove intermedate parts of a sequence of vertical curves to make one long vertical curve.
trimEQ :: Show p => (Ordering, [Triple p]) -> (Ordering, [Triple p])
trimEQ (EQ, [a]) = (EQ, [a])
trimEQ (EQ, ss ) = let (V3 a b _ ,_, V3 _ _ c) = takeFirstLast ss
                   in (EQ,[V3 a b c])
trimEQ x         = x

-- | If a direction group that has a direction is surrounded by vertical groups attach them like wings.
attachEQ :: [(Ordering, [Triple p])] -> [(Maybe (Triple p),(Ordering,[Triple p]), Maybe (Triple p))]
attachEQ ((EQ,[a]):x:y@(EQ,[b]):xs) = (Just a , x, Just b ):attachEQ(y:xs)
attachEQ ((EQ,[a]):x:           xs) = (Just a , x, Nothing):attachEQ   xs
attachEQ ((EQ,_  ):             []) = []
attachEQ (         x:y@(EQ,[b]):xs) = (Nothing, x, Just b ):attachEQ(y:xs)
attachEQ (         x:           xs) = (Nothing, x, Nothing):attachEQ   xs
attachEQ []                         = []

-- | Get the vertical value from a curve segment.
vertical :: (Num s, Ord s, Iota s) => Maybe (Triple (Point2 s)) -> Ortho YDimension s
vertical (Just (V3 a _ b)) = a ^. pY - b ^. pY
vertical _                 = 0


-- Reverse a curve section.
flipTriple :: Triple a -> Triple a
flipTriple (V3 a x b) = V3 b x a

concatSimples' :: [V3 p] -> (p, [(p, p)])
concatSimples' (V3 a b c:[]) = (c, [(a, b)])
concatSimples' (V3 a b c:xs) = let (right, rest) = concatSimples' xs
                                in  (right, (a,b):rest)
concatSimples' [] = error "concatSimples' encountered empty list"

concatSimples :: [V3 p] -> (V3 p, [(p, p)])
concatSimples (V3 left control right:[]) = (V3 right left control, [])
concatSimples (V3 left control _    :xs) = let (right, rest) = concatSimples' xs
                                            in  (V3 right left control, rest)
concatSimples [] = error "concatSimples encountered empty list"

buildStrand' :: (Num s, Ord s, Iota s) => Maybe (Triple (Point2 s)) -> [Triple (Point2 s)] -> Maybe (Triple (Point2 s)) -> [Triple (Point2 s)]
buildStrand' start xs end =
  let s =  if vertical start < 0
           then  -- X -----> X
                 -- ↑
                 -- X
                 start
           else  -- X
                 -- ↓
                 -- X -----> X
                 Nothing
      e =  if vertical end > 0
           then  -- X -----> X
                 --          ↓
                 --          X
                 end
           else  --          X
                 --          ↑
                 -- X <----- X
                 Nothing
   in maybeToList s ++ xs ++ maybeToList e

-- | Build a strand from a directional section and possible flanking vertical sections
buildStrand :: (Num s, Ord s, Iota s) => (Maybe (Triple (Point2 s)), (Ordering, [Triple (Point2 s)]), Maybe (Triple (Point2 s))) -> [Triple (Point2 s)]
buildStrand (start, (LT, xs), end) = buildStrand' start                                            xs                  end
buildStrand (start, (GT, xs), end) = buildStrand' (flipTriple <$> end) (reverse . map flipTriple $ xs) (flipTriple <$> start)
buildStrand (start, (EQ, xs), end) = error "buildStrand'' encountered EQ section."

-- | Split Strands that are longer than the sectionsize into parts.
splitLong :: (Show p) => Int -> [p] -> [[p]]
splitLong sectionSize section = let triplesPerSection = sectionSize `div` 2
                                in  splitLong' triplesPerSection section

splitLong' :: (Show p) => Int -> [p] -> [[p]]
splitLong' triplesPerSection (triples) = let (part, rest) = splitAt triplesPerSection triples
                                         in  if length triples > triplesPerSection
                                             then part:splitLong' triplesPerSection rest
                                             else [triples]

-- | If a direction group spans the loop of the curve move it from the end to the begining of the sequence
wrapEQ :: (Show p) => [(Ordering, [Triple p])] -> [(Ordering, [Triple p])]
wrapEQ xs = let (f, mid, l) = takeFirstLast xs
            in  case f of
                (EQ, _) -> xs ++ [f]
                _       -> case l of
                             (EQ, _) -> l:xs
                             _       -> xs

-- | Build strands from a series of order labeled sections.
buildFromDirections :: CurveTable
                    -> Int
                    -> [(Ordering, [Triple (Point2 SubSpace)])]
                    -> [Strand]
buildFromDirections table sectionSize ss =
  if length ss > 1
  then
        map (Strand . PVect .
             makeCurveStrand table .
             concatSimples) .
        concatMap (splitLong sectionSize) .
        --tr "buildStrand" .
        map buildStrand .
        --tr "attachEQ" .
        attachEQ .
        --tr "trimEQ" .
        map trimEQ .
        --tr "connectLoop" .
        connectLoop $ ss
  else []

-- | Split an outline into strands.
splitShape :: CurveTable
           -> Int
           -> [CurvePair SubSpace]
           -> [Strand]
splitShape table sectionSize =
    buildFromDirections table sectionSize .
    --tr "groupDirections" .
    groupDirections .
    --tr "wrapEQ" .
    wrapEQ .
    --tr "direction" .
    map direction .
    concatMap checkKnob .
    --tr "triples" .
    triples
    -- . tr "vertices"

-- | Build a list of strands from an outline.
outlineToStrands :: CurveTable
                 -> Int
                 -> Outline SubSpace
                 -> [Strand]
outlineToStrands table sectionSize (Outline ps) =
    if length ps < 2
    then []
    else splitShape table sectionSize ps
