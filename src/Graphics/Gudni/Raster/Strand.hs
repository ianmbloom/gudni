{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

module Graphics.Gudni.Raster.Strand
  ( PointVector(..)
  , Strand(..)
  , shapeToStrands
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

----------------- Point Vector -----------------
-- Just a wrapper around vectors to override instances
newtype PointVector s = PVect (VS.Vector (Point2 s)) deriving (Eq, Show)

instance NFData s => NFData (PointVector s) where
  rnf (PVect t) = rnf t

instance Storable (PointVector DisplaySpace) where
  sizeOf (PVect vs) = VS.length vs * sizeOf (undefined::Point2 DisplaySpace)
  alignment _ = alignment (undefined::DisplaySpace)
  peek ptr = error "no peek for pointVectors"
  poke ptr pVect@(PVect vs) = VS.unsafeWith vs $ \ sourcePtr -> copyBytes (castPtr ptr) sourcePtr (sizeOf pVect)

------------------- Strand -----------------
-- A strand is each horizontal component of a shape ready to be rasterized.

-- Strands are in a special order to maximize the rasterizer. They basically start with the endpoints of the horizontal section and then
-- the are followed by a complete binary tree that contains all of the internal curve points.

newtype Strand = Strand {unStrand :: PointVector DisplaySpace} deriving (Show)

--  DisplaySpace Strands are written to memory so that the each point aligns to 64 bit boundaries in GPU memory.
--- The order is designed to facilitate as many 64bit memory accesses as possible and determine the horizontal range of the curve with the minumum loads
--- Because of this there are a lot of unused bits in the first 64 bits.
--  Size refers to the number of 64 bits chunks that the loading function must skip ahead to get to the next tree so it includes the header.
-- In memory the format the header should appear this way:
-- | 32 bit             | 32 bit    | 64 bit            | 64 bit          | 64 bit                | 64 bit            | ...
-- | 16 bit   | 16 bit  |           | 32 bit  | 32 bit  | 32 bit | 32bit  | 32 bit    | 32bit     | 32 bit  | 32 bit  | 32 bit          | 32bit           | ...
-- | size + 1 | unused  | unused    | right.x | right.y | left.x | left.y | control.x | control.y | tree0.x | tree0.y | tree0 control.x | tree0 control.y | ...
-- | in 64bit pieces    |           | ending curve point| leftmost curve point                    | complete binary tree of point control point pairs     | rest of tree

instance StorableM Strand where
  sizeOfM (Strand vector) = do sizeOfM (undefined::CUShort) -- size
                               sizeOfM (undefined::CUShort ) -- continuation
                               sizeOfM (undefined::CUInt  ) -- empty
                               sizeOfM vector
  alignmentM _  = do alignmentM (undefined::Point2 DisplaySpace) -- size
  peekM = error "no peek for PointVector"
  pokeM strand@(Strand vector) =
    do let size = (fromIntegral $ sizeOfV strand) `div` 8 :: CUShort
       pokeM size         -- size
       pokeM (0::CUShort) -- empty
       pokeM (0::CUInt)   -- empty
       pokeM vector

instance NFData Strand where
  rnf = rnf . unStrand
------------------ Converion from shapes to strands ---------------------------

type Triple p = (p, p, p)

rotateIfFirstControl :: Show p => [Vertex p] -> [Vertex p]
rotateIfFirstControl (v:vs) = if v ^. isControl then vs ++ [v] else v:vs
rotateIfFirstControl []     = []

-------------------- Clean Shape -----------------------

triples vs = triples' (view onCurve . head $ vs) vs

triples' :: Point2 s -> [CurvePair (Point2 s)] -> [Triple (Point2 s)]
triples' h (v0:v1:vs) = (v0 ^. onCurve, v0 ^. offCurve, v1 ^. onCurve):triples' h (v1:vs)
triples' h (v0:[]) = [(v0 ^. onCurve, v0 ^. offCurve, h)]
triples' h [] = error "triples encountered end of list"

-------------------- Remove Knobs ---------------------------
-- Remove knobs, which are convex curves on a shapes which is a sequence of an on curve point
-- a control point and another on curve point where the x component of the control point is either
-- less than or greater than both on curve points.
-- In this case we find an on curve point that is as close as possible to the verticle tangent line
-- so that each curve segment sent to the GPU is composed only of horizontal curve sections.

sPLIT :: (Fractional s, Num s) => s
sPLIT = 1 / 2

midPoint :: Num s => s -> Point2 s -> Point2 s -> Point2 s
midPoint t v0 v1 = (v0 ^* (1-t)) ^+^ (v1 ^* t)

curvePoint :: Num s => s -> Point2 s -> Point2 s -> Point2 s -> (Point2 s, Point2 s, Point2 s)
curvePoint t left control right =
  let leftMid  = midPoint t left    control
      rightMid = midPoint t control right
      onCurve  = midPoint t leftMid rightMid
  in  (leftMid, onCurve, rightMid)

leftConvex  :: (Show s, Num s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Bool
leftConvex control onCurve  = control ^. pX < onCurve ^. pX
                              --let diff = pX control - pX onCurve
                              --in  --tr ("leftConvex control: " ++ show (vX control) ++ "vX onCurve " ++ show (vX onCurve) ++ "diff: " ++ show diff) $
                              --    diff > iota

rightConvex :: (Show s, Num s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Bool
rightConvex control onCurve = onCurve ^. pX < control ^. pX
                              --let diff = pX onCurve - pX control
                              --in --tr ("rightConvex control: " ++ show (vX control) ++ "vX onCurve " ++ show (vX onCurve) ++ "diff: " ++ show diff) $
                              --    diff > iota

-- find an onCurve point and new control points to horizonally divide a curve that
-- if convex to the right like a right bracket ")"
splitRightKnob ::(Show s, Fractional s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Point2 s -> (Point2 s, Point2 s, Point2 s)
splitRightKnob = splitKnob rightConvex sPLIT -- start halfway

-- find an onCurve point and new control points to horizonally divide a curve that
-- is convex to the left of right.
splitLeftKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => Point2 s -> Point2 s -> Point2 s -> (Point2 s, Point2 s, Point2 s)
splitLeftKnob = splitKnob leftConvex sPLIT -- start halfway

splitKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => (Point2 s -> Point2 s -> Bool) -> s -> Point2 s -> Point2 s -> Point2 s -> (Point2 s, Point2 s, Point2 s)
splitKnob = splitKnob' 0.0 1.0


splitKnob' :: (Show s, Fractional s, Ord s, Num s, Iota s) => s -> s -> (Point2 s -> Point2 s -> Bool) -> s -> Point2 s -> Point2 s -> Point2 s -> (Point2 s, Point2 s, Point2 s)
splitKnob' bottom top convex t v0 control v1
  | top - bottom <= iota = (leftMid, onCurve, rightMid)
  | convex rightMid onCurve = -- if the leftMid control point is still convex.
        --tr ("if the leftMid  control point is still convex. " ++ show (top - bottom) ++ "bottom: " ++ show bottom ++ " top: " ++ show top ++ " t:" ++ show t ++ " leftMid: " ++ show leftMid ++ " onCurve: " ++ show onCurve ++ " rightMid: " ++ show rightMid) $
        splitKnob' t top convex (t + ((top - t) / 2)) v0 control v1 -- search closer to v0
  | convex leftMid  onCurve = -- if the rightMid control point is still convex.
        --tr ("if the rightMid control point is still convex. " ++ show (top - bottom) ++ "bottom:" ++ show bottom ++ " top: " ++ show top ++ " t:" ++ show t ++ " leftMid: " ++ show leftMid ++ " onCurve: "  ++ show onCurve ++ " rightMid: " ++ show rightMid) $
        splitKnob' bottom t convex (bottom + ((t - bottom) / 2)) v0 control v1 -- search closer to v1
  | otherwise =
        --tr "otherwise" $
        (leftMid, onCurve, rightMid)
  where (leftMid, onCurve, rightMid) = curvePoint t v0 control v1

checkKnob :: (Show s, Fractional s, Ord s, Num s, Iota s) => Triple (Point2 s) -> [Triple (Point2 s)]
checkKnob (a, b, c) =
  --tc ("checkKnob " ++ show (pX a) ++ " " ++ show (pX b) ++ " " ++ show (pX c)) $
    if leftConvex b a && leftConvex b c
    then
      --tr "left convex " $
      let (leftMid, onCurve, rightMid) = --tc ("splitLeftKnob a:" ++ show a ++ " b: " ++ show b ++ " c: " ++ show c) $
                                         splitLeftKnob a b c
      in [(a,leftMid,onCurve), (onCurve,rightMid,c)]
    else
      if rightConvex b a && rightConvex b c
      then
        --tr "right convex " $
        let (leftMid, onCurve, rightMid) = --tc ("splitRightKnob a:" ++ show a ++ " b: " ++ show b ++ " c: " ++ show c) $
                                           splitRightKnob a b c
        in [(a,leftMid,onCurve), (onCurve,rightMid,c)]
      else
        --tr "not convex " $
        [(a,b, c)]

smidge :: (Num s, Iota s) => s
smidge = iota * 100

compareHorizontal :: Ord s => Point2 s -> Point2 s -> Ordering
compareHorizontal a b = compare (a ^. pX) (b ^. pX)

direction :: Ord s => Triple (Point2 s) -> (Ordering, [Triple (Point2 s)])
direction (a, control, b) = (compareHorizontal a b, [(a, control, b)])

groupDirections :: [(Ordering , [a])] -> [(Ordering, [a])]
groupDirections ((ac, as):(bc, bs):ss)= if ac == bc
                                        then groupDirections ((ac, as ++ bs):ss)
                                        else (ac, as):groupDirections((bc, bs):ss)
groupDirections ss                    = ss

connectLoop :: Show a => [(Ordering, [a])] -> [(Ordering, [a])]
connectLoop ss = let ((ac, as), mid, (bc, bs)) = takeFirstLast ss
                 in  if ac == bc && ac /= EQ
                     then (ac, bs ++ as):mid
                     else ss

trimEQ :: Show p => (Ordering, [Triple p]) -> (Ordering, [Triple p])
trimEQ (EQ, [a]) = (EQ, [a])
trimEQ (EQ, ss ) = let ((a,b,_),_,(_,_,c)) = takeFirstLast ss
                   in (EQ,[(a,b,c)])
trimEQ x         = x


attachEQ :: [(Ordering, [Triple p])] -> [(Maybe (Triple p),(Ordering,[Triple p]), Maybe (Triple p))]
attachEQ ((EQ,[a]):x:y@(EQ,[b]):xs) = (Just a , x, Just b ):attachEQ(y:xs)
attachEQ ((EQ,[a]):x:           xs) = (Just a , x, Nothing):attachEQ   xs
attachEQ ((EQ,_  ):             []) = []
attachEQ (         x:y@(EQ,[b]):xs) = (Nothing, x, Just b ):attachEQ(y:xs)
attachEQ (         x:           xs) = (Nothing, x, Nothing):attachEQ   xs
attachEQ []                         = []

vertical :: (Num s, Ord s, Iota s) => Maybe (Triple (Point2 s)) -> Ortho YDimension s
vertical (Just (a, _, b)) = b ^. pY - b ^. pY
vertical _                = 0

flipTriple :: (a, b, a) -> (a, b, a)
flipTriple (a, x, b) = (b, x, a)

simpleTriple :: (p, Maybe p, p) -> (p, p, p)
simpleTriple (a, Just control, b) = (a, control, b)
simpleTriple (a, Nothing, b) = (a, a, b)

concatSimples' :: [(p, p, p)] -> (p, [(p, p)])
concatSimples' ((a, b, c):[]) = (c, [(a, b)])
concatSimples' ((a, b, c):xs) = let (right, rest) = concatSimples' xs
                                in  (right, (a,b):rest)
concatSimples' [] = error "concatSimples' encountered empty list"

concatSimples :: [(p, p, p)] -> (p, p, p, [(p, p)])
concatSimples ((left, control, right):[]) = (right, left, control, [])
concatSimples ((left, control, _):xs) = let (right, rest) = concatSimples' xs
                                        in  (right, left, control, rest)
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

{-
buildStrand :: (Num s, Ord s, Iota s) => (Maybe (Triple (Point2 s)), (Ordering, [Triple (Point2 s)]), Maybe (Triple (Point2 s))) -> [Triple (Point2 s)]
buildStrand (start, (LT, xs), end) =                            buildStrand' start xs end
buildStrand (start, (GT, xs), end) = reverse . map flipTriple $ buildStrand' start xs end
buildStrand (start, (EQ, xs), end) = error "buildStrand'' encountered EQ section."
-}

buildStrand :: (Num s, Ord s, Iota s) => (Maybe (Triple (Point2 s)), (Ordering, [Triple (Point2 s)]), Maybe (Triple (Point2 s))) -> [Triple (Point2 s)]
buildStrand (start, (LT, xs), end) = buildStrand' start                                            xs                  end
buildStrand (start, (GT, xs), end) = buildStrand' (flipTriple <$> end) (reverse . map flipTriple $ xs) (flipTriple <$> start)
buildStrand (start, (EQ, xs), end) = error "buildStrand'' encountered EQ section."

splitLong :: (Show p) => Int -> [p] -> [[p]]
splitLong sectionSize section = let triplesPerSection = sectionSize `div` 2
                                in  splitLong' triplesPerSection section

splitLong' :: (Show p) => Int -> [p] -> [[p]]
splitLong' triplesPerSection (triples) = let (part, rest) = splitAt triplesPerSection triples
                                         in  if length triples > triplesPerSection
                                             then part:splitLong' triplesPerSection rest
                                             else [triples]

wrapEQ :: (Show p) => [(Ordering, [Triple p])] -> [(Ordering, [Triple p])]
wrapEQ xs = let (f, mid, l) = takeFirstLast xs
            in  case f of
                (EQ, _) -> xs ++ [f]
                _       -> case l of
                             (EQ, _) -> l:xs
                             _       -> xs

buildFromDirections :: CurveTable
                    -> Int
                    -> [(Ordering, [Triple (Point2 DisplaySpace)])]
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


splitShape :: CurveTable
           -> Int
           -> [CurvePair (Point2 DisplaySpace)]
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


makeVertexTree :: CurveTable
               -> Int
               -> [CurvePair (Point2 DisplaySpace)]
               -> [Strand]
makeVertexTree table sectionSize vs =

  if length vs < 3
  then []
  else splitShape table sectionSize vs

-------------------- Shape To Curve Strands ------------------------

shapeToStrands :: CurveTable
               -> Int
               -> Outline DisplaySpace
               -> [Strand]
shapeToStrands table sectionSize (Outline ps) = makeVertexTree table sectionSize ps
