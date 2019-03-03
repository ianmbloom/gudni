{-# LANGUAGE FlexibleContexts #-}

module Graphics.Gudni.Raster.StrandLookupTable
  ( CurveTable
  , buildCurveTable
  , makeCurveStrand
  )
where

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Debug
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Data.Vector ((!))
import Linear.V3

import Control.Lens

data ITree =
  IBranch
  { iTreeIndex :: Int
  , iTreeLeft  :: ITree
  , iTeeeRight :: ITree
  } |
  ILeaf

subForest (IBranch _ left right) = [left, right]
subForest ILeaf = []

powerLess n x =
  --find a power of 2 <= n//2
  if x <= n `div` 2 then powerLess n (x * 2) else x

perfectTreePartition n =
    -- Find the point to partition n nodes for a perfect binary tree.
    let x = powerLess n 1
    in
    if (x `div` 2) - 1 <= n - x
    then x - 1             -- case 1
                           -- the left subtree of the root is perfect and the right subtree has less nodes or
    else n - (x `div` 2)   -- case 2 == n - (x//2 - 1) - 1
                           -- the left subtree of the root has more nodes and the right subtree is perfect.

buildITree xs =
   let l = length xs
       half = perfectTreePartition l
       left = take half xs
       rightCenter = drop half xs
       center = head rightCenter
       right = tail rightCenter
   in
       if l > 0
       then IBranch center (buildITree left) (buildITree right)
       else ILeaf

getIndex :: ITree -> [Int]
getIndex ILeaf = []
getIndex x = [iTreeIndex x]

breadth :: ITree -> [Int]
--breadth ILeaf = []
breadth nd = concatMap getIndex $ nd : breadth' [nd]
    where breadth' []  = []
          breadth' nds = let cs = foldr ((++).subForest) [] nds
                         in  cs ++ breadth' cs

curveLine :: Int -> Int -> (V.Vector Int)
curveLine maxSize size =
  let internal = if size > 3 then [0..(((size-3) `div` 2) - 1)] else []
      halfTree = breadth $ buildITree internal
      doubleTree = map (*2) halfTree
      tree = map (+3) $ concatMap (\x -> [x, x+1]) doubleTree
      rightBound = 0
      leftBound  = 1
      leftControl  = 2
  in V.fromList $ rightBound:leftBound:leftControl:tree

type CurveTable = V.Vector (V.Vector Int)

buildCurveTable :: Int -> CurveTable
buildCurveTable maxSize = V.fromList $ map (curveLine maxSize . (+3) . (*2)) [0..maxSize `div` 2]

makeCurveStrand :: (Show s, Ord s, Num s, Iota s, VS.Storable s)
                => CurveTable
                -> (V3 (Point2 s), [(Point2 s, Point2 s)] )
                -> VS.Vector (Point2 s)
makeCurveStrand table (V3 right left control, rest) =
  let curveVector = V.fromList (right:left:control:concatMap (\(x,y) -> [x,y]) rest)
      direction = compare (V.head curveVector ^. pX) (V.last curveVector ^. pX) -- determine the direction of the entire strand.
      size = V.length curveVector
      line = table ! {- tr ("size" ++ show size ++ "table index") -} (((size - 1) `div` 2) - 1)
  in
  if   size >= 3 && odd size
  then VS.generate size (\x -> curveVector ! (line ! x))
  else error ("makeCurveStrand must have size >= 3 and odd size." ++ show curveVector)
