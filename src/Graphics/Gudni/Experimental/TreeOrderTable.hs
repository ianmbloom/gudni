{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.StrandLookupTable
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for building a lookup table for reordering sequenced strands into
-- binary trees based on their length.

module Graphics.Gudni.Experimental.TreeOrderTable
  ( TreeOrderTable(..)
  , buildTreeOrderTable
  , treeOrderFold
  )
where

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Debug
import qualified Data.Vector as V
import Data.Vector ((!))
import Linear.V3

import Control.Lens

type TreeOrderTable = V.Vector (V.Vector Int)

data ITree =
  IBranch
  { iTreeIndex :: Int
  , iTreeLeft  :: ITree
  , iTeeeRight :: ITree
  } |
  ILeaf

-- | Convert an ITree to a list.
subForest :: ITree -> [ITree]
subForest (IBranch _ left right) = [left, right]
subForest ILeaf = []

powerLess :: Int -> Int -> Int
powerLess n x =
  --find a power of 2 <= n//2
  if x <= n `div` 2 then powerLess n (x * 2) else x

perfectTreePartition :: Int -> Int
perfectTreePartition n =
    -- Find the point to partition n nodes for a perfect binary tree.
    let x = powerLess n 1
    in
    if (x `div` 2) - 1 <= n - x
    then x - 1             -- case 1
                           -- the left subtree of the root is perfect and the right subtree has less nodes or
    else n - (x `div` 2)   -- case 2 == n - (x//2 - 1) - 1
                           -- the left subtree of the root has more nodes and the right subtree is perfect.

buildITree :: [Int] -> ITree
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

makeTreeRow :: Int -> [Int]
makeTreeRow size =
  let internal = [0..size - 1]
  in  breadth $ buildITree internal

fromTreeTable :: TreeOrderTable -> Int -> Int -> Int
fromTreeTable table size i = (table ! size) ! i

buildTreeOrderTable :: Int -> TreeOrderTable
buildTreeOrderTable maxSize = V.fromList . map (V.fromList . makeTreeRow) $ [0..maxSize]

treeOrderFold :: TreeOrderTable -> (a -> b -> a) -> a -> V.Vector b -> a
treeOrderFold table f start vector =
    let len = V.length vector
    in  foldl f start . map ((vector !) . fromTreeTable table len) $ [0..len - 1]
