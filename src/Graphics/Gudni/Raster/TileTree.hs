{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.TileTree
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for partitioning the canvas into a tree of tiles.

module Graphics.Gudni.Raster.TileTree
  ( TileTree(..)
  , EntrySequence(..)
  , TileEntry(..)
  , ItemEntry(..)
  , itemEntryTag
  , itemStrandCount
  , itemBox
  , tileItems
  , buildTileTree
  , addItemToTree
  , traverseTileTree
  , adjustedLog
  , splitTreeTiles
  )
where

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.Tile
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Util.Debug

import Control.Lens

import Data.Tree
import qualified Data.Sequence as S
import Data.Sequence ((|>),(<|))

type Width  = SubSpace
type Height = SubSpace
type Size   = Int

-- | A wrapper for ItemTags
data ItemEntry = ItemEntry
    { _itemEntryTag    :: ItemTag
    , _itemStrandCount :: NumStrands
    , _itemBox         :: BoundingBox
    } deriving (Show)
makeLenses ''ItemEntry

type EntrySequence = S.Seq ItemEntry

-- | A tile entry is the intermediate storage for the contents of a tile.
data TileEntry = TileEntry
    { _tileItems      :: EntrySequence
    } deriving (Show)
makeLenses ''TileEntry


-- | A TileTree is a XY partition tree for dividing the canvas into tiles.
type TileTree item = VTree item

-- | HTrees divide the space into two horiztonally adjacent partitions
-- left and right of the cut line.
data HTree item = HTree
    { hCut        :: Width
    , leftBranch  :: VTree item
    , rightBranch :: VTree item
    }
    | HLeaf (Tile item)

-- | VTrees divide the space into two vertically adjacent partitions
-- above and below the cut line.
data VTree item = VTree
    { vCut         :: Height
    , topBranch    :: HTree item
    , bottomBranch :: HTree item
    }
    | VLeaf (Tile item)

-- | Return the ceiling value of a log2 x adjusted to zero for x < 1.
adjustedLog :: (Integral s, Integral v )=> s -> v
adjustedLog x = if x < 1 then 0 else ceiling . logBase 2 . fromIntegral $ x

-- | Create an initial empty tile
emptyTile :: a -> Int -> Int -> Box PixelSpace -> Tile a
emptyTile emptyRep hDepth vDepth box = Tile box hDepth vDepth emptyRep

buildTileTree :: a -> PixelSpace -> Point2 PixelSpace -> TileTree a
buildTileTree emptyRep  tileSize canvasSize = goV canvasDepth box
    where
    -- Choose the largest dimension of the canvas as the square side dimension of the area covered by the tileTree.
    maxCanvasDimension = max (canvasSize ^. pX) (canvasSize ^. pY)
    -- Canvas depth is the adjusted log2 of the larges side of the canvas.
    canvasDepth = adjustedLog maxCanvasDimension
    -- Initial tile depth is the adjusted log of the max tileSize
    tileDepth   = adjustedLog tileSize
    -- The dimensions of the area covered by the tree will be a square with dimensions the smallest power of two
    -- the contains both sides of the canvas. This is not a problem because the incoming shapes will still be excluded
    -- based on the dimensions of the canvas and empty tiles will just have their threads inactive.
    box = pointToBox $ makePoint (2 ^ canvasDepth) (2 ^ canvasDepth)
    -- split the tile by dividing into a vertical stack
    goV depth box =
      let vIntCut = box ^. topSide + (2 ^ (depth - 1))
          vCut = fromIntegral vIntCut
      in  if depth > tileDepth
          then VTree vCut (goH depth (set bottomSide vIntCut box))
                          (goH depth (set topSide    vIntCut box))
          else VLeaf $ emptyTile emptyRep depth depth box
    -- split the tile by dividing into a horizontal row.
    goH depth box =
      let hIntCut = box ^. leftSide + (2 ^ (depth - 1))
          hCut = fromIntegral hIntCut
      in  if depth > tileDepth
          then HTree hCut (goV (depth - 1) (set rightSide hIntCut box))
                          (goV (depth - 1) (set leftSide  hIntCut box))
          else HLeaf $ emptyTile emptyRep depth depth box

-- | Add an itemEntry to a tile.
insertItemTile :: Tile EntrySequence -> ItemEntry -> Tile EntrySequence
insertItemTile tile itemEntry = over tileRep (flip (S.|>) itemEntry) tile

-- | Add an itemEntry to a tile tree.

addItemToTree :: TileTree EntrySequence -> ItemEntry -> TileTree EntrySequence
addItemToTree tree = insertItemV tree

-- | Add a shape to an HTree
insertItemH :: HTree EntrySequence -> ItemEntry -> HTree EntrySequence
insertItemH (HTree cut left right) itemEntry =
    let left'  = -- if the left side of the shape is left of the cut add it to the left branch
                 if itemEntry ^. itemBox . leftSide < cut
                 then insertItemV left itemEntry
                 else left
        right' = -- if the right side of the shape is right of the cut add it to the right branch
                 if itemEntry ^. itemBox . rightSide > cut
                 then insertItemV right itemEntry
                 else right
    in  HTree cut left' right'
insertItemH (HLeaf tile) itemEntry =
       HLeaf $ insertItemTile tile itemEntry


insertItemV :: VTree EntrySequence -> ItemEntry -> VTree EntrySequence
insertItemV (VTree cut top bottom) itemEntry =
    let top'    = if itemEntry ^. itemBox . topSide < cut
                  then insertItemH top itemEntry
                  else top
        bottom' = if itemEntry ^. itemBox . bottomSide > cut
                  then insertItemH bottom itemEntry
                  else bottom
    in  VTree cut top' bottom'
insertItemV (VLeaf tile) itemEntry =
    VLeaf $ insertItemTile tile itemEntry

splitTreeTiles :: NumStrands -> TileTree EntrySequence -> TileTree EntrySequence
splitTreeTiles maxThresholds tree = goV tree
    where
        goH tree =
           case tree of
             (HTree cut left right) -> HTree cut (goV left) (goV right)
             (HLeaf tile) -> if shouldSplitH maxThresholds tile
                             then goH $ hSplit tile
                             else HLeaf tile
        goV tree =
           case tree of
             (VTree cut top bottom) -> VTree cut (goH top) (goH bottom)
             (VLeaf tile) -> if shouldSplitV maxThresholds tile
                             then goV $ vSplit tile
                             else VLeaf tile

hSplit :: Tile EntrySequence -> HTree EntrySequence
hSplit tile =
  let cut = tile ^. tileBox . leftSide + (widthOf (tile ^. tileBox) `div` 2)
      lEmpty = emptyTile S.empty (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set rightSide cut (tile ^. tileBox))
      rEmpty = emptyTile S.empty (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set leftSide cut (tile ^. tileBox))
      hTree = HTree (fromIntegral cut) (VLeaf lEmpty) (VLeaf rEmpty)
  in  foldl insertItemH hTree $ (tile ^. tileRep)

vSplit :: Tile EntrySequence -> VTree EntrySequence
vSplit tile =
  let cut = tile ^. tileBox . topSide + (heightOf (tile ^. tileBox) `div` 2)
      tEmpty = emptyTile S.empty (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set bottomSide cut (tile ^. tileBox))
      bEmpty = emptyTile S.empty (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set topSide    cut (tile ^. tileBox))
      vTree = VTree (fromIntegral cut) (HLeaf tEmpty) (HLeaf bEmpty)
  in  foldl insertItemV vTree $ (tile ^. tileRep)

shouldSplitV :: NumStrands -> Tile EntrySequence -> Bool
shouldSplitV maxThresholds tile =
   heightOf (tile ^. tileBox) > mINtILEsIZE ^. pY &&
   (length (tile ^. tileRep) > mAXlAYERS ||
   sum (fmap (view itemStrandCount) (tile ^. tileRep)) > maxThresholds)

shouldSplitH :: NumStrands -> Tile EntrySequence -> Bool
shouldSplitH maxThresholds tile =
   widthOf (tile ^. tileBox) > mINtILEsIZE ^. pX &&
   (length (tile ^. tileRep) > mAXlAYERS ||
   sum (fmap (view itemStrandCount) (tile ^. tileRep)) > maxThresholds)

-- | Traverse a TileTree with a monadic function.
traverseTileTree :: Monad m => (Tile a -> m t) -> TileTree a -> m t
traverseTileTree = traverseTileTreeV

traverseTileTreeH :: Monad m => (Tile a -> m t) -> HTree a -> m t
traverseTileTreeH f (HTree _ left right) = do traverseTileTreeV f left
                                              traverseTileTreeV f right
traverseTileTreeH f (HLeaf tile) = f tile

traverseTileTreeV :: Monad m => (Tile a -> m t) -> VTree a -> m t
traverseTileTreeV f (VTree _ top bottom) = do traverseTileTreeH f top
                                              traverseTileTreeH f bottom
traverseTileTreeV f (VLeaf tile) = f tile

-- | Display the contents of a tile.
showTile tile = " (" ++ show (widthOf $ tile ^. tileBox)
              ++ "X" ++ show (heightOf $ tile ^. tileBox)
              ++ " tileRep " ++ show (tile ^. tileRep)

-- | Display a TileTree by converting it first to a data tree and drawing it.
toDataTreeH (HLeaf tile) = Node (showTile tile) []
toDataTreeH (HTree hCut left right) = Node ("H" ++ show hCut) [toDataTreeV left, toDataTreeV right]

toDataTreeV (VLeaf tile) = Node (showTile tile) []
toDataTreeV (VTree vCut top bottom) = Node ("V" ++ show vCut) [toDataTreeH top, toDataTreeH bottom]

instance Show a => Show (HTree a) where
  show = drawTree . toDataTreeH

instance Show a => Show (VTree a) where
  show = drawTree . toDataTreeV
