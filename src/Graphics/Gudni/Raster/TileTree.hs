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
  , TileEntry(..)
  , tileShapes
  , tileStrandCount
  , tileShapeCount
  , buildTileTree
  , addItemToTree
  , traverseTileTree
  , adjustedLog
  )
where

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.TileEntry
import Graphics.Gudni.Raster.SubstanceInfo
import Graphics.Gudni.Figure hiding (reverse)
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Util.Debug

import Control.Lens

import Data.Tree

type Width  = X SubSpace
type Height = Y SubSpace
type Size   = Int

-- | A tile entry is the intermediate storage for the contents of a tile.
data TileEntry = TileEntry
    { _tileShapes        :: [ItemTag]
    , _tileLastSubstance :: (Maybe SubstanceId)
    , _tileStrandCount   :: NumStrands
    , _tileShapeCount    :: Size
    } deriving (Show)
makeLenses ''TileEntry

-- | A TileTree is a XY partition tree for dividing the canvas into tiles.
type TileTree = VTree

-- | HTrees divide the space into two horiztonally adjacent partitions
-- left and right of the cut line.
data HTree = HTree
    { hCut        :: Width
    , leftBranch  :: VTree
    , rightBranch :: VTree
    }
    | HLeaf (Tile TileEntry)

-- | VTrees divide the space into two vertically adjacent partitions
-- above and below the cut line.
data VTree = VTree
    { vCut         :: Height
    , topBranch    :: HTree
    , bottomBranch :: HTree
    }
    | VLeaf (Tile TileEntry)

-- | Return the ceiling value of a log2 x adjusted to zero for x < 1.
adjustedLog :: (Integral s, Integral v )=> s -> v
adjustedLog x = if x < 1 then 0 else ceiling . logBase 2 . fromIntegral $ x

-- | Create an initial empty tile
emptyTile :: Int -> Int -> Box PixelSpace -> Tile TileEntry
emptyTile hDepth vDepth box = Tile box hDepth vDepth (TileEntry [] Nothing 0 0)

buildTileTree :: PixelSpace -> Point2 PixelSpace -> TileTree
buildTileTree tileSize canvasSize = goV canvasDepth box
    where
    -- Choose the largest dimension of the canvas as the square side dimension of the area covered by the tileTree.
    maxCanvasDimension = max (unOrtho $ canvasSize ^. pX) (unOrtho $ canvasSize ^. pY)
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
          else VLeaf $ emptyTile depth depth box
    -- split the tile by dividing into a horizontal row.
    goH depth box =
      let hIntCut = box ^. leftSide + (2 ^ (depth - 1))
          hCut = fromIntegral hIntCut
      in  if depth > tileDepth
          then HTree hCut (goV (depth - 1) (set rightSide hIntCut box))
                          (goV (depth - 1) (set leftSide  hIntCut box))
          else HLeaf $ emptyTile depth depth box

-- | Add a shape to a tile tree by choosing one side of each branch or copying a shape
-- that bridges the cut.
addItemToTree :: NumStrands -> NumStrands -> BoundingBox -> TileTree -> ItemTag -> TileTree
addItemToTree maxStrandsPerTile strandCount itemBox tree = {-tr "result" .-} insertItemV tree {-. tr "addShape"-}
    where
    -- | Add a shape to an HTree
    insertItemH :: HTree -> ItemTag -> HTree
    insertItemH (HTree cut left right) newItem =
        let left'  = -- if the left side of the shape is left of the cut add it to the left branch
                     if itemBox ^. leftSide < cut
                     then insertItemV left newItem
                     else left
            right' = -- if the right side of the shape is right of the cut add it to the right branch
                     if itemBox ^. rightSide > cut
                     then insertItemV right newItem
                     else right
        in  HTree cut left' right'
    insertItemH (HLeaf tile) newItem =
      if -- the tile has room for more shapes or its to small to be split again
         checkTileSpace tile newItem || widthOf (tile ^. tileBox) <= mINtILEsIZE ^. pX
      then -- then simple add it.
           HLeaf $ insertItemTile tile newItem
      else -- otherwise split the tile in half and then add the shape to the split tiles.
           insertItemH (hSplit tile) newItem

    insertItemV :: VTree -> ItemTag -> VTree
    insertItemV (VTree cut top bottom) newItem =
        let top'    = if itemBox ^. topSide < cut
                      then insertItemH top newItem
                      else top
            bottom' = if itemBox ^. bottomSide > cut
                      then insertItemH bottom newItem
                      else bottom
        in  VTree cut top' bottom'
    insertItemV (VLeaf tile) newItem =
        if -- the tile has room for more shapes or its to small to be split again
           checkTileSpace tile newItem || heightOf (tile ^. tileBox) <= mINtILEsIZE ^. pY
        then -- then simple add it.
             VLeaf $ insertItemTile tile newItem
        else -- otherwise split the tile in half and then add the shape to the split tiles.
             insertItemV (vSplit tile) newItem

    -- | Add a shape to a tile.
    insertItemTile :: Tile TileEntry -> ItemTag -> Tile TileEntry
    insertItemTile tile newItem =
      let tileEntry = tile ^. tileRep
          isFacet = tagIsFacet newItem
          newEntry = if (isFacet && (Just (tagToSubstanceId newItem) == tileEntry ^. tileLastSubstance)) || not isFacet
                     then   over tileShapes (newItem:)
                          . over tileStrandCount (+ strandCount)
                          . over tileShapeCount (+ 1)
                          $ tileEntry
                     else tileEntry
          recordedLast = if not isFacet
                         then set tileLastSubstance (Just (tagToSubstanceId newItem)) newEntry
                         else newEntry
      in  set tileRep recordedLast tile

    -- | Check if the tile can hold an additional shape without splitting.
    checkTileSpace :: Tile TileEntry -> ItemTag -> Bool
    checkTileSpace tile newItem =
      let tileEntry = tile ^. tileRep
          withAddedStrands = (tileEntry ^. tileStrandCount + strandCount)
      in     tileEntry ^. tileShapeCount < mAXlAYERS - 1 -- the total shapes would be less than the maximum
          && withAddedStrands < (maxStrandsPerTile) -- the total strands would be less than the maximum.

    -- | Split a tile into two horizontal sections and put its contents into both sides in the proper order (reversed)
    hSplit :: Tile TileEntry -> HTree
    hSplit tile =
      let cut = tile ^. tileBox . leftSide + (widthOf (tile ^. tileBox) `div` 2)
          lEmpty = emptyTile (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set rightSide cut (tile ^. tileBox))
          rEmpty = emptyTile (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set leftSide cut (tile ^. tileBox))
          hTree = HTree (fromIntegral cut) (VLeaf lEmpty) (VLeaf rEmpty)
      in  {-tr "hSplit" $-} foldl insertItemH hTree $ reverse (tile ^. tileRep . tileShapes)

    -- | Split a tile into two vertical sections and put its contents into both sides in the proper order (reversed)
    vSplit :: Tile TileEntry -> VTree
    vSplit tile =
      let cut = tile ^. tileBox . topSide + (heightOf (tile ^. tileBox) `div` 2)
          tEmpty = emptyTile (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set bottomSide cut (tile ^. tileBox))
          bEmpty = emptyTile (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set topSide    cut (tile ^. tileBox))
          vTree = VTree (fromIntegral cut) (HLeaf tEmpty) (HLeaf bEmpty)
      in  {-tr "vSplit" $-} foldl insertItemV vTree $ reverse (tile ^. tileRep . tileShapes)

-- | Traverse a TileTree with a monadic function.
traverseTileTree :: Monad m => (Tile TileEntry -> m t) -> TileTree -> m t
traverseTileTree = traverseTileTreeV

traverseTileTreeH :: Monad m => (Tile TileEntry -> m t) -> HTree -> m t
traverseTileTreeH f (HTree _ left right) = do traverseTileTreeV f left
                                              traverseTileTreeV f right
traverseTileTreeH f (HLeaf tile) = f tile

traverseTileTreeV :: Monad m => (Tile TileEntry -> m t) -> VTree -> m t
traverseTileTreeV f (VTree _ top bottom) = do traverseTileTreeH f top
                                              traverseTileTreeH f bottom
traverseTileTreeV f (VLeaf tile) = f tile

-- | Display the contents of a tile.
showTile tile = " (" ++ show (widthOf $ tile ^. tileBox)
              ++ "X" ++ show (heightOf $ tile ^. tileBox)
              ++ ") Shapes " ++ show (tile ^. tileRep . tileShapeCount)
              ++ " Strands " ++ show (tile ^. tileRep . tileStrandCount)

              -- ++ " Shapes " ++ show (map (view shapeBox . view shRep) $ tile ^. tileRep . tileShapes)

-- | Display a TileTree by converting it first to a data tree and drawing it.
toDataTreeH (HLeaf tile) = Node (showTile tile) []
toDataTreeH (HTree hCut left right) = Node ("H" ++ show hCut) [toDataTreeV left, toDataTreeV right]

toDataTreeV (VLeaf tile) = Node (showTile tile) []
toDataTreeV (VTree vCut top bottom) = Node ("V" ++ show vCut) [toDataTreeH top, toDataTreeH bottom]

instance Show HTree where
  show = drawTree . toDataTreeH

instance Show VTree where
  show = drawTree . toDataTreeV
