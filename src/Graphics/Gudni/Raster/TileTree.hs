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
  , addShapeToTree
  , traverseTileTree
  , adjustedLog
  )
where

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.Types
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Util.Debug

import Control.Lens

import Data.Tree

type Width  = Ortho XDimension SubSpace
type Height = Ortho YDimension SubSpace
type Size   = Int

-- | A tile entry is the intermediate storage for the contents of a tile.
data TileEntry = TileEntry
    { _tileShapes      :: [Shape ShapeEntry]
    , _tileStrandCount :: NumStrands
    , _tileShapeCount  :: Size
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
emptyTile hDepth vDepth box = Tile box hDepth vDepth (TileEntry [] 0 0)

-- | Build an empty TileTree based on the canvas size
buildTileTree :: Point2 PixelSpace -> TileTree
buildTileTree = buildTileTree' (fromIntegral $ mAXtILEsIZE ^. pX)

buildTileTree' :: PixelSpace -> Point2 PixelSpace -> TileTree
buildTileTree' tileSize canvasSize = goV canvasDepth box
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
addShapeToTree :: TileTree -> Shape ShapeEntry -> TileTree
addShapeToTree tree = {-tr "result" .-} insertShapeV tree {-. tr "addShape"-}

-- | Add a shape to an HTree
insertShapeH :: HTree -> Shape ShapeEntry -> HTree
insertShapeH (HTree cut left right) shapeEntry =
    let left'  = -- if the left side of the shape is left of the cut add it to the left branch
                 if shapeEntry ^. shRep . shapeBox . leftSide < cut
                 then insertShapeV left shapeEntry
                 else left
        right' = -- if the right side of the shape is right of the cut add it to the right branch
                 if shapeEntry ^. shRep . shapeBox . rightSide > cut
                 then insertShapeV right shapeEntry
                 else right
    in  HTree cut left' right'
insertShapeH (HLeaf tile) shapeEntry =
  if -- the tile has room for more shapes or its to small to be split again
     checkTileSpace tile shapeEntry || widthBox (tile ^. tileBox) <= mINtILEsIZE ^. pX
  then -- then simple add it.
       HLeaf $ insertShapeTile tile shapeEntry
  else -- otherwise split the tile in half and then add the shape to the split tiles.
       insertShapeH (hSplit tile) shapeEntry

insertShapeV :: VTree -> Shape ShapeEntry -> VTree
insertShapeV (VTree cut top bottom) shapeEntry =
    let top'    = if shapeEntry ^. shRep . shapeBox . topSide < cut
                  then insertShapeH top shapeEntry
                  else top
        bottom' = if shapeEntry ^. shRep . shapeBox . bottomSide > cut
                  then insertShapeH bottom shapeEntry
                  else bottom
    in  VTree cut top' bottom'
insertShapeV (VLeaf tile) shapeEntry =
    if -- the tile has room for more shapes or its to small to be split again
       checkTileSpace tile shapeEntry || heightBox (tile ^. tileBox) <= mINtILEsIZE ^. pY
    then -- then simple add it.
         VLeaf $ insertShapeTile tile shapeEntry
    else -- otherwise split the tile in half and then add the shape to the split tiles.
         insertShapeV (vSplit tile) shapeEntry

-- | Add a shape to a tile.
insertShapeTile :: Tile TileEntry -> Shape ShapeEntry -> Tile TileEntry
insertShapeTile tile shapeEntry =
  let tileEntry = tile ^. tileRep
      newEntry = TileEntry { -- append the shape.
                             _tileShapes = shapeEntry:tileEntry ^. tileShapes
                             -- add to the total strand count.
                           , _tileStrandCount = tileEntry ^. tileStrandCount + shapeEntry ^. shRep . shapeStrandCount
                             -- increment the shape count.
                           , _tileShapeCount = tileEntry ^. tileShapeCount + 1
                           }
  in  set tileRep newEntry tile

-- | Check if the tile can hold an additional shape without splitting.
checkTileSpace :: Tile TileEntry -> Shape ShapeEntry -> Bool
checkTileSpace tile shapeEntry =
  let tileEntry = tile ^. tileRep
      withAddedStrands = (tileEntry ^. tileStrandCount + (shapeEntry ^. shRep . shapeStrandCount))
  in     tileEntry ^. tileShapeCount < mAXsHAPE - 1 -- the total shapes would be less than the maximum
      && withAddedStrands < (NumStrands . fromIntegral $ mAXsTRANDpERtILE) -- the total strands would be less than the maximum.

-- | Split a tile into two horizontal sections and put its contents into both sides in the proper order (reversed)
hSplit :: Tile TileEntry -> HTree
hSplit tile =
  let cut = tile ^. tileBox . leftSide + (widthBox (tile ^. tileBox) `div` 2)
      lEmpty = emptyTile (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set rightSide cut (tile ^. tileBox))
      rEmpty = emptyTile (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set leftSide cut (tile ^. tileBox))
      hTree = HTree (fromIntegral cut) (VLeaf lEmpty) (VLeaf rEmpty)
  in  {-tr "hSplit" $-} foldl insertShapeH hTree $ reverse (tile ^. tileRep . tileShapes)

-- | Split a tile into two vertical sections and put its contents into both sides in the proper order (reversed)
vSplit :: Tile TileEntry -> VTree
vSplit tile =
  let cut = tile ^. tileBox . topSide + (heightBox (tile ^. tileBox) `div` 2)
      tEmpty = emptyTile (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set bottomSide cut (tile ^. tileBox))
      bEmpty = emptyTile (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set topSide    cut (tile ^. tileBox))
      vTree = VTree (fromIntegral cut) (HLeaf tEmpty) (HLeaf bEmpty)
  in  {-tr "vSplit" $-} foldl insertShapeV vTree $ reverse (tile ^. tileRep . tileShapes)

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
showTile tile = " (" ++ show (widthBox $ tile ^. tileBox)
              ++ "X" ++ show (heightBox $ tile ^. tileBox)
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
