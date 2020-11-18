{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Graphics.Gudni.Raster.Thresholds.TileTree
  ( TileTree(..)
  --, tileItems
  , buildTileTree
  , buildTileTreeM
  , addItemTagIdToTree
  , addItemTagIdToTreePile
  , Tile (..)
  , TileId(..)
  , tileBox
  , locatePointInTileTree
  , foldMapTileTree
  , traverseTileTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.Thresholds.Constants
import Graphics.Gudni.Raster.Thresholds.StrandReference
import Graphics.Gudni.Raster.Thresholds.ItemInfo
import Graphics.Gudni.Raster.Thresholds.Enclosure
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Raster.Serial.Slice
import Graphics.Gudni.Raster.Serial.Pile

import Graphics.Gudni.Util.StorableM

import Control.Lens hiding ((|>),(<|))
import Control.Monad.State

import Data.Tree
import qualified Data.Sequence as S
import Data.Sequence ((|>),(<|))
import Foreign.C.Types(CInt)
import Foreign.Ptr

type TileId_ = Int
newtype TileId = TileId {unTileId :: TileId_} deriving (Show, Eq, Ord, Num)

newtype PointQueryId = PointQueryId {unPointQueryId :: Int} deriving (Show, Eq, Ord)

-- | Tile is just a pairing of the Tile Info Header and some representation of its contents.
data Tile = Tile
  { -- | Pixel boundaries of tile.
    _tileBox    :: !(Box PixelSpace)
  } deriving (Show, Eq)
makeLenses ''Tile

-- | A TileTree is a XY partition tree for dividing the canvas into tiles.
type TileTree leaf = VTree leaf

-- | HTrees divide the space into two horiztonally adjacent partitions
-- left and right of the cut line.
data HTree leaf = HTree
    { hCut        :: Ax Horizontal SubSpace
    , leftBranch  :: VTree leaf
    , rightBranch :: VTree leaf
    }
    | HLeaf leaf

-- | VTrees divide the space into two vertically adjacent partitions
-- above and below the cut line.
data VTree leaf = VTree
    { vCut         :: Ax Vertical SubSpace
    , topBranch    :: HTree leaf
    , bottomBranch :: HTree leaf
    }
    | VLeaf leaf

buildTileTree :: Point2 PixelSpace -> PixelSpace -> a -> TileTree (Tile, a)
buildTileTree canvasSize tileSize emptyRep = goV canvasDepth box
    where
    -- Choose the largest dimension of the canvas as the square side dimension of the area covered by the tileTree.
    maxCanvasDimension = max (fromAlong Horizontal $ canvasSize ^. pX) (fromAlong Vertical $ canvasSize ^. pY)
    -- Canvas depth is the adjusted log2 of the larges side of the canvas.
    canvasDepth = adjustedLog (fromIntegral maxCanvasDimension)
    -- Initial tile depth is the adjusted log of the max tileSize
    tileDepth   = adjustedLog (fromIntegral $ tileSize)
    -- The dimensions of the area covered by the tree will be a square with dimensions the smallest power of two
    -- the contains both sides of the canvas. This is not a problem because the incoming shapes will still be excluded
    -- based on the dimensions of the canvas and empty tiles will just have their threads inactive.
    box = sizeToBox $ makePoint (2 ^ canvasDepth) (2 ^ canvasDepth)
    -- split the tile by dividing into a vertical stack
    goV depth box =
      let vIntCut = box ^. topSide + (2 ^ (depth - 1))
          vCut = fromIntegral vIntCut
      in  if depth > tileDepth
          then VTree vCut (goH depth (set bottomSide vIntCut box))
                          (goH depth (set topSide    vIntCut box))
          else VLeaf (Tile box, emptyRep)
    -- split the tile by dividing into a horizontal row.
    goH depth box =
      let hIntCut = box ^. leftSide + (2 ^ (depth - 1))
          hCut = fromIntegral hIntCut
      in  if depth > tileDepth
          then HTree hCut (goV (depth - 1) (set rightSide hIntCut box))
                          (goV (depth - 1) (set leftSide  hIntCut box))
          else HLeaf (Tile box, emptyRep)

buildTileTreeM :: Monad m => Point2 PixelSpace -> PixelSpace -> m a -> m (TileTree (Tile, a))
buildTileTreeM canvasSize tileSize emptyRep = goV canvasDepth box
    where
    -- Choose the largest dimension of the canvas as the square side dimension of the area covered by the tileTree.
    maxCanvasDimension = max (fromAlong Horizontal $ canvasSize ^. pX) (fromAlong Vertical $ canvasSize ^. pY)
    -- Canvas depth is the adjusted log2 of the larges side of the canvas.
    canvasDepth = adjustedLog (fromIntegral maxCanvasDimension)
    -- Initial tile depth is the adjusted log of the max tileSize
    tileDepth   = adjustedLog (fromIntegral $ tileSize)
    -- The dimensions of the area covered by the tree will be a square with dimensions the smallest power of two
    -- the contains both sides of the canvas. This is not a problem because the incoming shapes will still be excluded
    -- based on the dimensions of the canvas and empty tiles will just have their threads inactive.
    box = sizeToBox $ makePoint (2 ^ canvasDepth) (2 ^ canvasDepth)
    -- split the tile by dividing into a vertical stack
    goV depth box =
      let vIntCut = box ^. topSide + (2 ^ (depth - 1))
          vCut = fromIntegral vIntCut
      in  if depth > tileDepth
          then VTree vCut <$> (goH depth (set bottomSide vIntCut box))
                          <*> (goH depth (set topSide    vIntCut box))
          else do rep <- emptyRep
                  return $ VLeaf (Tile box, rep)
    -- split the tile by dividing into a horizontal row.
    goH depth box =
      let hIntCut = box ^. leftSide + (2 ^ (depth - 1))
          hCut = fromIntegral hIntCut
      in  if depth > tileDepth
          then HTree hCut <$> (goV (depth - 1) (set rightSide hIntCut box))
                          <*> (goV (depth - 1) (set leftSide  hIntCut box))
          else do rep <- emptyRep
                  return $ HLeaf (Tile box, rep)

-- | Add an itemEntry to a sequence of entries.
insertItemTagId :: ItemTagId -> (Tile, S.Seq ItemTagId) -> (Tile, S.Seq ItemTagId)
insertItemTagId itemEntry (tile, items) = (tile, items |> itemEntry)

addItemTagIdToTree :: TileTree (Tile, S.Seq ItemTagId) -> Box SubSpace -> ItemTagId -> TileTree (Tile, S.Seq ItemTagId)
addItemTagIdToTree tree box itemTagId = addItemToTree (insertItemTagId itemTagId) tree box

-- | Add an itemEntry to a tile tree.
addItemToTree :: (a -> a) -> TileTree a -> Box SubSpace -> TileTree a
addItemToTree f tree = insertItemV f tree

-- | Add a shape to an HTree
insertItemH :: (a -> a) -> HTree a -> Box SubSpace -> HTree a
insertItemH f (HTree cut left right) box =
    let left'  = -- if the left side of the shape is left of the cut add it to the left branch
                 if box ^. leftSide < cut
                 then insertItemV f left box
                 else left
        right' = -- if the right side of the shape is right of the cut add it to the right branch
                 if box ^. rightSide > cut
                 then insertItemV f right box
                 else right
    in  HTree cut left' right'
insertItemH f (HLeaf leaf) box =
    HLeaf $ f leaf

insertItemV :: (a -> a) -> VTree a -> Box SubSpace -> VTree a
insertItemV f (VTree cut top bottom) box =
    let top'    = if box ^. topSide < cut
                  then insertItemH f top box
                  else top
        bottom' = if box ^. bottomSide > cut
                  then insertItemH f bottom box
                  else bottom
    in  VTree cut top' bottom'
insertItemV f (VLeaf leaf) box =
    VLeaf $ f leaf

-- | Add an itemEntry to a sequence of entries.
insertItemTagIdPile :: MonadIO m => ItemTagId -> (Tile, Pile ItemTagId) -> m (Tile, Pile ItemTagId)
insertItemTagIdPile itemEntry (tile, items) =
  do (items', _) <- liftIO $ addToPile items itemEntry
     return (tile, items')

addItemTagIdToTreePile :: MonadIO m => TileTree (Tile, Pile ItemTagId) -> Box SubSpace -> ItemTagId -> m (TileTree (Tile, Pile ItemTagId))
addItemTagIdToTreePile tree box itemTagId = addItemToTreeM (insertItemTagIdPile itemTagId) tree box

-- | Add an itemEntry to a tile tree.
addItemToTreeM :: Monad m => (a -> m a) -> TileTree a -> Box SubSpace -> m (TileTree a)
addItemToTreeM f tree = insertItemVM f tree

-- | Add a shape to an HTree
insertItemHM :: Monad m => (a -> m a) -> HTree a -> Box SubSpace -> m (HTree a)
insertItemHM f (HTree cut left right) box =
    let left'  = -- if the left side of the shape is left of the cut add it to the left branch
                 if box ^. leftSide < cut
                 then insertItemVM f left box
                 else return left
        right' = -- if the right side of the shape is right of the cut add it to the right branch
                 if box ^. rightSide > cut
                 then insertItemVM f right box
                 else return right
    in  HTree cut <$> left' <*> right'
insertItemHM f (HLeaf leaf) box =
    HLeaf <$> f leaf

insertItemVM :: Monad m => (a -> m a) -> VTree a -> Box SubSpace -> m (VTree a)
insertItemVM f (VTree cut top bottom) box =
    let top'    = if box ^. topSide < cut
                  then insertItemHM f top box
                  else return top
        bottom' = if box ^. bottomSide > cut
                  then insertItemHM f bottom box
                  else return bottom
    in  VTree cut <$> top' <*> bottom'
insertItemVM f (VLeaf leaf) box =
    VLeaf <$> f leaf


-- | Display the contents of a tile.
showTile :: (t -> String) -> Tile -> t -> String
showTile f tile rep =   show (widthOf $ tile ^. tileBox)
                      ++ "X" ++ show (heightOf $ tile ^. tileBox)
                      ++ " rep " ++ f rep

-- | Display a TileTree by converting it first to a data tree and drawing it.
toDataTreeH f (HLeaf (tile, rep)) = Node (showTile f tile rep) []
toDataTreeH f (HTree hCut left right) = Node ("H" ++ show hCut) [toDataTreeV f left, toDataTreeV f right]

toDataTreeV f (VLeaf (tile, rep)) = Node (showTile f tile rep) []
toDataTreeV f (VTree vCut top bottom) = Node ("V" ++ show vCut) [toDataTreeH f top, toDataTreeH f bottom]

-- | Locate a point query in a tile tree.
locatePointInTileTree :: TileTree leaf -> Point2 SubSpace -> leaf
locatePointInTileTree = locatePointInVTree

locatePointInVTree :: VTree leaf -> Point2 SubSpace -> leaf
locatePointInVTree (VTree vCut top bottom) point =
  if point ^. pY <= vCut
  then locatePointInHTree top    point
  else locatePointInHTree bottom point
locatePointInVTree (VLeaf leaf) point = leaf

locatePointInHTree :: HTree leaf -> Point2 SubSpace -> leaf
locatePointInHTree (HTree hCut left right) point =
  if point ^. pX <= hCut
  then locatePointInVTree left  point
  else locatePointInVTree right point
locatePointInHTree (HLeaf leaf) point = leaf

-- | Fold a TileTree with a monadic function.
foldMapTileTree :: Monad m => (leaf -> m t) -> TileTree leaf -> m t
foldMapTileTree = foldMapTileTreeV

foldMapTileTreeH :: Monad m => (leaf -> m t) -> HTree leaf -> m t
foldMapTileTreeH f (HTree _ left right) = do foldMapTileTreeV f left
                                             foldMapTileTreeV f right
foldMapTileTreeH f (HLeaf leaf) = f leaf

foldMapTileTreeV :: Monad m => (leaf -> m t) -> VTree leaf -> m t
foldMapTileTreeV f (VTree _ top bottom) = do foldMapTileTreeH f top
                                             foldMapTileTreeH f bottom
foldMapTileTreeV f (VLeaf leaf) = f leaf

-- | Traverse a TileTree with a monadic function.
traverseTileTree :: Monad m => (a -> m b) -> TileTree a -> m (TileTree b)
traverseTileTree = traverseTileTreeV

traverseTileTreeH :: Monad m => (a -> m b) -> HTree a -> m (HTree b)
traverseTileTreeH f (HTree hCut left right) = do left'  <- traverseTileTreeV f left
                                                 right' <- traverseTileTreeV f right
                                                 return (HTree hCut left' right')
traverseTileTreeH f (HLeaf leaf) = HLeaf <$> f leaf

traverseTileTreeV :: Monad m => (a -> m b) -> VTree a -> m (VTree b)
traverseTileTreeV f (VTree vCut top bottom) = do top'    <- traverseTileTreeH f top
                                                 bottom' <- traverseTileTreeH f bottom
                                                 return (VTree vCut top' bottom')
traverseTileTreeV f (VLeaf leaf) = VLeaf <$> f leaf

-- | Instances
instance Show a => Show (HTree (Tile, S.Seq a)) where
  show = drawTree . toDataTreeH (show . S.length)

instance Show a => Show (VTree (Tile, S.Seq a)) where
  show = drawTree . toDataTreeV (show . S.length)

instance StorableM Tile where
  sizeOfM _ = do sizeOfM (undefined :: Box PixelSpace)
  alignmentM _ = do alignmentM (undefined :: Box PixelSpace)
  peekM = do box    <- peekM
             return (Tile box)
  pokeM (Tile box) =
          do pokeM box

instance Storable Tile where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance Storable TileId where
  sizeOf (TileId a) = sizeOf a
  alignment (TileId a) = alignment a
  peek i = TileId <$> peek (castPtr i)
  poke i (TileId a) = poke (castPtr i) a
