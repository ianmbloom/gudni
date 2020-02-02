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

module Graphics.Gudni.Raster.TileTree
  ( TileTree(..)
  -- , TileEntry(..)
  , ItemEntry(..)
  , itemEntryTagId
  , itemStrandCount
  , itemBox
  --, tileItems
  , buildTileTree
  , addItemToTree
  , Tile (..)
  , TileId(..)
  , tileBox
  , tileHDepth
  , tileVDepth
  , locatePointInTileTree
  , foldMapTileTree
  , traverseTileTree
  )
where

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.GeoReference
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM

import Control.Lens hiding ((|>),(<|))
import Control.DeepSeq
import Control.Monad.State

import Data.Tree
import qualified Data.Sequence as S
import Data.Sequence ((|>),(<|))
import Foreign.C.Types(CShort,CInt)
import Foreign.Ptr

type TileId_ = Int
newtype TileId = TileId {unTileId :: TileId_} deriving (Show, Eq, Ord, Num)

-- | A wrapper for ItemTags
data ItemEntry = ItemEntry
    { _itemEntryTagId  :: ItemTagId
    , _itemStrandCount :: NumStrands
    , _itemBox         :: BoundingBox
    } deriving (Show)
makeLenses ''ItemEntry

newtype PointQueryId = PointQueryId {unPointQueryId :: Int} deriving (Show, Eq, Ord)

-- | Tile is just a pairing of the Tile Info Header and some representation of its contents.
data Tile = Tile
  { -- | Pixel boundaries of tile.
    _tileBox    :: !(Box PixelSpace)
    -- | Logarithmic horizontal depth.
  , _tileHDepth :: !Int
    -- | Logarithmic vertical depth.
  , _tileVDepth :: !Int
  } deriving (Show)
makeLenses ''Tile

-- | A TileTree is a XY partition tree for dividing the canvas into tiles.
type TileTree leaf = VTree leaf

-- | HTrees divide the space into two horiztonally adjacent partitions
-- left and right of the cut line.
data HTree leaf = HTree
    { hCut        :: SubSpace
    , leftBranch  :: VTree leaf
    , rightBranch :: VTree leaf
    }
    | HLeaf leaf

-- | VTrees divide the space into two vertically adjacent partitions
-- above and below the cut line.
data VTree leaf = VTree
    { vCut         :: SubSpace
    , topBranch    :: HTree leaf
    , bottomBranch :: HTree leaf
    }
    | VLeaf leaf

buildTileTree :: a -> PixelSpace -> Point2 PixelSpace -> TileTree (Tile, a)
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
          else VLeaf (Tile box depth depth, emptyRep)
    -- split the tile by dividing into a horizontal row.
    goH depth box =
      let hIntCut = box ^. leftSide + (2 ^ (depth - 1))
          hCut = fromIntegral hIntCut
      in  if depth > tileDepth
          then HTree hCut (goV (depth - 1) (set rightSide hIntCut box))
                          (goV (depth - 1) (set leftSide  hIntCut box))
          else HLeaf (Tile box depth depth, emptyRep)

-- | Add an itemEntry to a sequence of entries.
insertItem :: (Tile, S.Seq ItemEntry) -> ItemEntry -> (Tile, S.Seq ItemEntry)
insertItem (tile, items) itemEntry = (tile, items |> itemEntry)

-- | Add an itemEntry to a tile tree.
addItemToTree :: TileTree (Tile, S.Seq ItemEntry) -> ItemEntry -> TileTree (Tile, S.Seq ItemEntry)
addItemToTree tree = insertItemV tree

-- | Add a shape to an HTree
insertItemH :: HTree (Tile, S.Seq ItemEntry) -> ItemEntry -> HTree (Tile, S.Seq ItemEntry)
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
insertItemH (HLeaf leaf) itemEntry =
    HLeaf $ insertItem leaf itemEntry

insertItemV :: VTree (Tile, S.Seq ItemEntry) -> ItemEntry -> VTree (Tile, S.Seq ItemEntry)
insertItemV (VTree cut top bottom) itemEntry =
    let top'    = if itemEntry ^. itemBox . topSide < cut
                  then insertItemH top itemEntry
                  else top
        bottom' = if itemEntry ^. itemBox . bottomSide > cut
                  then insertItemH bottom itemEntry
                  else bottom
    in  VTree cut top' bottom'
insertItemV (VLeaf leaf) itemEntry =
    VLeaf $ insertItem leaf itemEntry


-- | Display the contents of a tile.
showTile tile rep = " (" ++ show (widthOf $ tile ^. tileBox)
                  ++ "X" ++ show (heightOf $ tile ^. tileBox)
                  ++ " rep " ++ show rep

-- | Display a TileTree by converting it first to a data tree and drawing it.
toDataTreeH (HLeaf (tile, rep)) = Node (showTile tile rep) []
toDataTreeH (HTree hCut left right) = Node ("H" ++ show hCut) [toDataTreeV left, toDataTreeV right]

toDataTreeV (VLeaf (tile, rep)) = Node (showTile tile rep) []
toDataTreeV (VTree vCut top bottom) = Node ("V" ++ show vCut) [toDataTreeH top, toDataTreeH bottom]

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
instance Show a => Show (HTree (Tile, a)) where
  show = drawTree . toDataTreeH

instance Show a => Show (VTree (Tile, a)) where
  show = drawTree . toDataTreeV

instance StorableM Tile where
  sizeOfM _ = do sizeOfM (undefined :: Box PixelSpace)
                 sizeOfM (undefined :: CShort)
                 sizeOfM (undefined :: CShort)
  alignmentM _ = do alignmentM (undefined :: Box PixelSpace)
                    alignmentM (undefined :: CShort)
                    alignmentM (undefined :: CShort)
  peekM = do box    <- peekM
             hDepth :: CShort <- peekM
             vDepth :: CShort <- peekM
             return (Tile box (fromIntegral hDepth) (fromIntegral vDepth))
  pokeM (Tile box hDepth vDepth) =
          do pokeM box
             pokeM (fromIntegral hDepth :: CShort)
             pokeM (fromIntegral vDepth :: CShort)

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

instance NFData Tile where
  rnf (Tile a b c) = a `deepseq` b `deepseq` c `deepseq` ()

instance NFData (TileId) where
  rnf (TileId a) = a `deepseq`  ()
