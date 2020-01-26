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
  , EntrySequence(..)
  , unEntrySequence
  -- , TileEntry(..)
  , ItemEntry(..)
  , itemEntryTag
  , itemStrandCount
  , itemBox
  --, tileItems
  , buildTileTree
  , addItemToTree
  , foldMapTileTree
  , traverseTileTree
  , adjustedLog
  , splitTreeTiles
  , Tile (..)
  , TileId(..)
  , tileBox
  , tileHDepth
  , tileVDepth
  , tileRep
  , locatePointInTileTree
  )
where

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.GeoReference
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM

import Control.Lens
import Control.DeepSeq
import Control.Monad.State

import Data.Tree
import qualified Data.Sequence as S
import Data.Sequence ((|>),(<|))
import Foreign.C.Types(CShort,CInt)
import Foreign.Ptr

type Width  = SubSpace
type Height = SubSpace
type Size   = Int

type TileId_ = Int
newtype TileId = TileId {unTileId :: TileId_} deriving (Show, Eq, Ord, Num)

-- | A wrapper for ItemTags
data ItemEntry = ItemEntry
    { _itemEntryTag    :: ItemTag
    , _itemStrandCount :: NumStrands
    , _itemBox         :: BoundingBox
    } deriving (Show)
makeLenses ''ItemEntry

newtype PointQueryId = PointQueryId {unPointQueryId :: Int} deriving (Show, Eq, Ord)
newtype EntrySequence = EntrySequence
  { _unEntrySequence :: S.Seq ItemEntry
  }
makeLenses ''EntrySequence


instance Show EntrySequence where
  show (EntrySequence ss) = "EntrySequence " ++ (show . S.length $ ss) ++ " totalStrands " ++ show (sum (fmap (view itemStrandCount) ss))

-- -- | A tile entry is the intermediate storage for the contents of a tile.
-- data TileEntry = TileEntry
--     { _tileItems      :: EntrySequence
--     , _tilePoints     :: PointSequence
--     } deriving (Show)
-- makeLenses ''TileEntry

-- | Tile is just a pairing of the Tile Info Header and some representation of its contents.
data Tile rep = Tile
  { -- | Pixel boundaries of tile.
    _tileBox    :: !(Box PixelSpace)
    -- | Logarithmic horizontal depth.
  , _tileHDepth :: !Int
    -- | Logarithmic vertical depth.
  , _tileVDepth :: !Int
    -- | Representation of the contents of the tile.
  , _tileRep :: rep
  } deriving (Show)
makeLenses ''Tile

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
insertItemTile tile itemEntry = over (tileRep . unEntrySequence) (flip (S.|>) itemEntry) tile

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
      lEmpty = emptyTile (EntrySequence S.empty) (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set rightSide cut (tile ^. tileBox))
      rEmpty = emptyTile (EntrySequence S.empty) (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set leftSide cut (tile ^. tileBox))
      hTree = HTree (fromIntegral cut) (VLeaf lEmpty) (VLeaf rEmpty)
  in  foldl insertItemH hTree $ (tile ^. tileRep . unEntrySequence)

vSplit :: Tile EntrySequence -> VTree EntrySequence
vSplit tile =
  let cut = tile ^. tileBox . topSide + (heightOf (tile ^. tileBox) `div` 2)
      tEmpty = emptyTile (EntrySequence S.empty) (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set bottomSide cut (tile ^. tileBox))
      bEmpty = emptyTile (EntrySequence S.empty) (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set topSide    cut (tile ^. tileBox))
      vTree = VTree (fromIntegral cut) (HLeaf tEmpty) (HLeaf bEmpty)
  in  foldl insertItemV vTree $ (tile ^. tileRep . unEntrySequence)

shouldSplitV :: NumStrands -> Tile EntrySequence -> Bool
shouldSplitV maxThresholds tile =
   heightOf (tile ^. tileBox) > mINtILEsIZE ^. pY &&
   (length (tile ^. tileRep . unEntrySequence) > mAXlAYERS ||
   sum (fmap (view itemStrandCount) (tile ^. tileRep . unEntrySequence)) > maxThresholds)

shouldSplitH :: NumStrands -> Tile EntrySequence -> Bool
shouldSplitH maxThresholds tile =
   widthOf (tile ^. tileBox) > mINtILEsIZE ^. pX &&
   (length (tile ^. tileRep . unEntrySequence) > mAXlAYERS ||
   sum (fmap (view itemStrandCount) (tile ^. tileRep . unEntrySequence)) > maxThresholds)

-- | Traverse a TileTree with a monadic function.
foldMapTileTree :: Monad m => (Tile a -> m t) -> TileTree a -> m t
foldMapTileTree = foldMapTileTreeV

foldMapTileTreeH :: Monad m => (Tile a -> m t) -> HTree a -> m t
foldMapTileTreeH f (HTree _ left right) = do foldMapTileTreeV f left
                                             foldMapTileTreeV f right
foldMapTileTreeH f (HLeaf tile) = f tile

foldMapTileTreeV :: Monad m => (Tile a -> m t) -> VTree a -> m t
foldMapTileTreeV f (VTree _ top bottom) = do foldMapTileTreeH f top
                                             foldMapTileTreeH f bottom
foldMapTileTreeV f (VLeaf tile) = f tile

-- | Traverse a TileTree with a monadic function.
traverseTileTree :: Monad m => (Tile a -> m (Tile b)) -> TileTree a -> m (TileTree b)
traverseTileTree = traverseTileTreeV

traverseTileTreeH :: Monad m => (Tile a -> m (Tile b)) -> HTree a -> m (HTree b)
traverseTileTreeH f (HTree hCut left right) = do left'  <- traverseTileTreeV f left
                                                 right' <- traverseTileTreeV f right
                                                 return (HTree hCut left' right')
traverseTileTreeH f (HLeaf tile) = HLeaf <$> f tile

traverseTileTreeV :: Monad m => (Tile a -> m (Tile b)) -> VTree a -> m (VTree b)
traverseTileTreeV f (VTree vCut top bottom) = do top'    <- traverseTileTreeH f top
                                                 bottom' <- traverseTileTreeH f bottom
                                                 return (VTree vCut top' bottom')
traverseTileTreeV f (VLeaf tile) = VLeaf <$> f tile

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

instance StorableM (Tile (Slice ItemTag, Int)) where
  sizeOfM _ = do sizeOfM (undefined :: Box PixelSpace)
                 sizeOfM (undefined :: CShort)
                 sizeOfM (undefined :: CShort)
                 sizeOfM (undefined :: CInt)
                 sizeOfM (undefined :: Slice (Shape GeoReference))
  alignmentM _ = do alignmentM (undefined :: Box PixelSpace)
                    alignmentM (undefined :: CShort)
                    alignmentM (undefined :: CShort)
                    alignmentM (undefined :: CInt)
                    alignmentM (undefined :: Slice (Shape GeoReference))
  peekM = do box    <- peekM
             hDepth :: CShort <- peekM
             vDepth :: CShort <- peekM
             columnAllocation :: CInt <- peekM
             slice  <- peekM
             return (Tile box (fromIntegral hDepth) (fromIntegral vDepth) (slice, fromIntegral columnAllocation))
  pokeM (Tile box hDepth vDepth (slice, columnAllocation)) =
          do pokeM box
             pokeM (fromIntegral hDepth :: CShort)
             pokeM (fromIntegral vDepth :: CShort)
             pokeM (fromIntegral columnAllocation :: CInt)
             pokeM slice

instance Storable (Tile (Slice ItemTag, Int)) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance Storable TileId where
  sizeOf (TileId a) = sizeOf a
  alignment (TileId a) = alignment a
  peek i = TileId <$> peek (castPtr i)
  poke i (TileId a) = poke (castPtr i) a

instance NFData rep => NFData (Tile rep) where
  rnf (Tile a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()

instance NFData (TileId) where
  rnf (TileId a) = a `deepseq`  ()

locatePointInTileTree :: TileTree a -> Point2 SubSpace -> a
locatePointInTileTree = locatePointInVTree

locatePointInVTree :: VTree a -> Point2 SubSpace -> a
locatePointInVTree (VTree vCut top bottom) point =
  if point ^. pY <= vCut
  then locatePointInHTree top    point
  else locatePointInHTree bottom point
locatePointInVTree (VLeaf tile) point = tile ^. tileRep

locatePointInHTree :: HTree a -> Point2 SubSpace -> a
locatePointInHTree (HTree hCut left right) point =
  if point ^. pX <= hCut
  then locatePointInVTree left  point
  else locatePointInVTree right point
locatePointInHTree (HLeaf tile) point = tile ^. tileRep
