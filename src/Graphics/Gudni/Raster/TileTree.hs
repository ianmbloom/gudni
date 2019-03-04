{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Gudni.Raster.TileTree
  ( TileTree(..)
  , Tile(..)
  , buildTileTree
  , addPrimToTree
  , tileTreeToList
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

type Width  = Ortho XDimension DisplaySpace
type Height = Ortho YDimension DisplaySpace
type Size   = Int

data Tile = Tile
    { tilePrims       :: [Shaper PrimEntry]
    , tileStrandCount :: NumStrands
    , tilePrimCount   :: Size
    , tileBox         :: Box IntSpace
    } deriving (Show)

type TileTree = HTree

data HTree = HTree
    { hCut        :: Width
    , leftBranch  :: VTree
    , rightBranch :: VTree
    }
    | HLeaf Tile

data VTree = VTree
    { vCut         :: Height
    , topBranch    :: HTree
    , bottomBranch :: HTree
    }
    | VLeaf Tile

buildTileTree :: Point2 IntSpace -> HTree
buildTileTree = buildTileTree' (fromIntegral $ mAXtILEsIZE ^. pX)

adjustedLog :: IntSpace -> Int
adjustedLog x = if x < 1 then 0 else ceiling . logBase 2 . fromIntegral $ x

buildTileTree' :: IntSpace -> Point2 IntSpace -> HTree
buildTileTree' tileSize canvasSize = goH canvasDepth $ tr "box" box
    where
    maxCanvasDimension = max (unOrtho $ canvasSize ^. pX) (unOrtho $ canvasSize ^. pY)
    canvasDepth = tr "logWidth"  $ adjustedLog maxCanvasDimension
    tileDepth = tr "logWidth"  $ adjustedLog tileSize
    box = pointToBox $ makePoint (2 ^ canvasDepth) (2 ^ canvasDepth)
    goH depth box =
      let hIntCut = box ^. leftSide + (2 ^ (depth - 1))
          hCut = fromIntegral hIntCut
      in  if depth > tileDepth
          then HTree hCut (goV depth (set rightSide hIntCut box))
                          (goV depth (set leftSide  hIntCut box))
          else HLeaf $ emptyTile box
    goV depth box =
      let vIntCut = box ^. topSide + (2 ^ (depth - 1))
          vCut = fromIntegral vIntCut
      in  VTree vCut (goH (depth - 1) (set bottomSide vIntCut box))
                     (goH (depth - 1) (set topSide    vIntCut box))

addPrimToTree :: HTree -> Shaper PrimEntry -> HTree
addPrimToTree = insertPrimH

emptyTile :: Box IntSpace -> Tile
emptyTile box = Tile [] 0 0 box

insertPrimH :: HTree -> Shaper PrimEntry -> HTree
insertPrimH (HTree cut left right) primEntry =
    let left'  = if primEntry ^. shRep . primBox . leftSide < cut
                 then insertPrimV left primEntry
                 else left
        right' = if primEntry ^. shRep .primBox . rightSide > cut
                 then insertPrimV right primEntry
                 else right
    in  HTree cut left' right'
insertPrimH (HLeaf tile) primEntry =
  if checkTileSpace tile primEntry || widthBox (tileBox tile) <= mINtILEsIZE ^. pX
  then HLeaf $ insertPrimTile tile primEntry
  else insertPrimH (hSplit tile) primEntry

insertPrimV :: VTree -> Shaper PrimEntry -> VTree
insertPrimV (VTree cut top bottom) primEntry =
    let top'    = if primEntry ^. shRep . primBox . topSide < cut
                  then insertPrimH top primEntry
                  else top
        bottom' = if primEntry ^. shRep . primBox . bottomSide > cut
                  then insertPrimH bottom primEntry
                  else bottom
    in  VTree cut top' bottom'
insertPrimV (VLeaf tile) primEntry =
    if checkTileSpace tile primEntry || heightBox (tileBox tile) <= mINtILEsIZE ^. pY
    then VLeaf $ insertPrimTile tile primEntry
    else insertPrimV (vSplit tile) primEntry

insertPrimTile :: Tile -> Shaper PrimEntry -> Tile
insertPrimTile tile primEntry =
  Tile { tilePrims = primEntry:tilePrims tile
       , tileStrandCount = tileStrandCount tile + primEntry ^. shRep . primStrandCount
       , tilePrimCount = tilePrimCount tile + 1
       , tileBox = tileBox tile
       }

checkTileSpace :: Tile -> Shaper PrimEntry -> Bool
checkTileSpace tile primEntry = tilePrimCount tile < mAXsHAPE && (tileStrandCount tile + (primEntry ^. shRep . primStrandCount)) < (NumStrands . fromIntegral $ mAXsTRANDpERtILE)

hSplit :: Tile -> HTree
hSplit tile =
  let box = tileBox tile
      cut = box ^. leftSide + (widthBox box `div` 2)
      lBox = set rightSide cut box
      rBox = set leftSide  cut box
      lTile = emptyTile lBox
      rTile = emptyTile rBox
      hTree = HTree (fromIntegral cut) (VLeaf lTile) (VLeaf rTile)
  in  {-tr "hSplit" $-} foldl insertPrimH hTree (tilePrims tile)

vSplit :: Tile -> VTree
vSplit tile =
  let box = tileBox tile
      cut = box ^. topSide + (heightBox box `div` 2)
      tBox = set bottomSide cut box
      bBox = set topSide    cut box
      tTile = emptyTile tBox
      bTile = emptyTile bBox
      vTree = VTree (fromIntegral cut) (HLeaf tTile) (HLeaf bTile)
  in  {-tr "vSplit" $-} foldl insertPrimV vTree (tilePrims tile)

tileTreeToList :: TileTree -> [Tile]
tileTreeToList = tileTreeToListH

tileTreeToListH :: HTree -> [Tile]
tileTreeToListH (HTree _ left right) = tileTreeToListV left ++ tileTreeToListV right
tileTreeToListH (HLeaf tile) = pure tile

tileTreeToListV :: VTree -> [Tile]
tileTreeToListV (VTree _ top bottom) = tileTreeToListH top ++ tileTreeToListH bottom
tileTreeToListV (VLeaf tile) = pure tile

showTile tile = "Shapes " ++ show (tilePrimCount tile) ++ " Strands " ++ show (tileStrandCount tile)
toDataTreeH (HLeaf tile) = Node (showTile tile) []
toDataTreeH (HTree hCut left right) = Node ("H" ++ show hCut) [toDataTreeV left, toDataTreeV right]

toDataTreeV (VLeaf tile) = Node (showTile tile) []
toDataTreeV (VTree vCut top bottom) = Node ("V" ++ show vCut) [toDataTreeH top, toDataTreeH bottom]

instance Show TileTree where
  show = drawTree . toDataTreeH

instance Show VTree where
  show = drawTree . toDataTreeV
