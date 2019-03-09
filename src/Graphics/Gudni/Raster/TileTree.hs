{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell      #-}

module Graphics.Gudni.Raster.TileTree
  ( TileTree(..)
  , TileEntry(..)
  , tileShapes
  , tileStrandCount
  , tileShapeCount
  , buildTileTree
  , addShapeToTree
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

data TileEntry = TileEntry
    { _tileShapes      :: [Shape ShapeEntry]
    , _tileStrandCount :: NumStrands
    , _tileShapeCount  :: Size
    } deriving (Show)
makeLenses ''TileEntry

type TileTree = VTree

data HTree = HTree
    { hCut        :: Width
    , leftBranch  :: VTree
    , rightBranch :: VTree
    }
    | HLeaf (Tile TileEntry)

data VTree = VTree
    { vCut         :: Height
    , topBranch    :: HTree
    , bottomBranch :: HTree
    }
    | VLeaf (Tile TileEntry)

buildTileTree :: Point2 IntSpace -> TileTree
buildTileTree = buildTileTree' (fromIntegral $ mAXtILEsIZE ^. pX)

adjustedLog :: (Integral s, Integral v )=> s -> v
adjustedLog x = if x < 1 then 0 else ceiling . logBase 2 . fromIntegral $ x

buildTileTree' :: IntSpace -> Point2 IntSpace -> TileTree
buildTileTree' tileSize canvasSize = goV canvasDepth box
    where
    maxCanvasDimension = max (unOrtho $ canvasSize ^. pX) (unOrtho $ canvasSize ^. pY)
    canvasDepth = adjustedLog maxCanvasDimension
    tileDepth   = adjustedLog tileSize
    box = pointToBox $ makePoint (2 ^ canvasDepth) (2 ^ canvasDepth)
    goH depth box =
      let hIntCut = box ^. leftSide + (2 ^ (depth - 1))
          hCut = fromIntegral hIntCut
      in  if depth > tileDepth
          then HTree hCut (goV (depth - 1) (set rightSide hIntCut box))
                          (goV (depth - 1) (set leftSide  hIntCut box))
          else HLeaf $ emptyTile depth depth box
    goV depth box =
      let vIntCut = box ^. topSide + (2 ^ (depth - 1))
          vCut = fromIntegral vIntCut
      in  if depth > tileDepth
          then VTree vCut (goH depth (set bottomSide vIntCut box))
                          (goH depth (set topSide    vIntCut box))
          else VLeaf $ emptyTile depth depth box

addShapeToTree :: TileTree -> Shape ShapeEntry -> TileTree
addShapeToTree tree = {-tr "result" .-} insertShapeV tree {-. tr "addShape"-}

emptyTile :: Int -> Int -> Box IntSpace -> Tile TileEntry
emptyTile hDepth vDepth box = Tile box hDepth vDepth (TileEntry [] 0 0)

insertShapeH :: HTree -> Shape ShapeEntry -> HTree
insertShapeH (HTree cut left right) shapeEntry =
    let left'  = if shapeEntry ^. shRep . shapeBox . leftSide < cut
                 then {-tr ("hLeft " ++ show cut ++ " insert") $-} insertShapeV left shapeEntry
                 else {-tr ("hLeft " ++ show cut ++ " ignore") $-} left
        right' = if shapeEntry ^. shRep . shapeBox . rightSide > cut
                 then {-tr ("hRight " ++ show cut ++ " insert") $-} insertShapeV right shapeEntry
                 else {-tr ("hRight " ++ show cut ++ " ignore") $-} right
    in  HTree cut left' right'
insertShapeH (HLeaf tile) shapeEntry =
  if checkTileSpace tile shapeEntry || widthBox (tile ^. tileBox) <= mINtILEsIZE ^. pX
  then {-tr "HLeaf insert" $-} HLeaf $ insertShapeTile tile shapeEntry
  else {-tr "HLeaf split"  $-} insertShapeH (hSplit tile) shapeEntry

insertShapeV :: VTree -> Shape ShapeEntry -> VTree
insertShapeV (VTree cut top bottom) shapeEntry =
    let top'    = if shapeEntry ^. shRep . shapeBox . topSide < cut
                  then {-tr ("vTop " ++ show cut ++ " insert") $-} insertShapeH top shapeEntry
                  else {-tr ("vTop " ++ show cut ++ " ignore") $-} top
        bottom' = if shapeEntry ^. shRep . shapeBox . bottomSide > cut
                  then {-tr ("vBot " ++ show cut ++ " insert") $-} insertShapeH bottom shapeEntry
                  else {-tr ("vBot " ++ show cut ++ " ignore") $-} bottom
    in  VTree cut top' bottom'
insertShapeV (VLeaf tile) shapeEntry =
    if checkTileSpace tile shapeEntry || heightBox (tile ^. tileBox) <= mINtILEsIZE ^. pY
    then {-tr "VLeaf insert" $-} VLeaf $ insertShapeTile tile shapeEntry
    else {-tr "VLeaf split"  $-} insertShapeV (vSplit tile) shapeEntry

insertShapeTile :: Tile TileEntry -> Shape ShapeEntry -> Tile TileEntry
insertShapeTile tile shapeEntry =
  let tileEntry = tile ^. tileRep
      newEntry = TileEntry { _tileShapes = shapeEntry:tileEntry ^. tileShapes
                           , _tileStrandCount = tileEntry ^. tileStrandCount + shapeEntry ^. shRep . shapeStrandCount
                           , _tileShapeCount = tileEntry ^. tileShapeCount + 1
                           }
  in  set tileRep newEntry tile

checkTileSpace :: Tile TileEntry -> Shape ShapeEntry -> Bool
checkTileSpace tile shapeEntry =
  let tileEntry = tile ^. tileRep
  in  tileEntry ^. tileShapeCount < mAXsHAPE && (tileEntry ^. tileStrandCount + (shapeEntry ^. shRep . shapeStrandCount)) < (NumStrands . fromIntegral $ mAXsTRANDpERtILE)

hSplit :: Tile TileEntry -> HTree
hSplit tile =
  let cut = tile ^. tileBox . leftSide + (widthBox (tile ^. tileBox) `div` 2)
      lEmpty = emptyTile (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set rightSide cut (tile ^. tileBox))
      rEmpty = emptyTile (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set leftSide cut (tile ^. tileBox))
      hTree = HTree (fromIntegral cut) (VLeaf lEmpty) (VLeaf rEmpty)
  in  {-tr "hSplit" $-} foldl insertShapeH hTree $ reverse (tile ^. tileRep . tileShapes)

vSplit :: Tile TileEntry -> VTree
vSplit tile =
  let cut = tile ^. tileBox . topSide + (heightBox (tile ^. tileBox) `div` 2)
      tEmpty = emptyTile (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set bottomSide cut (tile ^. tileBox))
      bEmpty = emptyTile (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set topSide    cut (tile ^. tileBox))
      vTree = VTree (fromIntegral cut) (HLeaf tEmpty) (HLeaf bEmpty)
  in  {-tr "vSplit" $-} foldl insertShapeV vTree $ reverse (tile ^. tileRep . tileShapes)

tileTreeToList :: TileTree -> [Tile TileEntry]
tileTreeToList = tileTreeToListV

tileTreeToListH :: HTree -> [Tile TileEntry]
tileTreeToListH (HTree _ left right) = tileTreeToListV left ++ tileTreeToListV right
tileTreeToListH (HLeaf tile) = pure tile

tileTreeToListV :: VTree -> [Tile TileEntry]
tileTreeToListV (VTree _ top bottom) = tileTreeToListH top ++ tileTreeToListH bottom
tileTreeToListV (VLeaf tile) = pure tile

showTile tile =   "Shapes " ++ show (tile ^. tileRep . tileShapeCount)
              ++ " Strands " ++ show (tile ^. tileRep . tileStrandCount)
              ++ " Shapes " ++ show (map (view shapeBox . view shRep) $ tile ^. tileRep . tileShapes)
toDataTreeH (HLeaf tile) = Node (showTile tile) []
toDataTreeH (HTree hCut left right) = Node ("H" ++ show hCut) [toDataTreeV left, toDataTreeV right]

toDataTreeV (VLeaf tile) = Node (showTile tile) []
toDataTreeV (VTree vCut top bottom) = Node ("V" ++ show vCut) [toDataTreeH top, toDataTreeH bottom]

instance Show HTree where
  show = drawTree . toDataTreeH

instance Show VTree where
  show = drawTree . toDataTreeV
