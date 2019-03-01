module Graphics.Gudni.Raster.TileTree
  ( TileTree(..)
  , Tile(..)
  , buildTileTree
  , addPrimToTree
  , tileTreeToGroups
  )
where

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Raster.Enclosure

import Control.Lens

type Width  = Ortho XDimension DisplaySpace
type Height = Ortho YDimension DisplaySpace
type Size   = Int

type TileTree = HTree
data HTree = HTree
    { hCut        :: Width
    , leftBranch  :: VTree
    , rightBranch :: VTree
    }
    | HLeaf Tile
    deriving (Show)

data VTree = VTree
    { vCut         :: Height
    , topBranch    :: HTree
    , bottomBranch :: HTree
    }
    | VLeaf Tile
    deriving (Show)

data Tile = Tile
    { tilePrims       :: [PrimEntry]
    , tileStrandCount :: NumStrands
    , tilePrimCount   :: Size
    , tileBox         :: BoundingBox
    } deriving (Show)

buildTileTree :: Point2 DisplaySpace -> HTree
buildTileTree = buildTileTree' (fromIntegral <$> mAXtILEsIZE)

buildTileTree' :: Point2 DisplaySpace -> Point2 DisplaySpace -> HTree
buildTileTree' tileSize canvasSize = goH maxDepth (pointToBox canvasSize)
    where
    gridW = canvasSize ^. pX / tileSize ^. pX
    gridH = canvasSize ^. pY / tileSize ^. pY
    logWidth  = if gridW < 1 then 0 else ceiling . logBase 2 $ gridW
    logHeight = if gridH < 1 then 0 else ceiling . logBase 2 $ gridH
    maxDepth  = max logWidth logHeight
    goH depth box =
      let hCut = max (canvasSize ^. pX) (box ^. leftSide + fromIntegral (2 ^ (depth - 1)))
      in  if depth > 0
          then HTree hCut (goV depth (set rightSide hCut box))
                          (goV depth (set leftSide  hCut box))
          else HLeaf $ emptyTile box
    goV depth box =
      let vCut = max (canvasSize ^. pY) (box ^. topSide + fromIntegral (2 ^ (depth - 1)))
      in  VTree vCut (goH (depth - 1) (set bottomSide vCut box))
                     (goH (depth - 1) (set topSide    vCut box))

addPrimToTree :: HTree -> PrimEntry -> HTree
addPrimToTree = insertPrimH

emptyTile :: BoundingBox -> Tile
emptyTile box = Tile [] 0 0 box

insertPrimH :: HTree -> PrimEntry -> HTree
insertPrimH (HTree cut left right) primEntry =
    let left'  = if (primBox primEntry) ^. leftSide < cut
                 then insertPrimV left primEntry
                 else left
        right' = if (primBox primEntry) ^. rightSide > cut
                 then insertPrimV right primEntry
                 else right
    in  HTree cut left' right'
insertPrimH (HLeaf tile) primEntry =
  if checkTileSpace tile primEntry
  then HLeaf $ insertPrimTile tile primEntry
  else insertPrimH (hSplit tile) primEntry

insertPrimV :: VTree -> PrimEntry -> VTree
insertPrimV (VTree cut top bottom) primEntry =
    let top'    = if (primBox primEntry)^. topSide < cut
                  then insertPrimH top primEntry
                  else top
        bottom' = if (primBox primEntry) ^. bottomSide > cut
                  then insertPrimH bottom primEntry
                  else bottom
    in  VTree cut top' bottom'
insertPrimV (VLeaf tile) primEntry =
    if checkTileSpace tile primEntry
    then VLeaf $ insertPrimTile tile primEntry
    else insertPrimV (vSplit tile) primEntry

insertPrimTile :: Tile -> PrimEntry -> Tile
insertPrimTile tile primEntry =
  Tile { tilePrims = primEntry:tilePrims tile
       , tileStrandCount = tileStrandCount tile + primStrandCount primEntry
       , tilePrimCount = tilePrimCount tile + 1
       , tileBox = tileBox tile
       }

checkTileSpace :: Tile -> PrimEntry -> Bool
checkTileSpace tile primEntry = tilePrimCount tile < mAXsHAPE && (tileStrandCount tile + (primStrandCount primEntry)) < (NumStrands . fromIntegral $ mAXtHRESHOLDS)

hSplit :: Tile -> HTree
hSplit tile =
  let box = tileBox tile
      cut = box ^. leftSide + (widthBox box / 2)
      lBox = set rightSide cut box
      rBox = set leftSide  cut box
      lTile = emptyTile lBox
      rTile = emptyTile rBox
      hTree = HTree cut (VLeaf lTile) (VLeaf rTile)
  in  foldl insertPrimH hTree (tilePrims tile)

vSplit :: Tile -> VTree
vSplit tile =
  let box = tileBox tile
      cut = box ^. topSide + (heightBox box / 2)
      tBox = set bottomSide cut box
      bBox = set topSide    cut box
      tTile = emptyTile tBox
      bTile = emptyTile bBox
      vTree = VTree cut (HLeaf tTile) (HLeaf bTile)
  in  foldl insertPrimV vTree (tilePrims tile)

tileTreeToGroups :: Int -> TileTree -> [[Tile]]
tileTreeToGroups groupSize = map fst . tileTreeToGroupsH groupSize

tileTreeToGroupsH :: Int -> HTree -> [([Tile],Int)]
tileTreeToGroupsH groupSize (HTree _ left right) =
  let leftJobs  = tileTreeToGroupsV groupSize left
      rightJobs = tileTreeToGroupsV groupSize right
      (leftTiles,  leftSize)  = head leftJobs
      (rightTiles, rightSize) = head rightJobs
      totalSize = leftSize + rightSize
  in  if totalSize > groupSize
      then if leftSize > rightSize
           then rightJobs ++ leftJobs
           else leftJobs ++ rightJobs
      else (leftTiles++rightTiles, totalSize):tail leftJobs ++ tail rightJobs
tileTreeToGroupsH groupSize (HLeaf tile) = [([tile],1)]

tileTreeToGroupsV :: Int -> VTree -> [([Tile],Int)]
tileTreeToGroupsV groupSize (VTree _ top bottom) =
  let topJobs    = tileTreeToGroupsH groupSize top
      bottomJobs = tileTreeToGroupsH groupSize bottom
      (topTiles,    topSize)  = head topJobs
      (bottomTiles, bottomSize) = head bottomJobs
      totalSize = topSize + bottomSize
  in  if totalSize > groupSize
      then if topSize > bottomSize
           then bottomJobs ++ topJobs
           else topJobs ++ bottomJobs
      else (topTiles++bottomTiles, totalSize):tail topJobs ++ tail bottomJobs
tileTreeToGroupsV groupSize (VLeaf tile) = [([tile],1)]
