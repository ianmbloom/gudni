module Graphics.Gudni.Raster.TileTree
  ( buildTree
  )
where

import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree

import Control.Lens

type Width  = Ortho XDimension DisplaySpace
type Height = Ortho YDimension DisplaySpace
type Size   = Int

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
    , tileStrandCount :: Size
    , tilePrimCount   :: Size
    , tileBox         :: Box DisplaySpace
    } deriving (Show)

data PrimEntry = PrimEntry
    { primStrandCount :: Size
    , primId :: PrimId
    , primBox :: Box DisplaySpace
    } deriving (Show)

emptyTile :: Box DisplaySpace -> Tile
emptyTile box = Tile [] 0 0 box

buildTree :: Width -> Height -> Width -> Height -> HTree
buildTree tileW tileH w h = goH maxDepth 0 0
    where
    logWidth  = if w < 1 then 0 else ceiling . logBase 2 $ w / tileW
    logHeight = if h < 1 then 0 else ceiling . logBase 2 $ h / tileH
    maxDepth  = max logWidth logHeight
    goH depth xOffset yOffset =
      let hCut = xOffset + 2 ^ (depth - 1)
      in  if depth > 0
          then HTree hCut (goV depth xOffset yOffset)
                          (goV depth hCut    yOffset)
          else HLeaf $ emptyTile (makeBox xOffset yOffset undefined undefined)
    goV depth xOffset yOffset =
      let vCut = yOffset + 2 ^ (depth - 1)
      in  VTree vCut (goH (depth - 1) xOffset yOffset)
                     (goH (depth - 1) xOffset vCut   )

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
checkTileSpace tile primEntry = tilePrimCount tile < mAXsHAPE && (tileStrandCount tile + (primStrandCount primEntry)) < mAXtHRESHOLDS

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
