-- | Check if the tile can hold an additional shape without splitting.
checkTileSpace :: Tile TileEntry -> ItemEntry -> Bool
checkTileSpace tile itemEntry =
  let tileEntry = tile ^. tileRep
      withAddedStrands = (tileEntry ^. tileStrandCount + (itemEntry ^. itemStrandCount))
  in     tileEntry ^. tileItemCount < mAXlAYERS - 1 -- the total shapes would be less than the maximum
      && withAddedStrands < maxStrandsPerTile -- the total strands would be less than the maximum.

    -- | Split a tile into two horizontal sections and put its contents into both sides in the proper order (reversed)
    hSplit :: Tile TileEntry -> HTree
    hSplit tile =
      let cut = tile ^. tileBox . leftSide + (widthOf (tile ^. tileBox) `div` 2)
          lEmpty = emptyTile (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set rightSide cut (tile ^. tileBox))
          rEmpty = emptyTile (tile ^. tileHDepth - 1) (tile ^. tileVDepth) (set leftSide cut (tile ^. tileBox))
          hTree = HTree (fromIntegral cut) (VLeaf lEmpty) (VLeaf rEmpty)
      in  {-tr "hSplit" $-} foldl insertShapeH hTree $ (tile ^. tileRep . tileItems)

    -- | Split a tile into two vertical sections and put its contents into both sides in the proper order (reversed)
    vSplit :: Tile TileEntry -> VTree
    vSplit tile =
      let cut = tile ^. tileBox . topSide + (heightOf (tile ^. tileBox) `div` 2)
          tEmpty = emptyTile (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set bottomSide cut (tile ^. tileBox))
          bEmpty = emptyTile (tile ^. tileHDepth) (tile ^. tileVDepth - 1) (set topSide    cut (tile ^. tileBox))
          vTree = VTree (fromIntegral cut) (HLeaf tEmpty) (HLeaf bEmpty)
      in  {-tr "vSplit" $-} foldl insertShapeV vTree $ (tile ^. tileRep . tileItems)

{-
      if -- the tile has room for more shapes or its to small to be split again
         checkTileSpace tile itemEntry || widthOf (tile ^. tileBox) <= mINtILEsIZE ^. pX
      then -- then simple add it.

      else -- otherwise split the tile in half and then add the shape to the split tiles.
           insertShapeH (hSplit tile) itemEntry

if -- the tile has room for more shapes or its to small to be split again
   checkTileSpace tile itemEntry || heightOf (tile ^. tileBox) <= mINtILEsIZE ^. pY
then -- then simple add it.
else -- otherwise split the tile in half and then add the shape to the split tiles.
     insertShapeV (vSplit tile) itemEntry
-}
