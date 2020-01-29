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
