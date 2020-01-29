-- | Divide entrySequence based on number of items and total number of strands
divideEntrySequences :: Int -> Tile (S.Seq ItemEntry) -> Identity (Tile (S.Seq (S.Seq ItemEntry)))
divideEntrySequences maxThresholds tile = set tileRep (go (tile ^. tileRep) tile
  where
  go ss = let len = S.length ss
          if (len > 1) && (len > mAXlAYERS || (sum (fmap (view itemStrandCount) ss) * 3) > maxThresholds)
          then let (left, right) = splitAt (len `div` 2)
               in go left <|> go right
          else S.singleton ss

allocateTiles :: Tile (S.Seq (S.Seq ItemEntry)) -> State ([TileEntry],Int) (Tile (S.Seq (Ref TileEntry)))
allocateTiles = undefined

generateThresholds :: Pile TileEntry -> IO ()

collectTilesToSplit :: Tile (S.Seq (Ref TileEntry)) -> State [Ref TileEntry] ()

if heightOf tile > 1 && sums of columns of thresholds cannot be split.
  if the height is <= 1 then we can just take the top thresholds

collectTilesToMerge :: Tile (S.Seq (Ref TileEntry)) -> State
  return tiles that have more than one section of thresholds

collectAllTiles :: Tile (S.Seq (Ref TileEntry)) -> State ([TileEntry])
collectAllTiles



splitTileLoop :: TileTree (S.Seq (Ref TileEntry)) -> TileTree (S.Seq (S.Seq (Ref TileEntry)))
splitTileLoop generatedTree =
   do -- Collect all of the tiles that need to be split.
      let ((tilesToSplit, splitPairs, newAllocation), splitTree) = runState (traverseTileTree collectTilesToSplit) generatedTree
      in
      if length tilesToSplit > 0
      then -- Divide all of the split tiles into chunks we know can execute in one kernel.
           splitJobs = S.chunksOf maxSplitJobSize tilesToSplit
           -- Execute all of the split jobs.
           mapM (splitTiles splitPairs newAllocations ) splitJobs
           --
           splitTileLoop splitTree
      else splitTree



splitAndMergeTileTree :: TileTree (S.Seq ItemEntry) -> IO ()
splitAndMergeTileTree tree =
  do let -- Start by dividing all of the items into sub seqeunces that can definitely be generated given the memory restraints of the generation kernel.
         dividedSequenceTree = runIdentity (traverseTileTree (divideEntrySequences maxThresholds) tree)
         -- Allocate space for each generation kernel.
         (tileEntries, tileAllocation) = runState (traverseTileTree allocateTiles dividedSequenceTree) ([],0)
         -- Generate all of the thresholds from the items
         generateThresholds tileEntries tileAllocation
         -- Given the size of each generated threshold queue continuously split tiles until the can all the divided sequences can be properly merged
         (splitTree, newTileAllocation) = splitTileLoop
         -- Now extract the tiles that need to be merged.
         (tilesToMerge, mergedTree) = runState (travereTileTree (collectTilesToMerge) splitTree)
         -- Divide tilesToMerge into chunks we know a kernel can execute.
         mergeJobs = S.chunksOf maxMergeJobSize tilesToMerge
         -- Execute each merge job.
         mapM mergeTiles mergeJobs
         -- Collect all of the tiles
         finalTiles <- runState (traverseTileTree collectAllTiles) S.emtpy
         return (finalTiles, mergedTree)

renderTileTree :: FinalTiles -> IO ()


queryTileTree :: FinalTiles -> TileTree -> IO [QueryResults]
queryTileTree queries =





-- | Fold a TileTree with a monadic function.
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
