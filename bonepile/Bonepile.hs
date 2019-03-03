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

                cursor size thickness = tTranslate (convert $ _stateCursor state) .
                                        solid (transparent 0.5 red) $
                                        cAdd (tTranslateXY (-size/2) 0 $ rectangle $ Point2 size thickness)
                                             (tTranslateXY 0 (-size/2) $ rectangle $ Point2 thickness size)
                withCursor = if False then overlap [cursor 100 1, tree] else tree
