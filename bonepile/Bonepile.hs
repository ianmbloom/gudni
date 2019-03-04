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

-- remove duplicates
removeDuplicateVertices :: (Eq s) => [Segment s] -> [Segment s]
removeDuplicateVertices (a:b:vs) =
  let rest = removeDuplicateVertices (b:vs) in
  if a ^. anchor == b ^. anchor then rest else a:rest
removeDuplicateVertices v = v

expandVerts :: (Ord s, Show s, Fractional s, Num s) => [Segment s] -> [CurvePair s]
expandVerts vs = let removed = removeDuplicateVertices vs
                 in  if length removed <= 1
                     then []
                     else segmentsToCurvePairs removed


bagSnd :: Bag PrimId (Shaper Enclosure) -> (BoundingBox, Shaper Enclosure) -> (Bag PrimId (Shaper Enclosure), PrimEntry)
bagSnd bag (box, shapeEnclosure) =
    let (bag', newPrimId) = addToBag bag shapeEnclosure
    in  (bag', PrimEntry newPrimId (enclosureNumStrands $ shapeEnclosure ^. shRep) box)

bagShapes :: [(BoundingBox, Shaper Enclosure)]
          -> (Bag PrimId (Shaper Enclosure), [PrimEntry])
bagShapes boxPrimEnclosures = mapAccumL bagSnd emptyBag boxPrimEnclosures


newShape :: MonadIO m
         => PrimId
         -> Shaper Enclosure
         -> RasterJobMonad DisplaySpace m ShapeId
newShape primId (Shaper shapeInfo enclosure) =
    do

curveToGeoRef :: MonadIO m
              => (PrimId, Shaper Enclosure)
              -> RasterJobMonad DisplaySpace m ShapeId
curveToGeoRef (primId, primEnclosure) =
    do  geoMap <- use rJShapeMap
        case M.lookup primId geoMap of
            Just shapeRef -> return $ shapeRef
            Nothing       -> newShape primId primEnclosure

offsetShape :: Reference b -> (ShapeHeader, Slice PrimId) -> (ShapeHeader, Slice b)
offsetShape offset (header, Slice ref breadth) = (header, Slice (Ref $ unRef ref+ unRef offset) (Breadth $ unBreadth breadth))

checkJob :: RasterJob -> Bool
checkJob job = not $ (isEmptyPile . view rJGeometryPile $ job) ||
                     (isEmptyPile . view rJGroupPile    $ job) ||
                     (isEmptyPile . view rJShapeRefPile  $ job) ||
                     (isEmptyPile . view rJTilePile     $ job)
