frameTimeMs = 3000000 --(3300000`div`10)*10 -- miliseconds

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

------------ Old tree traversal stuff

combineShapeTree :: Combinable o a => STree o trans a -> a
combineShapeTree = \case
  SLeaf rep -> rep
  SOverlap overlap above below ->
     let above' = combineShapeTree above
         below' = combineShapeTree below
     in  combine overlap above' below'
  STransform t child -> error "shapeTransforms should be removed before combining"

defaultCombineType :: t -> Group (Combiner t)
defaultCombineType a = Group [Combiner CombineAdd a]

class Combinable o t where
  combine :: o -> t -> t -> t

instance Combinable () [(SubstanceInfo, Group (Shape (Group (Outline DisplaySpace))))] where
  combine () over under = under ++ over

instance Combinable CombineType (Group (Combiner (Group (Outline DisplaySpace)))) where
  combine combineType (Group above) (Group below) =
      let inverter =
              case combineType of
                  CombineSubtract -> map (over coCombineType invertCombineType)
                  _ -> id
      in  Group $ inverter below ++ above

combinedCompounds :: STree () (TransformType DisplaySpace) (SRep SubstanceId PictId (Combiners (Outlines DisplaySpace)))
combinedCompounds = fmap (over shapeCompoundTree (combineShapeTree . fmap defaultCombineType)) transformedTree
curveShapes :: [(SubstanceInfo, Shapes (Outlines DisplaySpace))]
curveShapes = combineShapeTree . fmap (pure . buildShapes) $ combinedCompounds


buildShapes :: SRep SubstanceId PictId (Group (Combiner t)) -> (SubstanceInfo, Group (Shape t))
buildShapes (SRep token substance rep) = (SubstanceInfo substance, fmap (mkShape token substance) rep)

boundedShapedEnclosures = parMap rpar {- map -} (makeCurveEnclosures curveTable sectionSize canvasSize) $ map snd curveShapes
substances = map fst curveShapes
return (substances, boundedShapedEnclosures, shapeTreeState)
--------------------- Fast Truncate -----------------------

dOUBLEMAGIC         = 6755399441055744.0   :: CDouble  -- 2^52 * 1.5,  uses limited precisicion to floor
dOUBLEMAGICDELTA    = 1.5e-8               :: CDouble  -- almost .5f =.5f + 1e^(number of exp bit)
dOUBLEMAGICROUNDEPS = 0.5-dOUBLEMAGICDELTA :: CDouble  -- almost .5f =.5f - 1e^(number of exp bit)

{-# INLINE fastTruncateDouble #-}
fastTruncateDouble x = {-trWith showBin "fastTruncateDouble" $-} (unsafeCoerce :: CDouble -> Int) (x - dOUBLEMAGICROUNDEPS + dOUBLEMAGIC) .&. 0x0000FFFF

{-# INLINE fastRoundDouble #-}
fastRoundDouble    x = {-trWith showBin "fastRoundDouble" $-} (unsafeCoerce :: CDouble -> Int) (x + dOUBLEMAGIC) .&. 0x0000FFFF

sINGLEMAGIC = (2 ^ 23) {-*  1.5-} {-6291456.0-} :: CFloat  -- 2^22 * 1.5,  uses limited precisicion to floor
sINGLEMAGICDELTA    = 1.5e-8                    :: CFloat  -- almost .5f =.5f + 1e^(number of exp bit)
sINGLEMAGICROUNDEPS = 0.5-sINGLEMAGICDELTA      :: CFloat  -- almost .5f =.5f - 1e^(number of exp bit)

{-# INLINE fastTruncateSingle #-}
fastTruncateSingle x = {-trWith showBin ("fastTruncateSingle" ++ show x ++ "-->") $-} (unsafeCoerce :: CFloat -> Int) (x - sINGLEMAGICROUNDEPS + sINGLEMAGIC) .&. 0x0000FFFF

showTest = map fastTruncateSingle [4,4.1,4.5,4.7, 4.999, 4.99999]

{-# INLINE fastRoundSingle #-}
fastRoundSingle   x = {-trWith showBin "fastRoundSingle" $-} (unsafeCoerce :: CFloat -> Int) (x + sINGLEMAGIC) .&. 0x0000FFFF

10200

      repLevel (SRep token substance subtree) = do onSubstance token substance
                                                   combineLevel token substance defaultCombine subtree
      combineLevel token substance combine subtree =
          case subtree of
              SLeaf item -> onShape token substance combine item
              SOverlap operator above below ->
                  do let (aboveCombine, belowCombine) = traverseCombine operator combine
                     combineLevel token substance aboveCombine above
                     combineLevel token substance belowCombine below
              STransform t child -> error "shapeTransforms should be removed before this traversal"

buildGeometryPile :: [Shape (BoundingBox, Enclosure)] -> IO ([Shape ShapeEntry], GeometryPile)
buildGeometryPile boundedShapedEnclosures =
  do geometryPile <- newPileSize 65536 :: IO BytePile
     runStateT (mapM (overShape makeShapeEntry) boundedShapedEnclosures) geometryPile

outputGeometryPile :: GeometryPile -> IO ()
outputGeometryPile pile =
  do
    putStrLn "---------------- rJGeometryPile ------------------- "
    print pile
    putStr =<< fmap unlines (bytePileToGeometry pile)

-- | Reset pile cursors for the entire job and erase the shape map
resetRasterJob :: MonadIO m => StateT RasterJob m ()
resetRasterJob =
    do  rJShapePile %= resetPile
        rJTilePile  %= resetPile

instance (Num s, Ord s, Iota s) => Epsilon s where
  nearZero = (<= iota)

-- | Largest size for all of the geometry data for a scene.
geoMemoryLimit :: OpenCLKernelLibrary -> Int
geoMemoryLimit library = fromIntegral $ clGlobalMemSize library --clMaxConstantBufferSize library

-- |
groupSizeLimit :: OpenCLKernelLibrary -> CSize
groupSizeLimit = clMaxGroupSize

--
overShape :: (t -> StateT s IO u) -> (Shape t) -> StateT s IO (Shape u)
overShape f (Shape i t) = do u <- f t
                             return $ Shape i u

{-
buildStrand :: (Num s, Ord s, Iota s) => (Maybe (Triple (Point2 s)), (Ordering, [Triple (Point2 s)]), Maybe (Triple (Point2 s))) -> [Triple (Point2 s)]
buildStrand (start, (LT, xs), end) =                            buildStrand' start xs end
buildStrand (start, (GT, xs), end) = reverse . map flipTriple $ buildStrand' start xs end
buildStrand (start, (EQ, xs), end) = error "buildStrand'' encountered EQ section."
-}

transformShapeTree :: forall t s o leaf . (Transformable t s)
                   => ((t s -> t s) -> leaf -> leaf)
                   -> STree o (Transformer s) leaf
                   -> STree o (Transformer s) leaf
transformShapeTree f tree =
  case tree of
      SLeaf rep -> SLeaf rep
      STransform t child ->
          fmap (f (applyTransformer t)) $ transformShapeTree f child
      SMeld overlap above below ->
          let above' = transformShapeTree f above
              below' = transformShapeTree f below
          in  SMeld overlap above' below'

transformOutlineTree :: (Floating s, Eq s)
                     => STree o (Transformer s) (SRep token substance (STree o1 (Transformer s) (RawShape_ s)))
                     -> STree o (Transformer s) (SRep token substance (STree o1 (Transformer s) (Outlines s)))
transformOutlineTree shapeTree =
    let --outlineTree :: STree () (Transformer SubSpace) (SRep SubstanceId PictId (STree Compound (Transformer SubSpace) Outlines))
        outlineTree = fmap (over shapeCompoundTree (fmap (Group . rawShapeToOutlines))) shapeTree
        --transformedTree :: STree () (Transformer SubSpace) (SRep SubstanceId PictId (STree Compound (Transformer SubSpace) Outlines))
        transformedTree = transformShapeTree (fmap . fmap . fmap) . (fmap . fmap $ transformShapeTree fmap) $ outlineTree
    in  transformedTree


starLine c x = take x (repeat c)
titleBar m = starLine '>' 15 ++ m ++ starLine '>' 15 ++ "\n"
lowerBar m = starLine '<' 15 ++ m ++ starLine '<' 15 ++ "\n"
