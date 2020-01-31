-- | Instance for filling a functor of a compound shapetrees such as a list.
instance {-# Overlappable #-} ( Functor f, SpaceOf (f (CompoundTree s)) ~ s
                              , HasToken (f (ShapeTree token s))
                              , TokenOf (f (ShapeTree token s)) ~ token)
    => CanFill (f (CompoundTree s)) (f (ShapeTree token s)) where
    colorWith color     = fmap (colorWith color)
    textureWith texture = fmap (textureWith texture)
    assignToken token   = fmap (assignToken token)

-- | Instance for filling a functor of a glyph-wrapped compound shapetrees such as a list.
instance {-# Overlappable #-} ( Functor f, SpaceOf (f (Glyph (CompoundTree s))) ~ s
                              , HasToken (f (Glyph (ShapeTree token s)))
                              , TokenOf (f (Glyph (ShapeTree token s))) ~ token)
         => CanFill (f (Glyph (CompoundTree s))) (f (Glyph (ShapeTree token s))) where
    colorWith color   = fmap (mapGlyph (colorWith color))
    textureWith pict  = fmap (mapGlyph (textureWith pict))
    assignToken token = fmap (mapGlyph (assignToken token))



instance (SimpleTransformable a) => SimpleTransformable [a] where
    translateBy v = map (translateBy v)
    scaleBy     s = map (scaleBy     s)
    stretchBy   p = map (stretchBy   p)

instance (Transformable a) => Transformable [a] where
    rotateBy    a = map (rotateBy    a)

instance {-# Overlappable #-} (HasSpace a, HasSpace (f a), SpaceOf a ~ SpaceOf (f a), Functor f, SimpleTransformable a) => SimpleTransformable (f a) where
    translateBy v = fmap (translateBy v)
    scaleBy     s = fmap (scaleBy     s)
    stretchBy   p = fmap (stretchBy   p)

instance {-# Overlappable #-} (HasSpace a, HasSpace (f a), SpaceOf a ~ SpaceOf (f a), Functor f, Transformable a) => Transformable (f a) where
    rotateBy    a = fmap (rotateBy    a)

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

{-
projectPoint :: Space s => Int -> Maybe s -> Bezier s -> Point2 s -> Point2 s
projectPoint max_steps m_accuracy bz p@(Point x y) = goProjectPoint len bz (Point2 (x/len) y)
  | x <= 0 = projectPointBefore len bz p
  | x >= len = projectPointAfter len bz p
  | otherwise = projectPointInside len bz p
  where
   len = (inverseArcLength max_steps m_accuracy bz x)
-}

-- instance (Space s) => CanProject (Bezier s) (Bezier s) where
--     projectionWithStepsAccuracy max_steps m_accuracy path =
--        over bzPoints (fmap (goProjectPoint max_steps m_accuracy path))

-- stretchProject :: (Monad f, Alternative f) => s -> Bezier s -> Bezier s -> Bezier s
--     stretchProject len path = projectionWithStepsAccuracy max_steps m_accuracy path

-- | Project a point that is already known to be inside range of the bezier
-- by projecting off the tangent to the split point.
-- projectPointInside :: Space s => s -> s -> Bezier s -> Point2 s -> Point2 s
-- projectPointInside offset len bz@(Bez anchor control endpoint) (Point2 x y) =
--   start .+^ (y *^ normal)
--   where
--     t = (x-offset)/len
--     (_, Bez onCurve tangent _) = splitBezier t bz
--     start  = onCurve
--     normal = normalize (perp (tangent .-. onCurve))
--
-- projectBezierInside :: Space s => s -> s -> Bezier s -> Bezier s -> Bezier s
-- projectBezierInside offset len bz = overBezier (projectPointInside offset len bz)

projectBezierInsideOrdered :: forall s . Space s => s -> s -> Bezier s -> Bezier s -> Bezier s
projectBezierInsideOrdered offset len path bz =
  let correctX (Point2 x y) = Point2 ((x - offset)/len) y
      bzCorrected = tr "bzCorrected" $ over bzPoints (fmap correctX) bz
      (V3 t0 tc t1) = fmap (unOrtho . view pX) . view bzPoints $ bzCorrected
      (V3 y0 yC y1) = fmap (unOrtho . view pY) . view bzPoints $ bzCorrected
      (_, first) = splitBezier t0 path
      (Bez p0 pC p1) = first
      normal0 :: V2 s
      normal0 = normalize (perp (pC .-. p0))
  in
      if (t0 == t1)
      then Bez (p0 .+^ (normal0 ^* y0)) (p0 .+^ (normal0 ^* yC)) (p0 .+^ (normal0 ^* y1))
      else
        let (pathSlice, _) = splitBezier ((t1 - t0) / (1 - t0))  first
            (Bez v0 c v1) = tr "pathSlice" $ pathSlice
            normal1 = tr "normal1" $ normalize (perp (v1 .-. c))
            m = normal0 ^+^ normal1
            offset0 = normal0 ^* y0
            offset1 = normal1 ^* y1
            offsetC = m ^* (2 * yC / m `dot` m) -- probably wrong
        in  Bez (v0 .+^ offset0) (c .+^ offsetC) (v1 .+^ offset1)



projectBezierInside offset len path bz =
  tr "projectInside" $
  if bezierIsForward bz
  then projectBezierInsideOrdered offset len path bz
  else reverseBezier . projectBezierInsideOrdered offset len path . reverseBezier $ bz


-- | Project a point that is already known to be before the start of the bezier by projecting off the tangent of
-- the start point.
projectPointBefore :: Space s => s -> Bezier s -> Point2 s -> Point2 s
projectPointBefore offset bz@(Bez v0 c v1) (Point2 x y) =
   start .+^ (y *^ normal)
   where
   t = x - offset
   tangent = c .-. v0
   start  = v0 .+^ (normalize tangent ^* t)
   normal = normalize (perp tangent)

projectBezierBefore :: Space s => s -> Bezier s -> Bezier s -> Bezier s
projectBezierBefore offset bz = overBezier (projectPointBefore offset bz)

-- | Project a point that is already known to beyond the end of the bezier by projecting off the tangent of the v1.
projectPointAfter :: Space s => s -> Bezier s -> Point2 s -> Point2 s
projectPointAfter offset bz@(Bez v0 c v1) (Point2 x y) =
   start .+^ (y *^ normal)
   where
   t = x - offset
   tangent = v1 .-. c
   start  = v1 .+^ (normalize tangent ^* t)
   normal = normalize (perp tangent)

projectBezierAfter :: Space s => s -> Bezier s -> Bezier s -> Bezier s
projectBezierAfter offset bz = tr "projectAfter" . overBezier (projectPointAfter offset bz)

traverseBezierSpace :: forall f t a s
                    .  (Space (SpaceOf t)
                       , Alternative f
                       , HasBox t
                       , s ~ SpaceOf t
                       , Show t
                       , Show (f a))
                    => (s -> t -> (t, t))
                    -> (t -> a)
                    -> (s -> s -> Bezier s -> t -> a)
                    -> (s ->      Bezier s -> t -> a)
                    -> (s ->      Bezier s -> t -> a)
                    -> BezierSpace s
                    -> t
                    -> f a
traverseBezierSpace splitFun pass fInside fBefore fAfter (BezierSpace tree totalLen) item =
      go 0 item tree 5
      where
      go :: (SpaceOf t) -> t -> RangeTree (Bezier (SpaceOf t)) -> Int ->  f a
      go offset item tree i =
        if (i > 0)
        then
        tr ("traverseBezierSpace " ++ show item) $
            let box = tr ("boxOf " ++ show item) $ boxOf item
            in  case tree of
                    RangeSplit splitPoint left right ->
                      if box ^. leftSide < splitPoint
                      then if box ^. rightSide > splitPoint
                           then let (leftItem, rightItem) = tr "split" $ splitFun splitPoint item
                                    leftGroup  = go offset leftItem left (i - 1)
                                    rightGroup = go splitPoint rightItem right (i - 1)
                                 in leftGroup <|> rightGroup
                           else go offset item left (i - 1)
                      else go splitPoint item right (i - 1)
                    RangeEmpty -> empty
                    RangeLeaf len controlCurve ->
                      let near = tr "near" $ offset
                          far  = tr "far"  $ offset + len
                          margin = 0.01
                          rest
                            | near + margin > unOrtho (box ^. rightSide) = pure $ fBefore near controlCurve item
                            | far  - margin < unOrtho (box ^. leftSide ) = pure $ fAfter  far  controlCurve item
                            -- | widthOf box < 0.01                         = pure $ fInside near len controlCurve item
                            | near - margin > unOrtho (box ^. leftSide ) = let (leftItem, rightItem) = tr "splitBefore" $ splitFun near item
                                                                           in     pure (fBefore near controlCurve leftItem)
                                                                              <|> go near rightItem tree (i - 1)
                            | far + margin < unOrtho (box ^. rightSide)  = let (leftItem, rightItem) = tr "splitAfter" $ splitFun far item
                                                                           in     go near leftItem tree (i - 1)
                                                                              <|> pure (fAfter far controlCurve rightItem)
                            | otherwise                                  = pure $ fInside near len controlCurve item
                      in  rest
        else error "done"

getFirst :: HasSpace t => RangeTree t -> (X (SpaceOf t), t)
getFirst (RangeSplit splitX left right) = getFirst left
getFirst (BezierLeaf len sourceCurve) = (len, sourceCurve)
getFirst (BezierEmpty) = error "getFirst encountered RangeEmpty"

getLast :: HasSpace t => RangeTree t -> (X (SpaceOf t), t)
getLast (RangeSplit splitX left right) = getLast right
getLast (RangeLeaf len sourceCurve) = (len, sourceCurve)
getLast (RangeEmpty) = error "getLast encountered RangeEmpty"


splitCurveAcrossPoint :: a
splitCurveAcrossPoint mLeftPart
                      handleLeft
                      leftTree
                      mRightPart
                      handleRight
                      rightTree
                      near
                      splitX
                      item =
   let (leftLen,  leftCurve ) = extractRangeLeaf $ getFirst leftTree
       (rightLen, rightCurve) = extractRangeLeaf $ getLast rightTree
       (leftItem, rightItem) = splitBezierX splitX item
       cutPoint = rightItem ^. bzStart
       leftProjected  = projectPointAfter  splitX rightCurve cutPoint
       rightProjected = projectPointBefore splitX leftCurve  cutPoint
       leftGroup  = handleLeft  leftTree  mLeftPart (Just leftProjected)   near   leftItem
       rightGroup = handleRight rightTree (Just rightProjected) mRightPart splitX rightItem
       midGroup   = straight leftProjected rightProjected -- change this to adjust corner types
    in leftGroup <|> midGroup <|> rightGroup

traverseBezierSpace :: BezierSpace s
                    -> Bezier s
                    -> f (Bezier s)
traverseBezierSpace (BezierSpace tree len) bz =
    go Nothing Nothing 0 bz tree
    where
    box = tr ("boxOf " ++ show bz) $ boxOf bz
    go mLeftPart mRightPart near bz tree =
        case tree of
            RangeSplit splitX left right ->
                if box ^. leftSide < splitX
                then if box ^. rightSide > splitX
                     then splitCurveAcrossPoint mLeftPart
                                                go
                                                left
                                                mRightPart
                                                go
                                                right
                                                near
                                                splitX
                                                bz
                     else go mLeftPart mRightPart near bz left
                else go mLeftPart mRightPart near bz right
            RangeEmpty -> empty
            RangeLeaf len sourceCurve ->
                let far = near + len
                in
                if box ^. leftSide < near && mLeftPart == Nothing
                then splitCurveAcrossPoint mLeftPart
                                           mkBefore
                                           tree
                                           mRightPart
                                           go
                                           tree
                                           near
                                           near
                                           bz
                else if box ^.rightSide > far && mRightPart == Nothing
                     then splitCurveAcrossPoint mLeftPart
                                                go
                                                tree
                                                mRightPart
                                                mkAfter
                                                tree
                                                far
                                                far
                                                bz
                     else mkInside mLeftPart mRightPart near sourceCurve bz

                     mkInside :: Space s => Maybe (Point2 s) -> Maybe (Point2 s) -> s -> s -> Bezier s -> Bezier s -> Bezier s
                     mkInside mLeftPart mRightPart near len sourceCurve bz =
                         let correctX (Point2 x y) = Point2 ((x - near)/len) y
                             bzCorrected = over bzPoints (fmap correctX) bz
                             (V3 t0 tc t1) = fmap (unOrtho . view pX) . view bzPoints $ bzCorrected
                             sourceSlice = sliceBezier t0 t1 sourceCurve
                             offsetSlice = mkOffsetCurve sourceSlice bzCorrected
                             v0 = fromMaybe (offsetSlice ^. bzStart) mLeftPart
                             c  = offsetSlice ^. bzControl
                             v1 = fromMaybe (offsetSlice ^. bzEnd) mRightPart
                         in  Bez v0 c v1

                     mkOffsetCurve :: Space s => Bezier s -> Bezier s -> Bezier s
                     mkOffsetCurve (Bez s0 sC s1) (Bez (Point2 x0 y0) (Point2 xC yC) (Point2 x1 y1)) =
                       let normal0 = normalize (perp (sC .-. s0))
                           normal1 = normalize (perp (s1 .-. sC))
                           m = normal0 ^+^ normal1
                           offset0 = normal0 ^* y0
                           offsetC = m ^* (2 * yC / m `dot` m) -- probably wrong
                           offset1 = normal1 ^* y1
                           v0 = s0 .+^ offset0
                           vC = sC .+^ offsetC
                           v1 = s1 .+^ offset1
                       in  Bez v0 vC v1

                     mkBefore tree _ mRightPart near bz =
                       let (_, sourceCurve) = extractRangeLeaf tree
                           v0 = projectPointBefore near sourceCurve (bz ^. bzStart)
                           c  = projectPointBefore near sourceCurve (bz ^. bzControl)
                           v1 = fromMaybe (projectPointBefore near sourceCurve (bz ^. bzEnd)) mRightPart
                       in  Bez v0 c v1

                     extractRangeLeaf :: a
                     extractRangeLeaf (RangeLeaf len curve) = (len, curve)
                     extractRangeLeaf x = error $ "extractRange leaf found " ++ show x

                     mkAfter tree mLeftPart _ far bz =
                       let (_, sourceCurve) = extractRangeLeaf tree
                           v0 = fromMaybe (projectPointAfter far sourceCurve (bz ^. bzStart)) mLeftPart
                           c  = projectPointAfter far sourceCurve (bz ^. bzControl)
                           v1 = projectPointAfter far sourceCurve (bz ^. bzEnd)
                       in  Bez v0 c v1

                     projectPointBefore :: Space s => s -> Bezier s -> Point2 s -> Point2 s
                     projectPointBefore offset bz@(Bez v0 c v1) (Point2 x y) =
                        start .+^ (y *^ normal)
                        where
                        t = x - offset
                        tangent = c .-. v0
                        start  = v0 .+^ (normalize tangent ^* t)
                        normal = normalize (perp tangent)

                     projectPointAfter :: Space s => s -> Bezier s -> Point2 s -> Point2 s
                     projectPointAfter offset bz@(Bez v0 c v1) (Point2 x y) =
                        start .+^ (y *^ normal)
                        where
                        t = x - offset
                        tangent = v1 .-. c
                        start  = v1 .+^ (normalize tangent ^* t)
                        normal = normalize (perp tangent)

-- | Instance for filling a functor of a glyph-wrapped compound shapetrees such as a list.
instance {-# Overlappable #-} (Functor f, SpaceOf (f (Glyph (CompoundTree s))) ~ s) => CanFill (f (Glyph (CompoundTree s))) (f (Glyph (ShapeTree Int s))) where
    colorWith color    = fmap (colorWith color)
    textureWith pict = fmap (textureWith pict)

-- | Open rectangle (Temporary until stroke implemented)
openRectangle :: Space s
              => s
              -> Point2 s
              -> Glyph (CompoundTree s)
openRectangle s p = let strokeDelta = Point2 s s in
                    subtractFrom (rectangle p)
                              (mapGlyph (translateBy strokeDelta) $ rectangle (p ^-^ (strokeDelta ^* 2)))

{-
projectOffsetCurve :: Space s
                   => Int
                   -> Maybe s
                   -> s
                   -> Point2 s
                   -> Diff Point2 s
                   -> s
                   -> Point2 s
                   -> Diff Point2 s
                   -> Point2 s
                   -> s
                   -> Bezier s
                   -> Bezier s
projectOffsetCurve max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len bz =
    let sourceCurve = Bez sPoint control ePoint
        correctX x  = inverseArcLength max_steps m_accuracy sourceCurve (x - start)
        (V3 t0 tC t1) = fmap (correctX . view pX) . view bzPoints $ bz
        (V3 y0 yC y1) = fmap (view pY) . view bzPoints $ bz
        (s0, normal0) = bezierPointAndNormal t0 sourceCurve
        (sC, normalC) = bezierPointAndNormal tC sourceCurve
        (s1, normal1) = bezierPointAndNormal t1 sourceCurve
        m = normal0 ^+^ normal1
        cCurve = Bez (normal0 ^* yC)
                     (m ^* (2 * yC / m `dot` m)) -- probably wrong
                     (normal1 ^* yC)
        s = s0 .+^ normal0 ^* y0
        c = sC .+^ cOffset
        e = s1 .+^ normal1 ^* y1
    in  Bez s c e
-}

offsetCurve d bz@(Bez v0 c v1) =
  let normal0 = bezierStartNormal bz
      normal1 = bezierEndNormal   bz
      m = normal0 ^+^ normal1
      k = m ^* ((2 * d) / (m `dot` m))
  in  Bez (v0 .+^ (normal0 ^* d))
          (c  .+^ k)
          (v1 .+^ (normal1 ^* d))




          projectOffsetCurve :: forall s
                             .  Space s
                             => Int
                             -> Maybe s
                             -> s
                             -> Point2 s
                             -> Diff Point2 s
                             -> s
                             -> Point2 s
                             -> Diff Point2 s
                             -> Point2 s
                             -> s
                             -> Bezier s
                             -> Bezier s
          projectOffsetCurve max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len bz =
              let sourceCurve = tr "sourceCurve =" $ Bez sPoint control ePoint
                  correctX x  = inverseArcLength max_steps m_accuracy sourceCurve (x - start)
                  correctBz :: Bezier s
                  correctBz = tr "correctBz = " $ over bzPoints (fmap (over pX correctX)) $ tr "bz =" bz
                  (V3 t0 tC t1) = fmap (view pX) . view bzPoints $ correctBz
                  (V3 y0 yC y1) = fmap (view pY) . view bzPoints $ correctBz
                  curveSlice = tr "curveSlice =" $ sliceBezier t0 t1 sourceCurve
              in  if t0 == t1
                  then let (s0, normal0) = bezierPointAndNormal t0 sourceCurve
                           s = s0 .+^ (normal0 ^* y0)
                           e = s0 .+^ (normal0 ^* y1)
                           c = mid s e
                       in  Bez s c e
                  else let tangent0 = tr "tangent0 =" $ bezierStartTangent bz
                           tangent1 = tr "tangent1 =" $ bezierEndTangent   bz
                           normal0  = tr "normal0 ="  $ bezierStartNormal curveSlice
                           normal1  = tr "normal1 ="  $ bezierEndNormal   curveSlice
                           tangent0Rotated = tr "tangent0Rotated =" $ ontoVector normal0 tangent0
                           tangent1Rotated = tr "tangent1Rotated =" $ ontoVector normal1 tangent1
                           slope0 = tr "slope0 =" $ slopeOf tangent0Rotated
                           slope1 = tr "slope1 =" $ slopeOf tangent1Rotated
                           s0 = tr "s0 =" $ (tr "sliceStart=" $ curveSlice ^. bzStart) .+^ (normal0 ^* y0)
                           s1 = tr "s1 =" $ (tr "sliceEnd ="  $ curveSlice ^. bzEnd  ) .+^ (normal1 ^* y1)
                           c
                             | tangent0Rotated ^. _x == 0 && tangent1Rotated ^. _x == 0 = mid s0 s1
                             | tangent0Rotated ^. _x == 0 = Point2 (s0^.pX) (yInterceptSlope s1 slope1 (s0 ^. pX))
                             | tangent1Rotated ^. _x == 0 = Point2 (s1^.pX) (yInterceptSlope s0 slope0 (s1 ^. pX))
                             | abs (slope0 - slope1) < 0.1 = mid s0 s1
                             | otherwise = over pY (clamp (-1000) 1000) $ arbitraryIntersection s0 slope0 s1 slope1
                       in  tr "result =" $ Bez s0 c s1



--- Most recent

projectOffsetCurve :: forall s
                   .  Space s
                   => Bool
                   -> Int
                   -> Maybe s
                   -> s
                   -> Point2 s
                   -> Diff Point2 s
                   -> s
                   -> Point2 s
                   -> Diff Point2 s
                   -> Point2 s
                   -> s
                   -> Bezier s
                   -> Bezier s
projectOffsetCurve debug max_steps m_accuracy start sPoint sNormal end ePoint eNormal control len bz =
    let sourceCurve = trWhen debug  "sourceCurve =" $ Bez sPoint control ePoint
        correctX x  = inverseArcLength max_steps m_accuracy sourceCurve (x - start)
        bzCorrected :: Bezier s
        bzCorrected = trWhen debug  "bzCorrected = " $ over bzPoints (fmap (over pX correctX)) $ trWhen debug  "bz =" bz
        (V3 t0 tC t1) = fmap (view pX) . view bzPoints $ bzCorrected
        (V3 y0 yC y1) = fmap (view pY) . view bzPoints $ bzCorrected
        --curveSlice = trWhen debug  "curveSlice =" $ sliceBezier t0 t1 sourceCurve
    in  if t0 == t1 -- the curve is vertical.
        then let (s0, normal0) = bezierPointAndNormal sourceCurve t0
                 s = s0 .+^ (normal0 ^* y0)
                 e = s0 .+^ (normal0 ^* y1)
                 c = mid s e
             in  Bez s c e
        else let tangent0 = trWhen debug "tangent0 =" $ bezierStartTangent bz
                 tangent1 = trWhen debug "tangent1 =" $ bezierEndTangent   bz

                 (start, normal0)  = trWhen debug "(start, normal0) ="  $ bezierPointAndNormal sourceCurve t0
                 (end,   normal1)  = trWhen debug "(end,   normal1) ="  $ bezierPointAndNormal sourceCurve t1

                 s0 = trWhen debug "s0 =" $ start .+^ (normal0 ^* y0)
                 s1 = trWhen debug "s1 =" $ end .+^ (normal1 ^* y1)


                 tangent0Rotated = trWhen debug "tangent0Rotated =" $ ontoVector normal0 tangent0
                 tangent1Rotated = trWhen debug "tangent1Rotated =" $ ontoVector normal1 tangent1
                 slope0 = trWhen debug "slope0 =" $ slopeOf tangent0Rotated
                 slope1 = trWhen debug "slope1 =" $ slopeOf tangent1Rotated
                 c
                   | tangent0Rotated ^. _x == 0 && tangent1Rotated ^. _x == 0 = mid s0 s1
                   | tangent0Rotated ^. _x == 0 = Point2 (s0^.pX) (yInterceptSlope s1 slope1 (s0 ^. pX))
                   | tangent1Rotated ^. _x == 0 = Point2 (s1^.pX) (yInterceptSlope s0 slope0 (s1 ^. pX))
                   | abs (slope0 - slope1) < 0.01 = mid s0 s1
                   | otherwise = over pY (clamp (-1000) 1000) $ arbitraryIntersection s0 slope0 s1 slope1
             in  trWhen debug "result =" $ Bez s0 {-c-} sC s1
