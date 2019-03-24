fromJust (Just x) = x

buildThresholdHs :: ThresholdHs -> ShapeTree Int
buildThresholdHs action =
   --- thrIndex         :: Int
   --- thrTop           :: Float
   --- thrBottom        :: Float
   --- thrLeft          :: Float
   --- thrRight         :: Float
   --- thrPositiveSlope :: Int
   --- thrTopPersist    :: Int
   --- thrBottomPersist :: Int
   --- thrShapeIndex    :: Int
        }
buildThreshold :: [Color] -> (Color -> Color) -> Threshold -> ShapeTree
buildThreshold colors colorModifier threshold =
  let adjustedStroke = if thrTopPersist threshold == 1 || thrBottomPersist threshold == 1
                       then 0.05
                       else 0.025
      (leftY, rightY) = if (thrPositiveSlope threshold == 1)
                        then (thrTop threshold, thrBottom threshold)
                        else (thrBottom threshold, thrTop threshold)

      startPoint = Point2 (DSpace . realToFrac . thrLeft  $ threshold) (DSpace . realToFrac $ leftY )
      endPoint   = Point2 (DSpace . realToFrac . thrRight $ threshold) (DSpace . realToFrac $ rightY)
      shapeIndex = thrShapeIndex threshold
      color      = if shapeIndex == 0 then colorModifier $ colors !! shapeIndex else transparent 0.01 $ black
  in  overlap [solid color $ line adjustedStroke startPoint endPoint
              --,tTranslate (Point2 (DSpace . realToFrac . thrLeft  $ threshold) (DSpace . realToFrac . thrTop $ threshold)) .
              -- SLeaf (Left $ transparent 0.25 $ light blue) 0 $
              -- rectangle (Point2 ((DSpace . realToFrac . thrRight  $ threshold) - (DSpace . realToFrac . thrLeft $ threshold))
              --                   ((DSpace . realToFrac . thrBottom $ threshold) - (DSpace . realToFrac . thrTop  $ threshold))
              --           )
              ]

colorTupleToColor :: ColorTuple -> Color
colorTupleToColor (ColorTuple r g b a) = rgbaColor r g b a

buildColorState :: Action -> ShapeTree
buildColorState state =
  let colors = map colorTupleToColor . csColors $ state
      makeBox color = solid color $ rectangle (Point2 0.25 0.25)
      coloredBoxes = overlap . makeGrid 0.3 100 1 . map makeBox $ colors
  in  overlap [ tTranslateXY 0.1 0.1 $ coloredBoxes
              , solid black $ rectangle (Point2 1.4 0.4)
              ]

buildMaskStack :: [Color] -> DisplaySpace -> DisplaySpace -> MaskStack -> ShapeTree
buildMaskStack colors width height (MaskStack stack) =
    overlap .
    makeRow (2 * width) .
    map (buildBit width height colors (testBit (last stack))) $
    [0..min (length colors - 1) 31]

buildBit :: DisplaySpace -> DisplaySpace -> [Color] -> (Int -> Bool) -> Int -> ShapeTree
buildBit width height colors test bit =
    let size = Point2 width height
        shapeColor = if test bit
                     then solid (colors !! bit)
                     else solid (transparent 0 black)
    in shapeColor $ rectangle size

buildPixelGrid :: ShapeTree
buildPixelGrid =
    overlap .
    take 512 .
    makeColumn 1 .
    map (\i ->
    overlap [ solid (transparent 0.5 $ dark gray) $ openRectangle 0.01 (Point2 1 1)
            --, tScale 0.1 $ solid black $ overlap $ makeLine 1 (show i)
            ])
    $ [0..]

buildPassGrid :: [Color]
              -> DisplaySpace
              -> [Action]
              -> ShapeTree
buildPassGrid colorList width actions =
    let firstSections = map head . groupWith (snd . psSectionStart) . filter isParseState $ actions
        tops = map (DSpace . realToFrac . snd . psSectionStart) firstSections
        bottoms = map (DSpace . realToFrac . snd . psSectionEnd) firstSections
        heights = zipWith (-) bottoms tops
        stacks = map psShapeStack firstSections
        stackShapes = zipWith (buildMaskStack colorList width) heights stacks
    in  overlap $ zipWith (tTranslateXY 0) tops stackShapes

buildParseState :: [Color]
                -> [Threshold]
                -> Action
                -> ShapeTree
buildParseState colors thresholds action =
    case action of
        ParseState {} ->
            let start = 0 -- psActiveStart action
                end   = length thresholds - 1 --psActiveEnd   action
                --preActive = map (buildThreshold (blueish gray)) . take start  $ thresholds
                modifier = id
                active = map (buildThreshold colors modifier) . take (end - start) . drop start $ thresholds
                --postActive = map (buildThreshold gray) . drop end $ thresholds
                left   =  DSpace . realToFrac . fst . psSectionStart  $ action
                right  =  DSpace . realToFrac . fst . psSectionEnd    $ action
                top    =  DSpace . realToFrac . snd . psSectionStart  $ action
                bottom =  DSpace . realToFrac . snd . psSectionEnd    $ action
                width  = right  - left
                height = bottom - top
                renderStart = Point2 (DSpace . realToFrac . fst . psRenderStart $ action) (DSpace . realToFrac . snd . psRenderStart $ action)
                renderEnd   = Point2 (DSpace . realToFrac . fst . psRenderEnd   $ action) (DSpace . realToFrac . snd . psRenderEnd   $ action)
                renderArea  = solid (transparent 0.4 gray) . overlap $
                    [                                                          rectangle (makePoint  1                   (renderStart ^. pY                ))
                    , tTranslate (makePoint 0                 (renderStart ^. pY)) $ rectangle (makePoint (    renderStart ^. pX) (renderEnd ^. pY - renderStart ^. pY))
                    , tTranslate (makePoint (renderEnd ^. pX) (renderStart ^. pY)) $ rectangle (makePoint (1 - renderEnd   ^. pX) (renderEnd ^. pY - renderStart ^. pY))
                    , tTranslate (makePoint 0                 (renderEnd   ^. pY)) $ rectangle (makePoint  1                      (32              - renderEnd   ^. pY))
                    ]
                color  = colorTupleToColor . psSectionColor $ action
                sectionBox = tTranslateXY left top . solid color $ rectangle (Point2 width height)
                grid = buildPixelGrid
                thresholdSection = overlap $ {-preActive ++ -} active {- ++ postActive-} ++ [sectionBox]
                tempThresholds = tTranslateXY 1.2 0 $ overlap $ map (buildThreshold colors id) $ psThresholds action
            in  overlap [tempThresholds, thresholdSection, grid]
        ColorState {} -> buildColorState action

isThresholdState :: Action -> Bool
isThresholdState (ThresholdState {}) = True
isThresholdState _                   = False

isParseState :: Action -> Bool
isParseState (ParseState {}) = True
isParseState _               = False

breakWhen :: (a -> Bool) -> [a] -> [[a]]
breakWhen prop xs = breakWhen' prop xs []

breakWhen' :: (a -> Bool) -> [a] -> [a] -> [[a]]
breakWhen' prop (x:xs) ys =
  if prop x
  then ys:breakWhen' prop xs [x]
  else breakWhen' prop xs (ys++[x])
breakWhen' prop [] ys = [ys]

buildTrace :: [Action] -> Maybe TraceData
buildTrace (action:rest) =
    case action of
        ThresholdState thresholds colorTuples -> Just $
             TraceData { traceColors = map colorTupleToColor colorTuples
                       , traceThresholds = thresholds
                       , traceParseStates = filter isParseState rest
                       }
        _ -> Nothing

parseThresholdData :: String -> [Action]
parseThresholdData file = catMaybes . map buildTrace . breakWhen isThresholdState . read $ file
