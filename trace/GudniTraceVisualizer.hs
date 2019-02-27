{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module GudniTraceVisualizer
  ( main
  )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure

import Graphics.Gudni.Application
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Fuzzy
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Draw

import Data.Word
import Data.List
import Data.Maybe
import Data.Bits

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IfElse

import Codec.Picture

import GHC.Exts

data Threshold = Threshold
    { thrIndex      :: Int
    , thrTop        :: Float
    , thrBottom     :: Float
    , thrLeft       :: Float
    , thrRight      :: Float
    , thrPositiveSlope :: Int
    , thrTopPersist :: Int
    , thrBottomPersist :: Int
    , thrShapeIndex :: Int
    } deriving (Read, Show)

data ColorTuple = ColorTuple
    { redChan   :: Float
    , greenChan :: Float
    , blueChan  :: Float
    , alphaChan :: Float
    } deriving (Read, Show)

data MaskStack = MaskStack
    { msList :: [Word]
    } deriving (Read, Show)

data Action
    = ListStart
    | ThresholdState
    { tsThresholds   :: [Threshold]
    , tsColors       :: [ColorTuple]
    }
    | ParseState
    { psCurrentThreshold :: Int
    , psNumActive        :: Int
    , psRenderStart      :: (Float, Float)
    , psRenderEnd        :: (Float, Float)
    , psSectionStart     :: (Float, Float)
    , psSectionEnd       :: (Float, Float)
    , psPixelY           :: Float
    , psSectionColor     :: ColorTuple
    , psShapeStack        :: MaskStack
    , psThresholds       :: [Threshold]
    }
    | ColorState
    { csColors :: [ColorTuple]
    } deriving (Read, Show)

data TraceData = TraceData
  { traceColors :: [Color]
  , traceThresholds :: [Threshold]
  , traceParseStates :: [Action]
  } deriving (Show)

data TraceState = TraceState
  { _stateScale       :: DisplaySpace
  , _stateDelta       :: Point2 DisplaySpace
  , _stateCircleDelta :: Point2 DisplaySpace
  , _stateAngle       :: Angle  DisplaySpace
  , _statePaused      :: Bool
  , _stateSpeed       :: DisplaySpace
  , _statePace        :: DisplaySpace
  , _stateLastTime    :: SimpleTime
  , _stateDirection   :: Bool
  , _statePlayhead    :: DisplaySpace
  , _stateCursor      :: Point2 IntSpace
  , _stateStep        :: Int
  , _stateSelectPass  :: Int
  , _stateTraceData   :: [TraceData]
  }
makeLenses ''TraceState

instance Show TraceState where
  show state = "Scale: " ++ show (state ^. stateScale) ++ " Delta: " ++ show (state ^. stateDelta)

initialModel traceData =
    TraceState
    { _stateScale          = 10
    , _stateDelta          = Point2 0 0
    , _stateCircleDelta    = Point2 0 0
    , _stateAngle          = 0 @@ turn-- quarterTurn
    , _statePaused         = True
    , _stateSpeed          = 0.01
    , _statePace           = 100
    , _stateLastTime       = 0
    , _stateDirection      = True
    , _statePlayhead       = 0
    , _stateCursor         = Point2 18 6 :: Point2 IntSpace
    , _stateStep           = 0
    , _stateSelectPass     = 0
    , _stateTraceData      = traceData
    }

instance Model TraceState where
    screenSize state = --FullScreen
        Window $ Point2 1024 -- must be a multiple of 32 currently
                        1024
    shouldLoop _ = True
    fontFile state = fromMaybe "Times New Roman.ttf" <$> listToMaybe . filter (isInfixOf "Times New Roman.ttf") <$> fontLibrary
    modelCursor state = state ^. stateCursor
    updateModel frame elapsedTime inputs state =
        flip execStateT state $
            do  mapM_ processInput inputs
                lastTime <- use stateLastTime
                stateLastTime .= elapsedTime
                speed <- use stateSpeed
                whenM(not <$> use statePaused) $
                    do  direction <- use stateDirection
                        let f = if direction then (+) else (-)
                            timeDelta = elapsedTime - lastTime
                            dt = realToFrac timeDelta * realToFrac speed
                        statePlayhead %= (`f` dt)
    constructFigure state status =
        do  statusGlyphs <- mapM glyphString $ lines status
            let tree = transformFromState state $ constructFromState state
                cursor size thickness = tTranslate (convert $ _stateCursor state) .
                                        solid (transparent 0.5 $ dark red) $
                                        cAdd (tTranslateXY (-size/2) (-thickness/2) $ rectangle $ Point2 size thickness)
                                             (tTranslateXY (-thickness/2) (-size/2) $ rectangle $ Point2 thickness size)
                withCursor = if False then overlap [cursor 20 1, tree] else tree
                statusTree = statusDisplay state statusGlyphs
                withStatus = if True  then overlap [statusTree, withCursor] else withCursor
            return (ShapeRoot (light gray) withStatus, "textForm")
    pictureData state = return $ (Nothing,[])

statusDisplay state status =
    tTranslateXY 3100 2000 .
    tScale 24 .
    solid (redish $ dark gray) .
    foldl1 cAdd .
    paraGrid 1 $
    status

transformFromState :: TraceState -> ShapeTree -> ShapeTree
transformFromState state =
  let sc    = view stateScale state
      delta = view stateDelta state
      angle = view stateAngle state
  in  tTranslate delta .
      tRotate angle .
      tScale sc

constructFromState :: TraceState -> ShapeTree
constructFromState state =
  let td = view stateTraceData state !! view stateSelectPass state
      step  = view stateStep  state
      parseState = buildParseState (traceColors td) (traceThresholds td) (traceParseStates td !! step)
      passGrid = buildPassGrid (traceColors td) 0.25 (traceParseStates td)
  in  tTranslateXY 1 1 $ overlap [parseState, tTranslateXY 3.2 0 $ passGrid]

processInput :: Monad m => Input (Point2 IntSpace) -> StateT TraceState m ()
processInput input =
    case input of
        (InputKey Pressed _ inputKeyboard) ->
            do  speed <- use stateSpeed
                pace  <- use statePace
                td <- use stateTraceData
                case inputKeyboard of
                    KeySymbol SymbolSpace -> statePaused %= not
                    KeyArrow ArrowUp      -> stateSpeed *=  1.25
                    KeyArrow ArrowDown    -> stateSpeed //= 1.25
                    KeyLetter LetterW   -> stateDelta %= (^+^ Point2   0    (-pace))
                    KeyLetter LetterS   -> stateDelta %= (^+^ Point2   0      pace )
                    KeyLetter LetterA   -> stateDelta %= (^+^ Point2 (-pace)  0    )
                    KeyLetter LetterD   -> stateDelta %= (^+^ Point2   pace   0    )
                    KeyLetter LetterY   -> stateDirection %= not
                    KeySymbol SymbolRightBracket -> stateScale *=  1.1
                    KeySymbol SymbolLeftBracket  -> stateScale //= 1.1
                    KeySymbol SymbolComma  -> whenM (uses stateStep (> 0 {-arbitrary-})) $ stateStep -= 1
                    KeySymbol SymbolPeriod -> whenM (uses stateStep (< 1000)) $ stateStep += 1
                    KeySymbol SymbolQuote -> whenM (uses stateSelectPass (> 0 {-arbitrary-})) $ stateSelectPass -= 1
                    KeySymbol SymbolSlash -> whenM (uses stateSelectPass (< length td - 1)) $ stateSelectPass += 1
                    KeyLetter LetterR   -> stateAngle %= normalizeAngle . (^+^ (speed @@ turn))
                    KeyLetter LetterT   -> stateAngle %= normalizeAngle . (^-^ (speed @@ turn))
                    -- KeyArrow ArrowRight -> whenM (uses stateCurrentTest (< (length tests - 1))) $ stateCurrentTest += 1
                    -- KeyArrow ArrowLeft  -> whenM (uses stateCurrentTest (> 0)) $ stateCurrentTest -= 1
                    _                   -> return ()
        (InputMouse detection modifier clicks positionInfo) ->
            case detection of
              Pressed -> stateCursor .= positionInfo
              _ -> return ()
        _ -> return ()


fromJust (Just x) = x

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

parseThresholdData :: String -> [TraceData]
parseThresholdData file = catMaybes . map buildTrace . breakWhen isThresholdState . read $ file

main :: IO ()
main = do thresholdData <- readFile "benchmarks/trace18.txt"
          let traceData = parseThresholdData thresholdData
          runApplication (initialModel traceData :: TraceState)
