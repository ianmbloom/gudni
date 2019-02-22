{-# LANGUAGE TemplateHaskell  #-}
module GudniTests
 ( BenchmarkState(..)
 , initialModel
 , stateScale
 , stateDelta
 , stateCircleDelta
 , stateAngle
 , statePaused
 , stateSpeed
 , statePace
 , stateLastTime
 , stateDirection
 , statePlayhead
 , stateCursor
 , statePictures
 , stateTests
 , stateCurrentTest
 , stateStep
 , stateFrameNumber
 , testList
 )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Application

import Graphics.Gudni.Util.Draw
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Fuzzy
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.Draw

import Data.Word
import Data.Maybe
import Data.Char
import Control.Lens

import Control.Monad.Random
import System.Random


data BenchmarkState = BenchmarkState
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
  , _statePictures    :: (Maybe (Pile Word8), [PictureMemory])
  , _stateTests       :: [(String, BenchmarkState -> ShapeTree)]
  , _stateCurrentTest :: Int
  , _stateStep        :: Int
  , _stateFrameNumber :: Int
  }
makeLenses ''BenchmarkState

initialModel pictures =
    BenchmarkState
    { _stateScale       = 1
    , _stateDelta       = Point2 0 0
    , _stateCircleDelta = Point2 0 0
    , _stateAngle       = 0 @@ deg -- 0.02094 @@ rad -- 0 @@ turn-- quarterTurn
    , _statePaused      = True
    , _stateSpeed       = 0.1
    , _statePace        = 1
    , _stateLastTime    = 0
    , _stateDirection   = True
    , _statePlayhead    = 0
    , _stateCursor      = Point2 63 1376
    , _statePictures    = pictures
    , _stateTests       = testList
    , _stateCurrentTest = 8
    , _stateStep        = 3
    , _stateFrameNumber = 0
    }

instance Show BenchmarkState where
  show state =
     "BenchmarkState { " ++
     show (state ^. stateScale      ) ++ ", " ++
     show (state ^. stateDelta      ) ++ ", " ++
     show (state ^. stateCircleDelta) ++ ", " ++
     show (state ^. stateAngle      ) ++ ", " ++
     show (state ^. statePaused     ) ++ ", " ++
     show (state ^. stateSpeed      ) ++ ", " ++
     show (state ^. statePace       ) ++ ", " ++
     show (state ^. stateLastTime   ) ++ ", " ++
     show (state ^. stateDirection  ) ++ ", " ++
     show (state ^. statePlayhead   ) ++ ", " ++
     show (state ^. stateCursor     ) ++ ", " ++
     show (state ^. stateCurrentTest) ++ ", " ++
     show (state ^. stateStep       ) ++ ", " ++
     show (state ^. statePictures   ) ++ " }"

testList = [ ("benchmark1"        , benchmark1          ) --  0 -
           , ("fuzz testing 2"    , fuzzy2              ) --  2 -
           , ("fuzz testing 3"    , fuzzy3              ) --  3 -
           , ("fuzz testing 4"    , fuzzy4              ) --  4 -
           , ("fuzz testing 5"    , fuzzy6              ) --  5 -
           , ("plots"             , plots               ) --  6 -
           , ("testPict"          , testPict            ) --  7 -
           , ("simpleKnob"        , simpleKnob          ) --  8 -
           , ("maxThresholdTest"  , maxThresholdTest    ) --  9 -
           , ("maxShapeTest"      , maxShapeTest        ) -- 10 -
           , ("hourGlass"         , hourGlass           ) -- 11 -
           , ("simpleRectangle"   , simpleRectangle     ) -- 12 -
           , ("tallRectangle"     , tallRectangle       ) -- 13 -
           , ("openSquare"        , openSquare          ) -- 14 -
           , ("openSquareOverlap2", openSquareOverlap2  ) -- 15 -
           , ("openSquareOverlap3", openSquareOverlap3  ) -- 16 -
           , ("stackOfSquares"    , stackOfSquares      ) -- 17 -
           , ("concentricSquares2", concentricSquares2  ) -- 18 -
           , ("concentricSquares3", concentricSquares3  ) -- 19 -
           , ("simpleGlyph"       , simpleGlyph         ) -- 20 -
           , ("simpleGlyph2"      , simpleGlyph2        ) -- 21 -
           , ("simpleArc"         , simpleArc           ) -- 22 -
           , ("sixPointRectangle" , sixPointRectangle   ) -- 23 -
           , ("tinySquare"        , tinySquare          ) -- 24 -
           , ("anotherThreshold"  , anotherThreshold    ) -- 25 -
           , ("subtractDiamond "  , subtractDiamond     ) -- 26 -
           , ("rectGrid"          , rectGrid            ) -- 27 -
           , ("fuzz testing 5"    , fuzzy5              ) -- 28 -
           ]

maxThresholdTest :: BenchmarkState -> ShapeTree
maxThresholdTest state =
    let frame = view stateFrameNumber state
    in
    tTranslateXY (-1) 1 .
    tRotate (0.2 @@ rad) .
    tScale 0.1 .
    solid (transparent 1.0 . light $ purple) .
    overlap $
    makeGrid 2 1 2 $ repeat (rectangle (Point2 1000 1))

maxShapeTest :: BenchmarkState -> ShapeTree
maxShapeTest state =
     overlap . reverse .
     makeGrid 1 1 (state ^. stateStep + 2000 + 1) .
     concat .
     repeat $
        [ (solid (transparent 1.0 (pureRed    )) $ rectangle (Point2 10000 1))
        , (solid (transparent 1.0 (pureGreen  )) $ rectangle (Point2 10000 1))
        , (solid (transparent 1.0 (pureBlue   )) $ rectangle (Point2 10000 1))
        ]


fromJust (Just x) = x
plots :: BenchmarkState -> ShapeTree
plots state = tScale 100 . overlap . makeGrid 10 16 1 . catMaybes . map (fmap (solid purple . rawPlot) . plotLibrary) $ turtleNames

fuzzy2 :: BenchmarkState -> ShapeTree
fuzzy2 state = let time = view stateLastTime state
               in  tTranslateXY 500 500 .
                   overlap $ evalRand (sequence . replicate 16 $ fuzzyRadial 400 500 100) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep + 5))

fuzzy3 :: BenchmarkState -> ShapeTree
fuzzy3 state = let time = view stateLastTime state
               in  tTranslateXY (-100) (-100) .
                   overlap $
                   evalRand (sequence . replicate 4 $ fuzzyCurve (makePoint 3000 2000) 200) (mkStdGen $ state ^. stateStep)

fuzzy4 :: BenchmarkState -> ShapeTree
fuzzy4 state = let time = view stateLastTime state
               in  tTranslateXY (-100) (-100) .
                   overlap $
                   evalRand (sequence . replicate 10000 $ fuzzyCircle (makePoint 2880 1800) 5 60) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

fuzzy5 :: BenchmarkState -> ShapeTree
fuzzy5 state = let time = view stateLastTime state
               in  overlap $
                   evalRand (sequence . replicate 500 $ fuzzySquare (makePoint 100 100) 10 60) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

fuzzy6 :: BenchmarkState -> ShapeTree
fuzzy6 state = let time = view stateLastTime state
               in  overlap $
                   evalRand (sequence . replicate 10000 $ fuzzySquare (makePoint 2880 1800) 10 60) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

benchmark1 :: BenchmarkState -> ShapeTree
benchmark1 state =
    let dSize         = state ^. stateScale
        angle         = normalizeAngle ((((fromIntegral $ state ^. stateStep) / 100) + (state ^. statePlayhead)) @@ turn)
        defaultString = cycle "***O***X" --"ALl WoRk AnD nO pLaY mAkEs JaCk A dUlL bOy" ++ ['a'..'z']++['A'..'Z']++['0'..'9']++"!@#$%^&*()_+-={}[]:;\"'<>,./?\\|"
        angles        = {-repeat (0 @@ deg) -} map (normalizeAngle . (angle ^+^) . (^*0.5) . (@@ deg) ) [0..]
        textGrid  :: CompoundTree
        textGrid   = tScale dSize . foldl1 cAdd . makeGrid 0.5 50 50 . zipWith tRotate angles . map glyph $ defaultString
        subtractor:: CompoundTree
        subtractor = tScale (dSize * 15) . foldl1 cAdd . makeGrid 0.5 4 4 . map glyph . cycle $ "*" -- take 50 $ cycle $ "+@$" -- message
    in  tTranslateXY 0 0 .
        tScale 50 .
        solid (transparent 1.0 black) $
        cSubtract textGrid subtractor

benchmarkfault :: BenchmarkState -> ShapeTree
benchmarkfault state =
    let dSize         = state ^. stateScale
        angle         = normalizeAngle ((((fromIntegral $ state ^. stateStep + 298) / 100) + (state ^. statePlayhead)) @@ turn)
        defaultString = (take 150 $ cycle " ") ++ (take 50 $ cycle $ "*") -- "***O***X" --"ALl WoRk AnD nO pLaY mAkEs JaCk A dUlL bOy" ++ ['a'..'z']++['A'..'Z']++['0'..'9']++"!@#$%^&*()_+-={}[]:;\"'<>,./?\\|"
        angles        = {-repeat (0 @@ deg) -} map (normalizeAngle . (angle ^+^) . (^*0.5) . (@@ deg) ) [0..]
        textGrid  :: CompoundTree
        textGrid   = tScale dSize . foldl1 cAdd . makeGrid 0.5 50 50 . zipWith tRotate angles . map glyph $ defaultString
        subtractor:: CompoundTree
        subtractor = tScale (dSize * 15) . foldl1 cAdd . makeGrid 0.5 4 4 . map glyph . cycle $ "*" -- take 50 $ cycle $ "+@$" -- message
    in  tTranslate (Point2 (-247) 0) .
        tScale 25 .
        solid (transparent 1.0 black) $
        textGrid -- cSubtract textGrid subtractor

rectGrid :: BenchmarkState -> ShapeTree
rectGrid state =
    let grid  :: CompoundTree
        grid   = foldl1 cContinue . makeGrid 1 10 1000 . repeat . rectangle $ Point2 0.5 0.5
    in
        --tTranslate (Point2 0.3 0.3) .
        --tRotate (5 @@ deg) .
        solid (transparent 1.0 white) $
        grid

rectGrid2 :: BenchmarkState -> ShapeTree
rectGrid2 state =
    let grid  :: CompoundTree
        grid   = foldl1 cContinue . makeGrid 1 100 100 . repeat . rectangle $ Point2 0.5 0.5
    in
        solid (transparent 1.0 white) $
        grid

simpleKnob :: BenchmarkState -> ShapeTree
simpleKnob state =
        tScale 100 .
        solid (transparent 1.0 (dark $ dark gray)) .
        raw $
        [ Vert True  $ makePoint 0 0
        , Vert False $ makePoint 1 1
        , Vert True  $ makePoint 0 2
        ]

hourGlass :: BenchmarkState -> ShapeTree
hourGlass state =
  let step = fromIntegral $ state ^. stateStep
  in    tTranslateXY 0 0 .
        tTranslateXY 0.5 0.52 .
        tScale 8 .
        solid (transparent 1.0 (dark $ dark gray)) .
        raw $
        [ Vert True  $ makePoint 0 0
        , Vert True  $ makePoint 1 1
        , Vert True  $ makePoint 1 0
        , Vert True  $ makePoint 0 1
        ]

-- Very simple box
simpleRectangle :: BenchmarkState -> ShapeTree
simpleRectangle state =
  tTranslateXY (0.35 + 0.1) 0 .
  tRotate (45 @@ deg) .
  tScale 1 .
  solid (transparent 1.0 white) $
  rectangle (Point2 2 2)

testPict :: BenchmarkState -> ShapeTree
testPict state =
        overlap [ textureWith (PictureRef (Point2 0 0) 0 0) $ tTranslateXY 100 100 (cSubtract (tScale 200 circle) (tScale 100 circle))
                , tTranslateXY 50 50 $ solid (transparent 0.2 blue) $ rectangle (Point2 40  2000)]

-- this caused a precision error
tallRectangle :: BenchmarkState -> ShapeTree
tallRectangle state =
        tTranslateXY 0 1 .
        --tRotate (3 @@ deg) .
        solid (transparent 1.0 white) $
        rectangle (Point2 0.5 10)

-- Color Overlays with subtraction.
openSquareOverlap3 :: BenchmarkState -> ShapeTree
openSquareOverlap3 state =
    let angle         = view statePlayhead state @@ turn
    in  tScale 50 $
        overlap [ (tTranslateXY 0 0 . tRotate angle        $ solid (transparent 0.5 blue   ) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (tTranslateXY 4 4 . tRotate (angle ^/ 2) $ solid (transparent 0.5 orange ) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (tTranslateXY 8 8 . tRotate (angle ^/ 3) $ solid (transparent 0.5 green  ) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                ]

stackOfSquares :: BenchmarkState -> ShapeTree
stackOfSquares state =
        overlap
          [ (tTranslateXY 0 0  . solid (transparent 1.0 red    ) $ rectangle (Point2 4 4) )
          , (tTranslateXY 0 4  . solid (transparent 1.0 green  ) $ rectangle (Point2 4 4) )
          --, (tTranslateXY 0 8  . solid (transparent 1.0 blue   ) $ rectangle (Point2 4 4) )
          --, (tTranslateXY 0 12 . solid (transparent 1.0 purple ) $ rectangle (Point2 4 4) )
          ]

openSquareOverlap2 :: BenchmarkState -> ShapeTree
openSquareOverlap2 state =
        tScale 0.25 $
        overlap [ (tTranslateXY 0 0 . solid (transparent 0.25 blue  ) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (tTranslateXY 8 8 . solid (transparent 1.0  orange) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                ]

openSquare :: BenchmarkState -> ShapeTree
openSquare state =
    solid (transparent 0.5 white) $
          cSubtract (rectangle (Point2 5 5))
                    (tTranslate (Point2 1 1) $ rectangle (Point2 3 3))

concentricSquares2 :: BenchmarkState -> ShapeTree
concentricSquares2 state =
        overlap [ (tTranslateXY 0 0 . solid (transparent 1.0 red ) $ cSubtract (rectangle (Point2 5 5)) (tTranslate (Point2 1 1) $ rectangle (Point2 3 3) ) )
                , (tTranslateXY 1 1 . solid (transparent 1.0 blue) $ cSubtract (rectangle (Point2 3 3)) (tTranslate (Point2 1 1) $ rectangle (Point2 1 1) ) )
                ]

concentricSquares3 :: BenchmarkState -> ShapeTree
concentricSquares3 state =
        overlap [ (tTranslateXY 0 0 . solid (transparent 1.0 red   ) $ cSubtract (rectangle (Point2 10 10)) (tTranslate (Point2 2 2) $ rectangle (Point2 6 6) ) )
                , (tTranslateXY 2 2 . solid (transparent 1.0 green ) $ cSubtract (rectangle (Point2  6  6)) (tTranslate (Point2 2 2) $ rectangle (Point2 2 2) ) )
                , (tTranslateXY 4 4 . solid (transparent 1.0 blue  ) $            rectangle (Point2  2  2))
                ]

simpleGlyph :: BenchmarkState -> ShapeTree
simpleGlyph state =
  let character = chr $ state ^. stateStep
  in  tTranslateXY 0 0 .
      tRotate (6.28248 @@ rad) .
      tScale 20 .
      solid (transparent 1.0 white) $
      glyph character

simpleGlyph2 :: BenchmarkState -> ShapeTree
simpleGlyph2 state =
  let character = chr 67
  in  tTranslateXY (-18.90004) 1.6 .
      tTranslateXY (-1.98000) (-13.46012) .
      tRotate (6.22548 @@ rad) .
      tScale 33 .
      solid (transparent 1.0 $ light purple) $
      glyph character

simpleArc :: BenchmarkState -> ShapeTree
simpleArc state =
        tScale 100 .
        solid (transparent 1.0 $ dark gray) $
        arc (0.3 @@ turn)

sixPointRectangle :: BenchmarkState -> ShapeTree
sixPointRectangle state =
        solid (transparent 1.0 (dark gray)) $
        raw [Vert2 True 0 0, Vert2 True 1 0, Vert2 True 2 0
            ,Vert2 True 2 1, Vert2 True 1 1, Vert2 True 0 1
            ]

tinySquare :: BenchmarkState -> ShapeTree
tinySquare state =
        solid red $
        rectangle (Point2 2 2)

anotherThreshold :: BenchmarkState -> ShapeTree
anotherThreshold state =
    solid white $
    overlap [ raw  [Vert2 True (-0.2) 0.0, Vert2 True   1.2  1.4
                   ,Vert2 True   1.2  1.6, Vert2 True (-0.2) 0.2
                   ]
            , raw  [Vert2 True (-0.2) 0.4, Vert2 True   1.2  1.8
                   ,Vert2 True   1.2  2.0, Vert2 True (-0.2) 0.6
                   ]
            ]

subtractDiamond :: BenchmarkState -> ShapeTree
subtractDiamond state =
    solid white $
    cSubtract (diamond $ makePoint 8 8) (tTranslateXY 2 0 $ diamond $ makePoint 8 8)

mobyDick :: String
mobyDick = "Call me Ishmael. Some years ago--never mind how long precisely--having little or no money in my purse, and nothing particular to interest me on shore, I thought I would sail about a little and see the watery part of the world. It is a way I have of driving off the spleen and regulating the circulation. Whenever I find myself growing grim about the mouth; whenever it is a damp, drizzly November in my soul; whenever I find myself involuntarily pausing before coffin warehouses, and bringing up the rear of every funeral I meet; and especially whenever my hypos get such an upper hand of me, that it requires a strong moral principle to prevent me from deliberately stepping into the street, and methodically knocking people's hats off--then, I account it high time to get to sea as soon as I can. This is my substitute for pistol and ball. With a philosophical flourish Cato throws himself upon his sword; I quietly take to the ship. There is nothing surprising in this. If they but knew it, almost all men in their degree, some time or other, cherish very nearly the same feelings towards the ocean with me. There now is your insular city of the Manhattoes, belted round by wharves as Indian isles by coral reefs--commerce surrounds it with her surf. Right and left, the streets take you waterward. Its extreme downtown is the battery, where that noble mole is washed by waves, and cooled by breezes, which a few hours previous were out of sight of land. Look at the crowds of water-gazers there. Circumambulate the city of a dreamy Sabbath afternoon. Go from Corlears Hook to Coenties Slip, and from thence, by Whitehall, northward. What do you see?--Posted like silent sentinels all around the town, stand thousands upon thousands of mortal men fixed in ocean reveries. Some leaning against the spiles; some seated upon the pier-heads; some looking over the bulwarks of ships from China; some high aloft in the rigging, as if striving to get a still better seaward peep. But these are all landsmen; of week days pent up in lath and plaster--tied to counters, nailed to benches, clinched to desks. How then is this? Are the green fields gone? What do they here? But look! here come more crowds, pacing straight for the water, and seemingly bound for a dive. Strange! Nothing will content them but the extremest limit of the land; loitering under the shady lee of yonder warehouses will not suffice. No. They must get just as nigh the water as they possibly can without falling in. And there they stand--miles of them--leagues. Inlanders all, they come from lanes and alleys, streets and avenues--north, east, south, and west. Yet here they all unite. Tell me, does the magnetic virtue of the needles of the compasses of all those ships attract them thither?"
