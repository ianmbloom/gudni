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
import Graphics.Gudni.Util.Plot

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
  , _stateTests       :: [(String, BenchmarkState -> GlyphMonad IO ShapeTree)]
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
    , _stateCurrentTest = 21
    , _stateStep        = 3
    , _stateFrameNumber = 0
    }

testList = [ ("openSquareOverlap3", openSquareOverlap3  ) --  0 -
           , ("benchmark1"        , benchmark1          ) --  1 -
           , ("fuzz testing 2"    , fuzzy2              ) --  2 -
           , ("fuzz testing 3"    , fuzzy3              ) --  3 -
           , ("fuzz testing 4"    , fuzzy4              ) --  4 -
           , ("fuzz testing 5"    , fuzzy6              ) --  5 -
           , ("testPict"          , testPict            ) --  6 -
           , ("rectGrid"          , rectGrid            ) --  7 -
           , ("plotter test"      , plots               ) --  8 -
           , ("simpleKnob"        , simpleKnob          ) --  9 -
           , ("hourGlass"         , hourGlass           ) -- 10 -
           , ("simpleRectangle"   , simpleRectangle     ) -- 11 -
           , ("tallRectangle"     , tallRectangle       ) -- 12 -
           , ("openSquare"        , openSquare          ) -- 13 -
           , ("openSquareOverlap2", openSquareOverlap2  ) -- 14 -
           , ("stackOfSquares"    , stackOfSquares      ) -- 15 -
           , ("concentricSquares2", concentricSquares2  ) -- 16 -
           , ("concentricSquares3", concentricSquares3  ) -- 17 -
           , ("simpleGlyph"       , simpleGlyph         ) -- 18 -
           , ("simpleArc"         , simpleArc           ) -- 19 -
           , ("sixPointRectangle" , sixPointRectangle   ) -- 20 -
           , ("tinySquare"        , tinySquare          ) -- 21 -
           , ("anotherThreshold"  , anotherThreshold    ) -- 22 -
           , ("subtractDiamond "  , subtractDiamond     ) -- 23 -
           , ("fuzz testing 5"    , fuzzy5              ) -- 24 -
           , ("maxThresholdTest"  , maxThresholdTest    ) -- 25 -
           , ("maxShapeTest"      , maxShapeTest        ) -- 26 -
           ]

maxThresholdTest :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
maxThresholdTest state =
    return $
    let frame = view stateFrameNumber state
    in
    sTranslateXY (-1) 1 .
    sRotate (0.2 @@ rad) .
    sScale 0.1 .
    solid (transparent 1.0 . light $ purple) .
    makeGrid 2 1 2 $ repeat (rectangle (Point2 1000 1))

maxShapeTest :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
maxShapeTest state =
     return .
     makeGrid 1 1 (state ^. stateStep + 2000 + 1) .
     concat .
     repeat $
        [ (solid (transparent 1.0 (pureRed    )) $ rectangle (Point2 10000 1))
        , (solid (transparent 1.0 (pureGreen  )) $ rectangle (Point2 10000 1))
        , (solid (transparent 1.0 (pureBlue   )) $ rectangle (Point2 10000 1))
        ]


fromJust (Just x) = x
plots :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
plots state = return $
              sTranslateXY 100 100 . sScale 30 . makeGrid 10 16 1 . catMaybes . map (fmap (solid purple . rawCurve) . curveLibrary) $ turtleNames

fuzzy2 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
fuzzy2 state = return $
               let time = view stateLastTime state
               in  sTranslateXY 500 500 .
                   overlap $ evalRand (sequence . replicate 16 $ fuzzyRadial 400 500 100) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

fuzzy3 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
fuzzy3 state = return $
               let time = view stateLastTime state
               in  sTranslateXY (-100) (-100) .
                   overlap $
                   evalRand (sequence . replicate 4 $ fuzzyCurve (makePoint 1440 900) 200) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

fuzzy4 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
fuzzy4 state = return $
               let time = view stateLastTime state
               in  sTranslateXY (-100) (-100) .
                   overlap $
                   evalRand (sequence . replicate 5000 $ fuzzyCircle (makePoint 1440 900) 5 60) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

fuzzy5 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
fuzzy5 state = return $
               let time = view stateLastTime state
               in  overlap $
                   evalRand (sequence . replicate 100 $ fuzzySquare (makePoint 100 100) 10 60) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

fuzzy6 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
fuzzy6 state = return $
               let time = view stateLastTime state
               in  overlap $
                   evalRand (sequence . replicate 5000 $ fuzzySquare (makePoint 1440 900) 10 60) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

benchmark1 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
benchmark1 state =
    do defaultGlyphs <- glyphString "***O***X" --"ALl WoRk AnD nO pLaY mAkEs JaCk A dUlL bOy" ++ ['a'..'z']++['A'..'Z']++['0'..'9']++"!@#$%^&*()_+-={}[]:;\"'<>,./?\\|"
       subtractorGlyphs <- glyphString "*"
       let dSize         = state ^. stateScale
           angle         = normalizeAngle ((((fromIntegral $ state ^. stateStep) / 100) + (state ^. statePlayhead)) @@ turn)
           defaultString = cycle $ defaultGlyphs
           angles        = {-repeat (0 @@ deg) -} map (normalizeAngle . (angle ^+^) . (^*0.5) . (@@ deg) ) [0..]
           textGrid  :: CompoundTree
           textGrid   = sScale dSize . makeGrid 0.5 50 50 . zipWith sRotate angles . map glyph $ defaultString
           subtractor:: CompoundTree
           subtractor = sScale (dSize * 15) . makeGrid 0.5 4 4 . map glyph . cycle $ subtractorGlyphs -- "*" -- take 50 $ cycle $ "+@$" -- message
       return $ sTranslateXY 0 0 .
                sScale 50 .
                solid (transparent 1.0 black) $
                cSubtract textGrid subtractor

rectGrid :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
rectGrid state = return $
    let grid  :: CompoundTree
        grid   = makeGrid 1 100 100 . repeat . rectangle $ Point2 0.5 0.5
    in
        --sTranslate (Point2 0.3 0.3) .
        --sRotate (5 @@ deg) .
        solid (transparent 1.0 white) $
        grid

simpleKnob :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
simpleKnob state = return $
        sScale 100 .
        solid (transparent 1.0 (dark $ dark gray)) .
        raw $
        [ curved 0 0 1 1
        , straight 0 2
        ]

hourGlass :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
hourGlass state = return $
  let step = fromIntegral $ state ^. stateStep
  in    sTranslateXY 0 0 .
        sTranslateXY 0.5 0.52 .
        sScale 8 .
        solid (transparent 1.0 (dark $ dark gray)) .
        raw $
        [ straight 0 0
        , straight 1 1
        , straight 1 0
        , straight 0 1
        ]

-- Very simple box
simpleRectangle :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
simpleRectangle state = return $
  sTranslateXY (0.35 + 0.1) 0 .
  sRotate (45 @@ deg) .
  sScale 1 .
  solid (transparent 1.0 white) $
  rectangle (Point2 2 2)

testPict :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
testPict state = return $
        overlap [ textureWith (PictureRef (Point2 0 0) 0) $ sTranslateXY 100 100 (cSubtract (sScale 200 circle) (sScale 100 circle))
                , sTranslateXY 50 50 $ solid (transparent 0.2 blue) $ rectangle (Point2 40  2000)]

-- this caused a precision error
tallRectangle :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
tallRectangle state = return $
        sTranslateXY 0 1 .
        --sRotate (3 @@ deg) .
        solid (transparent 1.0 white) $
        rectangle (Point2 0.5 10)

-- Color Overlays with subtraction.
openSquareOverlap3 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
openSquareOverlap3 state = return $
    let angle = view statePlayhead state @@ turn
    in  sTranslateXY 400 400 .
        sScale 50 $
        overlap [ (sTranslateXY 0 0 . sRotate angle        $ solid (transparent 0.5 blue   ) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (sTranslateXY 4 4 . sRotate (angle ^/ 2) $ solid (transparent 0.5 orange ) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (sTranslateXY 8 8 . sRotate (angle ^/ 3) $ solid (transparent 0.5 green  ) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                ]

stackOfSquares :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
stackOfSquares state = return $
        overlap
          [ (sTranslateXY 0 0  . solid (transparent 1.0 red    ) $ rectangle (Point2 4 4) )
          , (sTranslateXY 0 4  . solid (transparent 1.0 green  ) $ rectangle (Point2 4 4) )
          --, (sTranslateXY 0 8  . solid (transparent 1.0 blue   ) $ rectangle (Point2 4 4) )
          --, (sTranslateXY 0 12 . solid (transparent 1.0 purple ) $ rectangle (Point2 4 4) )
          ]

openSquareOverlap2 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
openSquareOverlap2 state = return $
        sScale 0.25 $
        overlap [ (sTranslateXY 0 0 . solid (transparent 0.25 blue  ) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (sTranslateXY 8 8 . solid (transparent 1.0  orange) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                ]

openSquare :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
openSquare state = return $
    solid (transparent 0.5 white) $
          cSubtract (rectangle (Point2 5 5))
                    (sTranslate (Point2 1 1) $ rectangle (Point2 3 3))

concentricSquares2 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
concentricSquares2 state = return $
        overlap [ (sTranslateXY 0 0 . solid (transparent 1.0 red ) $ cSubtract (rectangle (Point2 5 5)) (sTranslate (Point2 1 1) $ rectangle (Point2 3 3) ) )
                , (sTranslateXY 1 1 . solid (transparent 1.0 blue) $ cSubtract (rectangle (Point2 3 3)) (sTranslate (Point2 1 1) $ rectangle (Point2 1 1) ) )
                ]

concentricSquares3 :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
concentricSquares3 state = return $
        overlap [ (sTranslateXY 0 0 . solid (transparent 1.0 red   ) $ cSubtract (rectangle (Point2 10 10)) (sTranslate (Point2 2 2) $ rectangle (Point2 6 6) ) )
                , (sTranslateXY 2 2 . solid (transparent 1.0 green ) $ cSubtract (rectangle (Point2  6  6)) (sTranslate (Point2 2 2) $ rectangle (Point2 2 2) ) )
                , (sTranslateXY 4 4 . solid (transparent 1.0 blue  ) $            rectangle (Point2  2  2))
                ]

simpleGlyph :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
simpleGlyph state =
  do let character = chr $ state ^. stateStep
     characterGlyph <- getGlyph . CodePoint . ord $ character
     return .   sTranslateXY 0 0 .
                sRotate (6.28248 @@ rad) .
                sScale 20 .
                solid (transparent 1.0 white) $
                glyph characterGlyph

simpleArc :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
simpleArc state = return $
        sScale 100 .
        solid (transparent 1.0 $ dark gray) $
        arc (0.3 @@ turn)

sixPointRectangle :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
sixPointRectangle state = return $
        solid (transparent 1.0 (dark gray)) $
        raw [straight 0 0, straight 1 0, straight 2 0
            ,straight 2 1, straight 1 1, straight 0 1
            ]

tinySquare :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
tinySquare state = return $
        solid red $
        rectangle (Point2 2 2)

anotherThreshold :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
anotherThreshold state = return $
    solid white $
    overlap [ raw  [straight (-0.2) 0.0, straight   1.2  1.4
                   ,straight   1.2  1.6, straight (-0.2) 0.2
                   ]
            , raw  [straight (-0.2) 0.4, straight   1.2  1.8
                   ,straight   1.2  2.0, straight (-0.2) 0.6
                   ]
            ]

subtractDiamond :: Monad m => BenchmarkState -> GlyphMonad m ShapeTree
subtractDiamond state = return $
    solid white $
    cSubtract (diamond $ makePoint 8 8) (sTranslateXY 2 0 $ diamond $ makePoint 8 8)

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

mobyDick :: String
mobyDick = "Call me Ishmael. Some years ago--never mind how long precisely--having little or no money in my purse, and nothing particular to interest me on shore, I thought I would sail about a little and see the watery part of the world. It is a way I have of driving off the spleen and regulating the circulation. Whenever I find myself growing grim about the mouth; whenever it is a damp, drizzly November in my soul; whenever I find myself involuntarily pausing before coffin warehouses, and bringing up the rear of every funeral I meet; and especially whenever my hypos get such an upper hand of me, that it requires a strong moral principle to prevent me from deliberately stepping into the street, and methodically knocking people's hats off--then, I account it high time to get to sea as soon as I can. This is my substitute for pistol and ball. With a philosophical flourish Cato throws himself upon his sword; I quietly take to the ship. There is nothing surprising in this. If they but knew it, almost all men in their degree, some time or other, cherish very nearly the same feelings towards the ocean with me. There now is your insular city of the Manhattoes, belted round by wharves as Indian isles by coral reefs--commerce surrounds it with her surf. Right and left, the streets take you waterward. Its extreme downtown is the battery, where that noble mole is washed by waves, and cooled by breezes, which a few hours previous were out of sight of land. Look at the crowds of water-gazers there. Circumambulate the city of a dreamy Sabbath afternoon. Go from Corlears Hook to Coenties Slip, and from thence, by Whitehall, northward. What do you see?--Posted like silent sentinels all around the town, stand thousands upon thousands of mortal men fixed in ocean reveries. Some leaning against the spiles; some seated upon the pier-heads; some looking over the bulwarks of ships from China; some high aloft in the rigging, as if striving to get a still better seaward peep. But these are all landsmen; of week days pent up in lath and plaster--tied to counters, nailed to benches, clinched to desks. How then is this? Are the green fields gone? What do they here? But look! here come more crowds, pacing straight for the water, and seemingly bound for a dive. Strange! Nothing will content them but the extremest limit of the land; loitering under the shady lee of yonder warehouses will not suffice. No. They must get just as nigh the water as they possibly can without falling in. And there they stand--miles of them--leagues. Inlanders all, they come from lanes and alleys, streets and avenues--north, east, south, and west. Yet here they all unite. Tell me, does the magnetic virtue of the needles of the compasses of all those ships attract them thither?"
