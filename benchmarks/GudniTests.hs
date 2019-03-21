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
import Graphics.Gudni.Util.Draw
import Graphics.Gudni.Util.Plot

import Data.Word
import Data.Maybe
import Data.Char
import Control.Lens
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

import Control.Monad.Random
import System.Random


data BenchmarkState = BenchmarkState
  { _stateScale       :: SubSpace
  , _stateDelta       :: Point2 SubSpace
  , _stateCircleDelta :: Point2 SubSpace
  , _stateAngle       :: Angle  SubSpace
  , _statePaused      :: Bool
  , _stateSpeed       :: SubSpace
  , _statePace        :: SubSpace
  , _stateLastTime    :: SimpleTime
  , _stateDirection   :: Bool
  , _statePlayhead    :: SubSpace
  , _stateCursor      :: Point2 PixelSpace
  , _statePictures    :: (VS.Vector Word8, [PictureMemoryReference])
  , _stateTests       :: [(String, BenchmarkState -> GlyphMonad IO (ShapeTree Int))]
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
    , _statePace        = 0.1
    , _stateLastTime    = 0
    , _stateDirection   = True
    , _statePlayhead    = 0
    , _stateCursor      = Point2 63 1376
    , _statePictures    = pictures
    , _stateTests       = testList
    , _stateCurrentTest = 4
    , _stateStep        = 69
    , _stateFrameNumber = 0
    }

testList = [ ("openSquareOverlap3", openSquareOverlap3  ) --  0 -
           , ("benchmark1"        , benchmark1          ) --  1 -
           , ("fuzzy cookie"      , fuzzyDonut          ) --  2 -
           , ("fuzzy basic"       , fuzzyBasic          ) --  3 -
           , ("fuzzy circles"     , fuzzyCircles        ) --  4 -
           , ("fuzzy squares"     , fuzzySquares        ) --  5 -
           , ("fuzzy glyphs"      , fuzzyGlyphs2         ) --  6 -
           , ("testPict"          , testPict            ) --  7 -
           , ("rectGrid"          , rectGrid            ) --  8 -
           , ("solidGrid"         , solidGrid           ) --  9 -
           , ("checkerBoard"      , checkerBoard        ) -- 10 -
           , ("plotter test"      , plots               ) -- 11 -
           , ("openSquare"        , openSquare          ) -- 12 -
           , ("openSquareOverlap2", openSquareOverlap2  ) -- 13 -
           , ("stackOfSquares"    , stackOfSquares      ) -- 14 -
           , ("concentricSquares2", concentricSquares2  ) -- 15 -
           , ("concentricSquares3", concentricSquares3  ) -- 16 -
           , ("subtractDiamond "  , subtractDiamond     ) -- 17 -
           , ("simpleKnob"        , simpleKnob          ) -- 18 -
           , ("hourGlass"         , hourGlass           ) -- 19 -
           , ("simpleGlyph"       , simpleGlyph         ) -- 20 -
           , ("simpleArc"         , simpleArc           ) -- 21 -
           , ("sixPointRectangle" , sixPointRectangle   ) -- 22 -
           , ("tinySquare"        , tinySquare          ) -- 23 -
           , ("mediumSequare"     , mediumSquare        ) -- 24 -
           , ("simpleRectangle"   , simpleRectangle     ) -- 25 -
           , ("tallRectangle"     , tallRectangle       ) -- 26 -
           , ("twoBrackets"       , twoBrackets         ) -- 27 -
           , ("fuzzySquares2"     , fuzzySquares2       ) -- 28 -
           , ("maxThresholdTest"  , maxThresholdTest    ) -- 29 -
           , ("maxShapeTest"      , maxShapeTest        ) -- 30 -
           , ("fuzzyCircles2"     , fuzzyCircles2       ) -- 31 -
           , ("fullRectangle"     , fullRectangle       ) -- 32 -
           ]

maxThresholdTest :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
maxThresholdTest state =
    return $
    let frame = view stateFrameNumber state
    in
    sTranslateXY (-1) 1 .
    sRotate (0.2 @@ rad) .
    sScale 0.1 .
    solid (transparent 1.0 . light $ purple) .
    makeGrid 2 1 2 $ repeat (rectangle (Point2 1000 1))

-- | Stack of very wide thin RGB rectangles useful for testing large numbers of shapes per tile and
-- subpixel geometry.
maxShapeTest :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
maxShapeTest state =
    return .
    makeGrid 1 1 (state ^. stateStep + 2000 + 1) .
    concat .
    repeat $
        [ (solid (transparent 1.0 (pureRed    )) $ rectangle (Point2 10000 1))
        , (solid (transparent 1.0 (pureGreen  )) $ rectangle (Point2 10000 1))
        , (solid (transparent 1.0 (pureBlue   )) $ rectangle (Point2 10000 1))
        ]

-- | All the turtle plots from the plot module.
plots :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
plots state = return $
              sTranslateXY 100 100 . sScale 30 . makeGrid 10 16 1 . catMaybes . map (fmap (solid yellow . rawCurve) . curveLibrary) $ turtleNames

-- | A fuzz test of random curves where the random points are all within a donut shape.
fuzzyDonut :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fuzzyDonut state = return $
               let time = view stateLastTime state
               in  sTranslateXY 500 500 .
                   overlap $ evalRand (sequence . replicate 16 $ fuzzyRadial 400 500 100) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

-- | A basic fuzz test of random curves contained in a rectagular area.
fuzzyBasic :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fuzzyBasic state = return $
               let time = view stateLastTime state
               in  sTranslateXY (100) (100) .
                   overlap $
                   evalRand (sequence . replicate 16 $ fuzzyCurve (makePoint 1440 900) 10) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

-- | A random field of transparent circles.
fuzzyCircles :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fuzzyCircles state = return $
               let time = view stateLastTime state
               in  --sTranslateXY (100) (100) .
                   --sScale 0.5 .
                   overlap $
                   evalRand (sequence . replicate 50000 $ fuzzyCircle (makePoint 5760 3600) 10 50) (mkStdGen $ (round $ state ^. statePlayhead * 2000))

-- | Smaller random field of transparent circles.
fuzzyCircles2 :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fuzzyCircles2 state = return $
               let time = view stateLastTime state
               in  sTranslateXY 0 0 .
                   overlap $
                   evalRand (sequence . replicate (state ^. stateStep) $ fuzzyCircle (makePoint 200 200) 5 10) (mkStdGen $ (round $ state ^. statePlayhead * 2000))

-- | A random field of transparent squares.
fuzzySquares :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fuzzySquares state = return $
               let time = view stateLastTime state
               in  overlap $
                   evalRand (sequence . replicate 5000 $ fuzzySquare (makePoint 1440 900) 10 60) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

-- | Smaller random field of transparent squares.
fuzzySquares2 :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fuzzySquares2 state = return $
               let time = view stateLastTime state
               in  overlap $
                   evalRand (sequence . replicate 2000 $ fuzzySquare (makePoint 300 300) 10 60) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

fuzzyGlyphs :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fuzzyGlyphs state =
  do defaultGlyphs <- V.fromList <$> glyphString ['!'..'z']
     let len = length defaultGlyphs
     return $
               let time = view stateLastTime state
               in  overlap $
                   evalRand (sequence . replicate 10000 $ fuzzyGlyph defaultGlyphs (makePoint 2880 1800) 10 300) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

fuzzyGlyphs2 :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fuzzyGlyphs2 state =
  do defaultGlyphs <- V.fromList <$> glyphString ['!'..'z']
     let len = length defaultGlyphs
     return $
               let time = view stateLastTime state
               in  sTranslateXY 200 200 .
                   overlap $
                   evalRand (sequence . replicate 200 $ fuzzyGlyph defaultGlyphs (makePoint 200 200) 10 200) (mkStdGen $ (round $ state ^. statePlayhead * 2000) + (state ^. stateStep))

-- | A grid of rotating glyphs with overlapping subtracted glyphs
benchmark1 :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
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

-- | A grid of rectangles.
rectGrid :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
rectGrid state = return $
    let grid  :: CompoundTree
        grid   = makeGrid 1 200 200 . repeat . rectangle $ Point2 0.5 0.5
    in
        solid (transparent 1.0 white) $
        grid

-- | A grid of rectangles in direct contact
solidGrid :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
solidGrid state = return $
    let grid  :: CompoundTree
        grid   = makeGrid 1 200 200 . repeat . rectangle $ Point2 1 1
    in
        solid (transparent 1.0 white) $
        grid

-- | A grid of rectangles.
checkerBoard :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
checkerBoard state = return $
    let grid  :: CompoundTree
        grid   = makeGrid 1 200 200 . repeat $ overlap[ rectangle $ Point2 0.5 0.5
                                                      , sTranslateXY 0.5 0.5 . rectangle $ Point2 0.5 0.5
                                                      ]
    in
        solid (transparent 1.0 white) $
        grid

-- | A knob is a vertical curve section whose control point sticks out further in the x direction than it's other points
simpleKnob :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
simpleKnob state = return $
        sTranslateXY 100 100 .
        sScale 100 .

        solid (transparent 1.0 (dark $ dark gray)) .
        raw $
        [ curved 0 0 1 1
        , straight 0 2
        ]

-- | Test shape for intersecting thresholds.
hourGlass :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
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

-- | Test for loading a texture.
testPict :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
testPict state = return $
        overlap [ textureWith (PictureUsage (Point2 0 0) 0) $ sTranslateXY 100 100 (cSubtract (sScale 200 circle) (sScale 100 circle))
                , sTranslateXY 50 50 $ solid (transparent 0.2 blue) $ rectangle (Point2 40  2000)]

-- | Simple stack of squares.
stackOfSquares :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
stackOfSquares state = return $
        overlap
          [ (sTranslateXY 0 0  . solid (transparent 1.0 red    ) $ rectangle (Point2 4 4) )
          , (sTranslateXY 0 4  . solid (transparent 1.0 green  ) $ rectangle (Point2 4 4) )
          --, (sTranslateXY 0 8  . solid (transparent 1.0 blue   ) $ rectangle (Point2 4 4) )
          --, (sTranslateXY 0 12 . solid (transparent 1.0 purple ) $ rectangle (Point2 4 4) )
          ]

-- | Basic test for shape subtraction.
openSquare :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
openSquare state = return $
    solid (transparent 0.5 orange) $
          cSubtract (rectangle (Point2 5 5))
                    (sTranslate (Point2 1 1) $ rectangle (Point2 3 3))

-- | Basic test for shape subtraction and transparency.
openSquareOverlap2 :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
openSquareOverlap2 state = return $
        sScale 0.25 $
        overlap [ (sTranslateXY 0 0 . solid (transparent 0.25 blue  ) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (sTranslateXY 8 8 . solid (transparent 1.0  orange) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                ]

-- | Basic test for shape subtraction and muliple transparency.
openSquareOverlap3 :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
openSquareOverlap3 state = return $
    let angle = view statePlayhead state @@ turn
    in  sTranslateXY 400 400 .
        sScale 50 $
        overlap [ (sTranslateXY 0 0 . sRotate angle        $ solid (transparent 0.5 blue   ) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (sTranslateXY 4 4 . sRotate (angle ^/ 2) $ solid (transparent 0.5 orange ) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (sTranslateXY 8 8 . sRotate (angle ^/ 3) $ solid (transparent 0.5 green  ) $ cSubtract (rectangle (Point2 16 16)) (sTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                ]

-- | Test for shape edges that abut.
concentricSquares2 :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
concentricSquares2 state = return $
        overlap [ (sTranslateXY 0 0 . solid (transparent 1.0 red ) $ cSubtract (rectangle (Point2 5 5)) (sTranslate (Point2 1 1) $ rectangle (Point2 3 3) ) )
                , (sTranslateXY 1 1 . solid (transparent 1.0 blue) $ cSubtract (rectangle (Point2 3 3)) (sTranslate (Point2 1 1) $ rectangle (Point2 1 1) ) )
                ]

-- | Another test for shape edges that abut.
concentricSquares3 :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
concentricSquares3 state = return $
        overlap [ (sTranslateXY 0 0 . solid (transparent 1.0 red   ) $ cSubtract (rectangle (Point2 10 10)) (sTranslate (Point2 2 2) $ rectangle (Point2 6 6) ) )
                , (sTranslateXY 2 2 . solid (transparent 1.0 green ) $ cSubtract (rectangle (Point2  6  6)) (sTranslate (Point2 2 2) $ rectangle (Point2 2 2) ) )
                , (sTranslateXY 4 4 . solid (transparent 1.0 blue  ) $            rectangle (Point2  2  2))
                ]

-- | Simple test one glyph.
simpleGlyph :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
simpleGlyph state =
  do let character = chr $ state ^. stateStep
     characterGlyph <- getGlyph . CodePoint . ord $ character
     return .   sTranslateXY 0 0 .
                sRotate (6.28248 @@ rad) .
                sScale 20 .
                solid (transparent 1.0 white) $
                glyph characterGlyph

-- | Simple test one arc.
simpleArc :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
simpleArc state = return $
        sScale 100 .
        solid (transparent 1.0 $ dark gray) $
        arc (0.3 @@ turn)

-- | Test for straight vertical segments with multiple colinear points.
sixPointRectangle :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
sixPointRectangle state = return $
        solid (transparent 1.0 (dark gray)) $
        raw [straight 0 0, straight 1 0, straight 2 0
            ,straight 2 1, straight 1 1, straight 0 1
            ]

-- | Very tiny square with no rotation. Usually the first thing tested for a new build.
tinySquare :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
tinySquare state = return $
        sTranslateXY 0.3 0.3 .
        solid red $
        rectangle (Point2 2 2)

-- | Medium sized square with no rotation.
mediumSquare :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
mediumSquare state = return $
        sTranslateXY 0.1 0.1 .
        solid red $
        rectangle (Point2 10 10)

-- | Medium sized square with no rotation.
fullRectangle :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
fullRectangle state = return $
        sTranslateXY 0 0 .
        solid red $
        rectangle (makePoint 2880 1800)

-- | Very simple rotated box.
simpleRectangle :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
simpleRectangle state = return $
  sTranslateXY (0.35 + 0.1) 0 .
  sRotate (45 @@ deg) .
  sScale 1 .
  solid (transparent 1.0 white) $
  rectangle (Point2 2 2)

-- | Simple rectangle test across multiple tiles.
tallRectangle :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
tallRectangle state = return $
        sTranslateXY 0 1 .
        sRotate (3 @@ deg) .
        solid (transparent 1.0 white) $
        rectangle (Point2 0.5 2000)

-- | Simple multishape test useful for subpixel geometry.
twoBrackets :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
twoBrackets state = return $
    solid white $
    overlap [ raw  [straight (-0.2) 0.0, straight   1.2  1.4
                   ,straight   1.2  1.6, straight (-0.2) 0.2
                   ]
            , raw  [straight (-0.2) 0.4, straight   1.2  1.8
                   ,straight   1.2  2.0, straight (-0.2) 0.6
                   ]
            ]

-- | Very simple subtraction test.
subtractDiamond :: Monad m => BenchmarkState -> GlyphMonad m (ShapeTree Int)
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
     show (state ^. stateStep       ) {-++ ", " ++
     show (state ^. statePictures   )-} ++ " }"

-- | Sample text paragraph.
mobyDick :: String
mobyDick = "Call me Ishmael. Some years ago--never mind how long precisely--having little or no money in my purse, and nothing particular to interest me on shore, I thought I would sail about a little and see the watery part of the world. It is a way I have of driving off the spleen and regulating the circulation. Whenever I find myself growing grim about the mouth; whenever it is a damp, drizzly November in my soul; whenever I find myself involuntarily pausing before coffin warehouses, and bringing up the rear of every funeral I meet; and especially whenever my hypos get such an upper hand of me, that it requires a strong moral principle to prevent me from deliberately stepping into the street, and methodically knocking people's hats off--then, I account it high time to get to sea as soon as I can. This is my substitute for pistol and ball. With a philosophical flourish Cato throws himself upon his sword; I quietly take to the ship. There is nothing surprising in this. If they but knew it, almost all men in their degree, some time or other, cherish very nearly the same feelings towards the ocean with me. There now is your insular city of the Manhattoes, belted round by wharves as Indian isles by coral reefs--commerce surrounds it with her surf. Right and left, the streets take you waterward. Its extreme downtown is the battery, where that noble mole is washed by waves, and cooled by breezes, which a few hours previous were out of sight of land. Look at the crowds of water-gazers there. Circumambulate the city of a dreamy Sabbath afternoon. Go from Corlears Hook to Coenties Slip, and from thence, by Whitehall, northward. What do you see?--Posted like silent sentinels all around the town, stand thousands upon thousands of mortal men fixed in ocean reveries. Some leaning against the spiles; some seated upon the pier-heads; some looking over the bulwarks of ships from China; some high aloft in the rigging, as if striving to get a still better seaward peep. But these are all landsmen; of week days pent up in lath and plaster--tied to counters, nailed to benches, clinched to desks. How then is this? Are the green fields gone? What do they here? But look! here come more crowds, pacing straight for the water, and seemingly bound for a dive. Strange! Nothing will content them but the extremest limit of the land; loitering under the shady lee of yonder warehouses will not suffice. No. They must get just as nigh the water as they possibly can without falling in. And there they stand--miles of them--leagues. Inlanders all, they come from lanes and alleys, streets and avenues--north, east, south, and west. Yet here they all unite. Tell me, does the magnetic virtue of the needles of the compasses of all those ships attract them thither?"
