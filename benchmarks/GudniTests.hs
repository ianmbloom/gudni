{-# LANGUAGE TemplateHaskell  #-}
module GudniTests
 ( BenchmarkState(..)
 , initialModel
 , stateBase
 , stateCursor
 , statePictureMap
 , stateTests
 , stateCurrentTest
 , stateFrameNumber
 , testList
 )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Interface.BasicSceneState
import Graphics.Gudni.Figure
import Graphics.Gudni.Application

import Graphics.Gudni.Layout

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Fuzzy

import Data.Word
import Data.Maybe
import Data.Char
import Control.Lens
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

import Control.Monad.Random
import System.Random

data BenchmarkState = BenchmarkState
  { _stateBase        :: BasicSceneState
  , _stateCursor      :: Point2 PixelSpace
  , _statePictureMap  :: PictureMap
  , _stateTests       :: [(String, BenchmarkState -> FontMonad IO (ShapeTree Int SubSpace))]
  , _stateCurrentTest :: Int
  }
makeLenses ''BenchmarkState

initialModel pictureMap =
    BenchmarkState
    { _stateBase = BasicSceneState
        { _stateScale       = 10
        , _stateDelta       = Point2 0 0
        , _stateAngle       = 0 @@ deg -- 0.02094 @@ rad -- 0 @@ turn-- quarterTurn
        , _statePaused      = True
        , _stateSpeed       = 0.1
        , _statePace        = 0.1
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 0
        , _stateFrameNumber = 0
        , _stateStep        = 69
        }
    , _stateCursor      = Point2 63 1376
    , _statePictureMap  = pictureMap
    , _stateTests       = testList
    , _stateCurrentTest = 8
    }

testList = [ ("openSquareOverlap3", openSquareOverlap3  ) --  0 -
           , ("benchmark1"        , benchmark1          ) --  1 -
           , ("fuzzy cookie"      , fuzzyDonut          ) --  2 -
           , ("fuzzy basic"       , fuzzyBasic          ) --  3 -
           , ("fuzzy circles"     , fuzzyCircles        ) --  4 -
           , ("fuzzy squares"     , fuzzySquares        ) --  5 -
           , ("fuzzy glyphs"      , fuzzyGlyphs2        ) --  6 -
           , ("testPict"          , testPict            ) --  7 -
           , ("rectGrid"          , rectGrid            ) --  8 -
           , ("solidGrid"         , solidGrid           ) --  9 -
           , ("checkerBoard"      , checkerBoard        ) -- 10 -
           , ("openSquare"        , openSquare          ) -- 11 -
           , ("openSquareOverlap2", openSquareOverlap2  ) -- 12 -
           , ("stackOfSquares"    , stackOfSquares      ) -- 13 -
           , ("concentricSquares2", concentricSquares2  ) -- 14 -
           , ("concentricSquares3", concentricSquares3  ) -- 15 -
           , ("subtractDiamond "  , subtractDiamond     ) -- 16 -
           , ("simpleKnob"        , simpleKnob          ) -- 17 -
           , ("hourGlass"         , hourGlass           ) -- 18 -
           , ("simpleGlyph"       , simpleGlyph         ) -- 19 -
           , ("simpleArc"         , simpleArc           ) -- 20 -
           , ("sixPointRectangle" , sixPointRectangle   ) -- 21 -
           , ("tinySquare"        , tinySquare          ) -- 22 -
           , ("mediumSequare"     , mediumSquare        ) -- 23 -
           , ("simpleRectangle"   , simpleRectangle     ) -- 24 -
           , ("tallRectangle"     , tallRectangle       ) -- 25 -
           , ("twoBrackets"       , twoBrackets         ) -- 26 -
           , ("fuzzySquares2"     , fuzzySquares2       ) -- 27 -
           , ("maxThresholdTest"  , maxThresholdTest    ) -- 28 -
           , ("maxShapeTest"      , maxShapeTest        ) -- 29 -
           , ("fuzzyCircles2"     , fuzzyCircles2       ) -- 30 -
           , ("fullRectangle"     , fullRectangle       ) -- 31 -
           , ("1 Million Circles" , millionFuzzyCircles ) -- 32 -
           ]

maxThresholdTest :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
maxThresholdTest state =
    return .
    tTranslateXY (-1) 1 .
    tRotate (0.2 @@ rad) .
    tScale 0.1 .
    solid (transparent 1.0 . light $ purple) .
    overlap .
    makeGrid 2 1 2 .
    repeat .
    rectangle $
    Point2 1000 1

-- | Stack of very wide thin RGB rectangles useful for testing large numbers of shapes per tile and
-- subpixel geometry.
maxShapeTest ::  Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
maxShapeTest state =
    return .
    overlap .
    makeGrid 1 1 (state ^. stateBase . stateStep + 2000 + 1) .
    concat .
    repeat $
        [ solid (transparent 1.0 (pureRed    )) $ rectangle (Point2 10000 1)
        , solid (transparent 1.0 (pureGreen  )) $ rectangle (Point2 10000 1)
        , solid (transparent 1.0 (pureBlue   )) $ rectangle (Point2 10000 1)
        ]

-- | A fuzz test of random curves where the random points are all within a donut shape.
fuzzyDonut :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyDonut state = return $
               let time = view (stateBase . stateLastTime) state
               in  tTranslateXY 500 500 .
                   overlap $ evalRand (sequence . replicate 16 $ fuzzyRadial 400 500 100) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | A basic fuzz test of random curves contained in a rectagular area.
fuzzyBasic :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyBasic state = return $
               let time = view (stateBase . stateLastTime) state
               in  tTranslateXY (100) (100) .
                   overlap $
                   evalRand (sequence . replicate 16 $ fuzzyCurve (makePoint 1440 900) 10) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | A random field of transparent circles.
fuzzyCircles :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyCircles state = return $
               let time = view (stateBase . stateLastTime) state
               in  --tTranslateXY (100) (100) .
                   --tScale 0.5 .
                   overlap $
                   evalRand (sequence . replicate 100000 $ fuzzyCircle (makePoint 5760 3600) 5 50) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | Smaller random field of transparent circles.
fuzzyCircles2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyCircles2 state = return $
               let time = view (stateBase . stateLastTime) state
               in  tTranslateXY 0 0 .
                   overlap $
                   evalRand (sequence . replicate (state ^. stateBase . stateStep) $ fuzzyCircle (makePoint 200 200) 5 10) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | A random field of transparent circles.
millionFuzzyCircles :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
millionFuzzyCircles state = return $
               let time = view (stateBase . stateLastTime) state
               in  --tTranslateXY (100) (100) .
                   tScale 0.5 .
                   overlap $
                   evalRand (sequence . replicate 1000000 $ fuzzyCircle (makePoint 5760 3600) 5 10) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | A random field of transparent squares.
fuzzySquares :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzySquares state = return $
               let time = view (stateBase . stateLastTime) state
               in  overlap $
                   evalRand (sequence . replicate 5000 $ fuzzySquare (makePoint 1440 900) 10 60) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | Smaller random field of transparent squares.
fuzzySquares2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzySquares2 state = return $
               let time = view (stateBase . stateLastTime) state
               in  overlap $
                   evalRand (sequence . replicate 2000 $ fuzzySquare (makePoint 300 300) 10 60) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

fuzzyGlyphs :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyGlyphs state =
  do defaultGlyphs <- V.fromList <$> glyphString ['!'..'z']
     let len = length defaultGlyphs
     return $  let time = view (stateBase . stateLastTime) state
               in  overlap $
                   evalRand (sequence . replicate 10000 $ fuzzyGlyph defaultGlyphs (makePoint 2880 1800) 10 300) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

fuzzyGlyphs2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyGlyphs2 state =
  do defaultGlyphs <- V.fromList <$> glyphString ['!'..'z']
     let len = length defaultGlyphs
     return $  let time = view (stateBase . stateLastTime) state
               in  tTranslateXY 200 200 .
                   overlap $
                   evalRand (sequence . replicate 200 $ fuzzyGlyph defaultGlyphs (makePoint 200 200) 10 200) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | A grid of rotating glyphs with overlapping subtracted glyphs
benchmark1 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
benchmark1 state =
    do defaultGlyphs <- glyphString "***O***X" --"ALl WoRk AnD nO pLaY mAkEs JaCk A dUlL bOy" ++ ['a'..'z']++['A'..'Z']++['0'..'9']++"!@#$%^&*()_+-={}[]:;\"'<>,./?\\|"
       subtractorGlyphs <- glyphString "*"
       let dSize         = state ^. stateBase . stateScale
           angle         = normalizeAngle ((((fromIntegral $ state ^. stateBase . stateStep) / 100) + (state ^. stateBase . statePlayhead)) @@ turn)
           defaultString = cycle $ defaultGlyphs
           angles        = {-repeat (0 @@ deg) -} map (normalizeAngle . (angle ^+^) . (^*0.5) . (@@ deg) ) [0..]
           textGrid  :: CompoundTree SubSpace
           textGrid   = tScale dSize . overlap . makeGrid 0.5 50 50 . zipWith (\a -> tRotate a) angles $ defaultString
           subtractor:: CompoundTree SubSpace
           subtractor = tScale (dSize * 15) . overlap . makeGrid 0.5 4 4 . cycle $ subtractorGlyphs
       return $ tTranslateXY 0 0 .
                tScale 50 .
                solid (transparent 1.0 black) $
                cSubtract textGrid subtractor

-- | A grid of rectangles.
rectGrid :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
rectGrid state = return $
    let grid  :: CompoundTree SubSpace
        grid   = overlap . makeGrid 1 200 200 . repeat . rectangle $ Point2 0.5 0.5
    in
        solid (transparent 1.0 white) $
        grid

-- | A grid of rectangles in direct contact
solidGrid :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
solidGrid state = return $
    let grid  :: CompoundTree SubSpace
        grid   = overlap . makeGrid 1 200 200 . repeat . rectangle $ Point2 1 1
    in
        solid (transparent 1.0 white) $
        grid

-- | A grid of rectangles.
checkerBoard :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
checkerBoard state = return $
    let grid  :: CompoundTree SubSpace
        grid = overlap . makeGrid 1 200 200 . repeat $ overlap[                        rectangle $ Point2 0.5 0.5
                                                              , tTranslateXY 0.5 0.5 . rectangle $ Point2 0.5 0.5
                                                              ]
    in
        solid (transparent 1.0 white) $
        grid

-- | A knob is a vertical curve section whose control point sticks out further in the x direction than it's other points
simpleKnob :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
simpleKnob state = return $
        tTranslateXY 100 100 .
        tScale 100 .
        solid (transparent 1.0 (dark $ dark gray)) .
        fromSegments $
        [ curved 0 0 1 1
        , straight 0 2
        ]

-- | Test shape for intersecting thresholds.
hourGlass :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
hourGlass state = return $
  let step = fromIntegral $ state ^. stateBase . stateStep
  in    tTranslateXY 0 0 .
        tTranslateXY 0.5 0.52 .
        tScale 8 .
        solid (transparent 1.0 (dark $ dark gray)) .
        fromSegments $
        [ straight 0 0
        , straight 1 1
        , straight 1 0
        , straight 0 1
        ]

-- | Test for loading a texture.
testPict :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
testPict state =
    let w = 200
        h = 200
        f (Point2 x y) = hslColor 0 (fromIntegral x / fromIntegral w) (fromIntegral y / fromIntegral h)
        size = Point2 w h
    in  return $
        overlap [ tTranslateXY 100 50 $ textureWith (SharedTexture "flowers") $ cSubtract (tScale 200 circle) (cAdd (tTranslateXY 100 100 $ tScale 100 circle) (tScale 100 circle))
                , tTranslateXY 100 50 $ textureWith (NewTexture "gradient" (PictureFunction f size)) $ tTranslateXY 0 0 $ (tScale 200 circle)
                , tTranslateXY 100 50 $ solid (transparent 0.2 blue) $ rectangle (Point2 40 2000)
                ]

-- | Simple stack of squares.
stackOfSquares :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
stackOfSquares state = return $
        overlap
          [ (tTranslateXY 0 0  . solid (transparent 1.0 red    ) $ rectangle (Point2 4 4) )
          , (tTranslateXY 0 4  . solid (transparent 1.0 green  ) $ rectangle (Point2 4 4) )
          --, (tTranslateXY 0 8  . solid (transparent 1.0 blue   ) $ rectangle (Point2 4 4) )
          --, (tTranslateXY 0 12 . solid (transparent 1.0 purple ) $ rectangle (Point2 4 4) )
          ]

-- | Basic test for shape subtraction.
openSquare :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
openSquare state = return $
    solid (transparent 0.5 orange) $
          cSubtract (rectangle (Point2 5 5))
                    (tTranslate (Point2 1 1) $ rectangle (Point2 3 3))

-- | Basic test for shape subtraction and transparency.
openSquareOverlap2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
openSquareOverlap2 state = return $
        tScale 20 $
        overlap [ (tTranslateXY 0 0 . solid (transparent 0.25 blue  ) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (tTranslateXY 8 8 . solid (transparent 1.0  orange) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                ]

-- | Basic test for shape subtraction and muliple transparency.
openSquareOverlap3 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
openSquareOverlap3 state = return $
    let angle = view (stateBase . statePlayhead) state @@ turn
    in  tTranslateXY 400 400 .
        tScale 50 $
        overlap [ (tTranslateXY 0 0 . tRotate angle        $ solid (transparent 0.5 blue   ) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (tTranslateXY 4 4 . tRotate (angle ^/ 2) $ solid (transparent 0.5 orange ) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                , (tTranslateXY 8 8 . tRotate (angle ^/ 3) $ solid (transparent 0.5 green  ) $ cSubtract (rectangle (Point2 16 16)) (tTranslate (Point2 4 4) $ rectangle (Point2 8 8) ) )
                ]

-- | Test for shape edges that abut.
concentricSquares2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
concentricSquares2 state = return $
        overlap [ (tTranslateXY 0 0 . solid (transparent 1.0 red ) $ cSubtract (rectangle (Point2 5 5)) (tTranslate (Point2 1 1) $ rectangle (Point2 3 3) ) )
                , (tTranslateXY 1 1 . solid (transparent 1.0 blue) $ cSubtract (rectangle (Point2 3 3)) (tTranslate (Point2 1 1) $ rectangle (Point2 1 1) ) )
                ]

-- | Another test for shape edges that abut.
concentricSquares3 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
concentricSquares3 state = return $
        overlap [ (tTranslateXY 0 0 . solid (transparent 1.0 red   ) $ cSubtract (rectangle (Point2 10 10)) (tTranslate (Point2 2 2) $ rectangle (Point2 6 6) ) )
                , (tTranslateXY 2 2 . solid (transparent 1.0 green ) $ cSubtract (rectangle (Point2  6  6)) (tTranslate (Point2 2 2) $ rectangle (Point2 2 2) ) )
                , (tTranslateXY 4 4 . solid (transparent 1.0 blue  ) $            rectangle (Point2  2  2))
                ]

-- | Simple test one glyph.
simpleGlyph :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
simpleGlyph state =
     let character = chr $ state ^. stateBase . stateStep in
     tTranslateXY 0 0 .
     tRotate (6.28248 @@ rad) .
     tScale 20 .
     solid (transparent 1.0 white) .
     glyph . CodePoint . ord $ character

-- | Simple test one arc.
simpleArc :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
simpleArc state = return $
        tScale 100 .
        solid (transparent 1.0 $ dark gray) $
        arc (0.3 @@ turn)

-- | Test for straight vertical segments with multiple colinear points.
sixPointRectangle :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
sixPointRectangle state = return $
        solid (transparent 1.0 (dark gray)) $
        fromSegments [straight 0 0, straight 1 0, straight 2 0
                     ,straight 2 1, straight 1 1, straight 0 1
                     ]

-- | Very tiny square with no rotation. Usually the first thing tested for a new build.
tinySquare :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
tinySquare state = return $
        tTranslateXY 0.1 0.1 .
        solid red $
        rectangle (Point2 2 2)

-- | Medium sized square with no rotation.
mediumSquare :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
mediumSquare state = return $
        tTranslateXY 0.1 0.1 .
        solid red $
        rectangle (Point2 10 10)

-- | Medium sized square with no rotation.
fullRectangle :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fullRectangle state = return $
        tTranslateXY 0 0 .
        solid red $
        rectangle (makePoint 2880 1800)

-- | Very simple rotated box.
simpleRectangle :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
simpleRectangle state = return $
  tTranslateXY (0.35 + 0.1) 0 .
  tRotate (45 @@ deg) .
  tScale 1 .
  solid (transparent 1.0 white) $
  rectangle (Point2 2 2)

-- | Simple rectangle test across multiple tiles.
tallRectangle :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
tallRectangle state = return $
        tTranslateXY 0 1 .
        tRotate (3 @@ deg) .
        solid (transparent 1.0 white) $
        rectangle (Point2 0.5 2000)

-- | Simple multishape test useful for subpixel geometry.
twoBrackets :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
twoBrackets state = return $
    solid white $
    overlap [ fromSegments [straight (-0.2) 0.0, straight   1.2  1.4
                           ,straight   1.2  1.6, straight (-0.2) 0.2
                           ]
            , fromSegments [straight (-0.2) 0.4, straight   1.2  1.8
                           ,straight   1.2  2.0, straight (-0.2) 0.6
                           ]
            ]

-- | Very simple subtraction test.
subtractDiamond :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
subtractDiamond state = return .
    tRotate (45 @@ deg) .
    solid white $
    cSubtract unitSquare (tTranslateXY 1 1 $ unitSquare)

instance Show BenchmarkState where
  show state =
     "BenchmarkState { " ++
     show (state ^. stateBase       ) ++ ", " ++
     show (state ^. stateCursor     ) ++ ", " ++
     show (state ^. stateCurrentTest) ++  " }"
