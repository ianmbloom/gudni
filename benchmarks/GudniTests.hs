{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Graphics.Gudni.Util.Segment

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
        { _stateScale       = 1
        , _stateDelta       = Point2 (-1) 1
        , _stateAngle       = 0 @@ deg
        , _statePaused      = True
        , _stateSpeed       = 0.1
        , _statePace        = 20
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 0
        , _stateFrameNumber = 0
        , _stateStep        = 100
        , _stateRepMode     = False
        , _stateRepDk       = False
        }
    , _stateCursor      = Point2 63 1376
    , _statePictureMap  = pictureMap
    , _stateTests       = testList
    , _stateCurrentTest = 6
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
           , ("sixPointRectangle" , sixPointRectangle   ) -- 20 -
           , ("tinySquare"        , tinySquare          ) -- 21 -
           , ("mediumSequare"     , mediumSquare        ) -- 22 -
           , ("simpleRectangle"   , simpleRectangle     ) -- 23 -
           , ("tallRectangle"     , tallRectangle       ) -- 24 -
           , ("twoBrackets"       , twoBrackets         ) -- 25 -
           , ("fuzzySquares2"     , fuzzySquares2       ) -- 26 -
           , ("maxThresholdTest"  , maxThresholdTest    ) -- 27 -
           , ("maxShapeTest"      , maxShapeTest        ) -- 28 -
           , ("fuzzyCircles2"     , fuzzyCircles2       ) -- 29 -
           , ("fullRectangle"     , fullRectangle       ) -- 30 -
           , ("1 Million Circles" , millionFuzzyCircles ) -- 31 -
           ]

maxThresholdTest :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
maxThresholdTest state =
    return .
    translateByXY (-1) 1 .
    rotateBy (0.2 @@ rad) .
    scaleBy 0.1 .
    colorWith (transparent 1.0 . light $ purple) .
    overlap .
    gridOf 2 1 2 .
    repeat .
    rectangle $
    Point2 1000 1

-- | Stack of very wide thin RGB rectangles useful for testing large numbers of shapes per tile and
-- subpixel geometry.
maxShapeTest ::  Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
maxShapeTest state =
    return .
    overlap .
    gridOf 1 1 (state ^. stateBase . stateStep + 2000 + 1) .
    concat .
    repeat $
        [ colorWith (transparent 1.0 (pureRed    )) $ rectangle (Point2 10000 1)
        , colorWith (transparent 1.0 (pureGreen  )) $ rectangle (Point2 10000 1)
        , colorWith (transparent 1.0 (pureBlue   )) $ rectangle (Point2 10000 1)
        ]

-- | A fuzz test of random curves where the random points are all within a donut shape.
fuzzyDonut :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyDonut state = return $
               let time = view (stateBase . stateLastTime) state
               in  translateByXY 500 500 .
                   overlap $ evalRand (sequence . replicate 16 $ fuzzyRadial 400 500 100) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | A basic fuzz test of random curves contained in a rectagular area.
fuzzyBasic :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyBasic state = return $
               let time = view (stateBase . stateLastTime) state
               in  translateByXY (100) (100) .
                   overlap $
                   evalRand (sequence . replicate 16 $ fuzzyCurve (makePoint 1440 900) 10) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | A random field of transparent circles.
fuzzyCircles :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyCircles state = return $
               let time = view (stateBase . stateLastTime) state
               in  --translateByXY (100) (100) .
                   --scaleBy 0.5 .
                   overlap $
                   evalRand (sequence . replicate 100000 $ fuzzyCircle (makePoint 5760 3600) 5 50) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | Smaller random field of transparent circles.
fuzzyCircles2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyCircles2 state = return $
               let time = view (stateBase . stateLastTime) state
               in  translateByXY 0 0 .
                   overlap $
                   evalRand (sequence . replicate (state ^. stateBase . stateStep) $ fuzzyCircle (makePoint 20 20) 5 10) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | A random field of transparent circles.
millionFuzzyCircles :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
millionFuzzyCircles state = return $
               let time = view (stateBase . stateLastTime) state
               in  --translateByXY (100) (100) .
                   scaleBy 0.5 .
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
                   evalRand (sequence . replicate 20 $ fuzzySquare (makePoint 30 30) 10 60) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

fuzzyGlyphs :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyGlyphs state =
  do defaultGlyphs <- V.fromList <$> glyphString ['!'..'z']
     let len = length defaultGlyphs
     return $  let time = view (stateBase . stateLastTime) state
               in  overlap $
                   evalRand (sequence . replicate 10000 $ fuzzyGlyph defaultGlyphs (2 * (makePoint 2880 1800)) 10 300) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

fuzzyGlyphs2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fuzzyGlyphs2 state =
  do defaultGlyphs <- V.fromList <$> glyphString ['!'..'z']
     let len = length defaultGlyphs
     return $  let time = view (stateBase . stateLastTime) state
               in  translateByXY 200 200 .
                   overlap $
                   evalRand (sequence . replicate 2000 $ fuzzyGlyph defaultGlyphs (makePoint 800 800) 10 200) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

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
           textGrid   = scaleBy dSize . overlap . gridOf 0.5 50 50 . zipWith (\a -> rotateBy a) angles $ defaultString
           subtractor:: CompoundTree SubSpace
           subtractor = scaleBy (dSize * 15) . overlap . gridOf 0.5 4 4 . cycle $ subtractorGlyphs
       return $ translateByXY 0 0 .
                scaleBy 50 .
                colorWith (transparent 1.0 black) $
                subtractFrom subtractor textGrid

-- | A grid of rectangles.
rectGrid :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
rectGrid state = return $
    let grid  :: CompoundTree SubSpace
        grid   = overlap . gridOf 1 200 200 . repeat . rectangle $ Point2 0.5 0.5
    in
        colorWith (transparent 1.0 white) $
        grid

-- | A grid of rectangles in direct contact
solidGrid :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
solidGrid state = return $
    let grid  :: CompoundTree SubSpace
        grid   = overlap . gridOf 1 200 200 . repeat . rectangle $ Point2 1 1
    in
        colorWith (transparent 1.0 white) $
        grid

-- | A grid of rectangles.
checkerBoard :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
checkerBoard state = return $
    let grid  :: CompoundTree SubSpace
        grid = overlap . gridOf 1 200 200 . repeat $ overlap[                        rectangle $ Point2 0.5 0.5
                                                              , translateByXY 0.5 0.5 . rectangle $ Point2 0.5 0.5
                                                              ]
    in
        colorWith (transparent 1.0 white) $
        grid

-- | A knob is a vertical curve section whose control point sticks out further in the x direction than it's other points
simpleKnob :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
simpleKnob state = return $
        -- translateByXY 10 10 .
        scaleBy 10 .
        colorWith (transparent 1.0 (red)) .
        fromSegments $
        [ curvedXY 0 0 1 1
        , straightXY 0 2
        ]

-- | Test shape for intersecting thresholds.
hourGlass :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
hourGlass state = return $
  let step = fromIntegral $ state ^. stateBase . stateStep
  in    scaleBy 8 .
        colorWith (transparent 1.0 (dark $ dark gray)) .
        fromSegments $
        [ straightXY 0 0
        , straightXY 1 1
        , straightXY 1 0
        , straightXY 0 1
        ]

-- | Test for loading a texture.
testPict :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
testPict state =
    let w = 1000
        h = 300
        f (Point2 x y) = hslColor 0 (fromIntegral x / fromIntegral w) (fromIntegral y / fromIntegral h)
        size = Point2 w h
    in  return $
        overlap [ translateByXY 100 50 $ textureWith (SharedTexture "flowers") $ subtractFrom (scaleBy 200 circle) (addOver (translateByXY 100 100 $ scaleBy 100 circle) (scaleBy 100 circle))
                , translateByXY 100 50 $ textureWith (NewTexture "gradient" (PictureFunction f size)) $ translateByXY 0 50 $ (scaleBy 50 circle)
                -- , translateByXY 100 50 $ colorWith (transparent 0.2 blue) $ rectangle (Point2 40 2000)
                ]

-- | Simple stack of squares.
stackOfSquares :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
stackOfSquares state = return $
        overlap
          [ (translateByXY 0 0  . colorWith (transparent 1.0 red    ) $ rectangle (Point2 4 4) )
          , (translateByXY 0 4  . colorWith (transparent 1.0 green  ) $ rectangle (Point2 4 4) )
          --, (translateByXY 0 8  . colorWith (transparent 1.0 blue   ) $ rectangle (Point2 4 4) )
          --, (translateByXY 0 12 . colorWith (transparent 1.0 purple ) $ rectangle (Point2 4 4) )
          ]

-- | Basic test for shape subtraction.
openSquare :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
openSquare state = return $
    colorWith (transparent 1 orange) $
          subtractFrom (translateBy (Point2 1 1) $ rectangle (Point2 3 3))
                    (rectangle (Point2 5 5))


-- | Basic test for shape subtraction and transparency.
openSquareOverlap2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
openSquareOverlap2 state = return $
        scaleBy 20 $
        overlap [ (translateByXY 0 0 . colorWith (transparent 0.25 blue  ) $ subtractFrom (translateBy (Point2 4 4) $ rectangle (Point2 8 8) ) (rectangle (Point2 16 16)) )
                , (translateByXY 8 8 . colorWith (transparent 1.0  orange) $ subtractFrom (translateBy (Point2 4 4) $ rectangle (Point2 8 8) ) (rectangle (Point2 16 16)) )
                ]

-- | Basic test for shape subtraction and muliple transparency.
openSquareOverlap3 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
openSquareOverlap3 state = return $
    let angle = view (stateBase . statePlayhead) state @@ turn
    in  translateByXY 400 400 .
        scaleBy 50 $
        overlap [ (translateByXY 0 0 . rotateBy angle        $ colorWith (transparent 0.5 blue   ) $ subtractFrom (translateBy (Point2 4 4) $ rectangle (Point2 8 8) ) (rectangle (Point2 16 16)) )
                , (translateByXY 4 4 . rotateBy (angle ^/ 2) $ colorWith (transparent 0.5 orange ) $ subtractFrom (translateBy (Point2 4 4) $ rectangle (Point2 8 8) ) (rectangle (Point2 16 16)) )
                , (translateByXY 8 8 . rotateBy (angle ^/ 3) $ colorWith (transparent 0.5 green  ) $ subtractFrom (translateBy (Point2 4 4) $ rectangle (Point2 8 8) ) (rectangle (Point2 16 16)) )
                ]

-- | Test for shape edges that abut.
concentricSquares2 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
concentricSquares2 state = return $
        overlap [ (translateByXY 0 0 . colorWith (transparent 1.0 red ) $ subtractFrom (translateBy (Point2 1 1) $ rectangle (Point2 3 3) ) (rectangle (Point2 5 5)) )
                , (translateByXY 1 1 . colorWith (transparent 1.0 blue) $ subtractFrom (translateBy (Point2 1 1) $ rectangle (Point2 1 1) ) (rectangle (Point2 3 3)) )
                ]

-- | Another test for shape edges that abut.
concentricSquares3 :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
concentricSquares3 state = return $
        overlap [ (translateByXY 0 0 . colorWith (transparent 1.0 red   ) $ subtractFrom (translateBy (Point2 2 2) $ rectangle (Point2 6 6) ) (rectangle (Point2 10 10)) )
                , (translateByXY 2 2 . colorWith (transparent 1.0 green ) $ subtractFrom (translateBy (Point2 2 2) $ rectangle (Point2 2 2) ) (rectangle (Point2  6  6)) )
                , (translateByXY 4 4 . colorWith (transparent 1.0 blue  ) $                                                                 rectangle (Point2  2  2))
                ]

-- | Simple test one glyph.
simpleGlyph :: forall m . Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
simpleGlyph state =
     let character = chr $ state ^. stateBase . stateStep in
     do g <- glyph . CodePoint . ord $ character
        return $
            translateByXY 0 0 .
            rotateBy (6.28248 @@ rad) .
            scaleBy 20 .
            colorWith (transparent 1.0 white) $
            g

-- | Test for straight vertical segments with multiple colinear points.
sixPointRectangle :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
sixPointRectangle state = return $
        colorWith (transparent 1.0 (dark gray)) $
        fromSegments [straightXY 0 0, straightXY 1 0, straightXY 2 0
                     ,straightXY 2 1, straightXY 1 1, straightXY 0 1
                     ]

-- | Very tiny square with no rotation. Usually the first thing tested for a new build.
tinySquare :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
tinySquare state = return $
        translateByXY 0.1 0.1 .
        colorWith red $
        rectangle (Point2 2 2)

-- | Medium sized square with no rotation.
mediumSquare :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
mediumSquare state = return $
        translateByXY 0.1 0.1 .
        colorWith red $
        rectangle (Point2 10 10)

-- | Medium sized square with no rotation.
fullRectangle :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
fullRectangle state = return $
        translateByXY 0 0 .
        colorWith red $
        rectangle (makePoint 2880 1800)

-- | Very simple rotated box.
simpleRectangle :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
simpleRectangle state = return $
  translateByXY (0.35 + 0.1) 0 .
  rotateBy (45 @@ deg) .
  scaleBy 1 .
  colorWith (transparent 1.0 white) $
  rectangle (Point2 2 2)

-- | Simple rectangle test across multiple tiles.
tallRectangle :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
tallRectangle state = return $
        translateByXY 0 1 .
        rotateBy (3 @@ deg) .
        colorWith (transparent 1.0 white) $
        rectangle (Point2 0.5 2000)

-- | Simple multishape test useful for subpixel geometry.
twoBrackets :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
twoBrackets state = return $
    colorWith white $
    overlap [ fromSegments [straightXY (-0.2) 0.0, straightXY   1.2  1.4
                           ,straightXY   1.2  1.6, straightXY (-0.2) 0.2
                           ]
            , fromSegments [straightXY (-0.2) 0.4, straightXY   1.2  1.8
                           ,straightXY   1.2  2.0, straightXY (-0.2) 0.6
                           ]
            ]

unitSquare = rectangle $ Point2 1 1
-- | Very simple subtraction test.
subtractDiamond :: Monad m => BenchmarkState -> FontMonad m (ShapeTree Int SubSpace)
subtractDiamond state = return .
    rotateBy (45 @@ deg) .
    colorWith white $
    subtractFrom (translateByXY 1 1 $ unitSquare) unitSquare

instance Show BenchmarkState where
  show state =
     "BenchmarkState { " ++
     show (state ^. stateBase       ) ++ ", " ++
     show (state ^. stateCursor     ) ++ ", " ++
     show (state ^. stateCurrentTest) ++  " }"
