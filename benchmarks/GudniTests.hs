{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GudniTests
 ( BenchmarkState(..)
 , initialModel
 , stateBase
 , statePictureMap
 , stateTests
 , stateCurrentTest
 , stateFrameNumber
 , testList
 )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Application

import Graphics.Gudni.Util.Debug
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
  , _statePictureMap  :: PictureMap
  , _stateTests       :: [(String, BenchmarkState -> Layout DefaultStyle )]
  , _stateCurrentTest :: Int
  }
makeLenses ''BenchmarkState

initialModel pictureMap =
    BenchmarkState
    { _stateBase = BasicSceneState
        { _stateScale       = 10
        , _stateDelta       = Point2 20 20
        , _stateAngle       = 0 @@ rad
        , _statePaused      = True
        , _stateSpeed       = 0.1
        , _statePace        = 1
        , _stateLastTime    = 0
        , _stateDirection   = True
        , _statePlayhead    = 0
        , _stateFrameNumber = 0
        , _stateStep        = 100
        , _stateRepMode     = False
        , _stateRepDk       = False
        , _stateCursor      = Point2 0 0
        }
    , _statePictureMap  = pictureMap
    , _stateTests       = testList
    , _stateCurrentTest = 19
    }

testList = [ ("openSquareOverlap3", openSquareOverlap3  ) --  0 -
           , ("benchmark1"        , benchmark1          ) --  1 -
           , ("fuzzy cookie"      , fuzzyDonut          ) --  2 -
           , ("fuzzy basic"       , fuzzyBasic          ) --  3 -
           , ("fuzzy circles"     , fuzzyCircles        ) --  4 -
           , ("fuzzy squares"     , fuzzySquares        ) --  5 -
           , ("fuzzy glyphs"      , fuzzyGlyphs         ) --  6 -
           , ("fuzzy glyphs2"     , fuzzyGlyphs2        ) --  7 -
           , ("testPict"          , testPict            ) --  8 -
           , ("testRadialGradient", testRadialGradient  ) --  9 -
           , ("testLinearGradient", testLinearGradient  ) -- 10 -
           , ("rectGrid"          , rectGrid            ) -- 11 -
           , ("solidGrid"         , solidGrid           ) -- 12 -
           , ("checkerBoard"      , checkerBoard        ) -- 13 -
           , ("openSquare"        , openSquare          ) -- 14 -
           , ("openSquareOverlap2", openSquareOverlap2  ) -- 15 -
           , ("stackOfSquares"    , stackOfSquares      ) -- 16 -
           , ("concentricSquares2", concentricSquares2  ) -- 17 -
           , ("concentricSquares3", concentricSquares3  ) -- 18 -
           , ("subtractDiamond "  , subtractDiamond     ) -- 19 -
           , ("subtractDiamond2"  , subtractDiamond2    ) -- 20 -
           , ("simpleKnob"        , simpleKnob          ) -- 21 -
           , ("hourGlass"         , hourGlass           ) -- 22 -
           , ("simpleGlyph"       , simpleGlyph         ) -- 23 -
           , ("triangle"          , triangle            ) -- 24 -
           , ("sixPointRectangle" , sixPointRectangle   ) -- 25 -
           , ("tinySquare"        , tinySquare          ) -- 26 -
           , ("mediumSequare"     , mediumSquare        ) -- 27 -
           , ("simpleRectangle"   , simpleRectangle     ) -- 28 -
           , ("tallRectangle"     , tallRectangle       ) -- 29 -
           , ("twoBrackets"       , twoBrackets         ) -- 30 -
           , ("fuzzySquares2"     , fuzzySquares2       ) -- 31 -
           , ("maxThresholdTest"  , maxThresholdTest    ) -- 32 -
           , ("maxShapeTest"      , maxShapeTest        ) -- 33 -
           , ("fuzzyCircles2"     , fuzzyCircles2       ) -- 34 -
           , ("fuzzyCirclesGrad"  , fuzzyCirclesGrad    ) -- 35 -
           , ("fullRectangle"     , fullRectangle       ) -- 36 -
           , ("overlappingSquares", overlappingSquares  ) -- 37 -
           , ("overlappingCircles", overlappingCircles  ) -- 38 -
           , ("1 Million Circles" , millionFuzzyCircles ) -- 39 -
           , ("bigGrid"           , bigGrid             )
           ]

maxThresholdTest :: BenchmarkState -> Layout DefaultStyle
maxThresholdTest state =
    translateByXY (-1) 1 .
    rotateBy (0.2 @@ rad) .
    scaleBy 0.1 .
    withColor (transparent 1.0 . light $ purple) .
    overlap .
    gridOf 2 1 2 .
    repeat .
    mask .
    rectangle $
    Point2 1000 1

-- | Stack of very wide thin RGB rectangles useful for testing large numbers of shapes per tile and
-- subpixel geometry.
maxShapeTest ::  BenchmarkState -> Layout DefaultStyle
maxShapeTest state =
    overlap .
    gridOf 1 1 (state ^. stateBase . stateStep + 400 + 1) .
    concat .
    repeat $
        [ withColor (transparent 1.0 (pureRed    )) . mask . rectangle $ Point2 10000 1
        , withColor (transparent 1.0 (pureGreen  )) . mask . rectangle $ Point2 10000 1
        , withColor (transparent 1.0 (pureBlue   )) . mask . rectangle $ Point2 10000 1
        ]

-- | A fuzz test of random curves where the random points are all within a donut shape.
fuzzyDonut :: BenchmarkState -> Layout DefaultStyle
fuzzyDonut state =
    let time = view (stateBase . stateLastTime) state
    in  place .
        translateByXY 500 500 .
        overlap $ evalRand (sequence . replicate 16 $ fuzzyRadial 400 500 100) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | A basic fuzz test of random curves contained in a rectagular area.
fuzzyBasic :: BenchmarkState -> Layout DefaultStyle
fuzzyBasic state =
    let time = view (stateBase . stateLastTime) state
    in  place .
        translateByXY 100 100 .
        overlap $
        evalRand (sequence . replicate 16 $ fuzzyCurve (makePoint 1440 900) 10) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | Just a lot of very transparent squares all on top of one and other.
overlappingSquares :: BenchmarkState -> Layout DefaultStyle
overlappingSquares state =
    let time = view (stateBase . stateLastTime) state
    in  place .
        translateByXY 5 5 .
        rotateBy (3 @@ deg) .
        --scaleBy 0.5 .
        overlap $
        replicate 2000 $ {-translateBy (Point2 (-22) (-22)) .-} withColor (transparent 0.001 blue) $ mask $ rectangle (50 `by` 50)

-- | Lot of circles stacked on top of one another in nearly the same place.
overlappingCircles :: BenchmarkState -> Layout DefaultStyle
overlappingCircles state =
    let time = view (stateBase . stateLastTime) state
    in  --translateByXY (-4) (-3) .
        --scaleBy 0.5 .
        place .
        overlap $
        evalRand (sequence . replicate 100 $ fuzzyCircle (makePoint 2 2) 5 5) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | A random field of transparent circles.
fuzzyCircles :: BenchmarkState -> Layout DefaultStyle
fuzzyCircles state =
     let time = view (stateBase . stateLastTime) state
     in  translateByXY (-4) (-3) .
         --scaleBy 0.5 .
         place .
         overlap $
         --evalRand (sequence . replicate 100000 $ fuzzyCircle (makePoint 200 200) 5 50) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))
         evalRand (sequence . replicate 100000 $ fuzzyCircle (makePoint 5760 3600) 5 50) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | A random field of transparent circles.
fuzzyCirclesGrad :: BenchmarkState -> Layout DefaultStyle
fuzzyCirclesGrad state =
    let time = view (stateBase . stateLastTime) state
    in  translateByXY (-4) (-3) .
        --scaleBy 0.5 .
        place .
        overlap $
        --evalRand (sequence . replicate 100000 $ fuzzyCircle (makePoint 200 200) 5 50) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))
        evalRand (sequence . replicate 10000 $ fuzzyCircleGradient (makePoint 5760 3600) 5 50) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | Smaller random field of transparent circles.
fuzzyCircles2 :: BenchmarkState -> Layout DefaultStyle
fuzzyCircles2 state =
    let time = view (stateBase . stateLastTime) state
    in  translateByXY 0 0 .
        place .
        overlap $
        evalRand (sequence . replicate (state ^. stateBase . stateStep) $ fuzzyCircle (makePoint 20 20) 5 10) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | A random field of transparent circles.
millionFuzzyCircles :: BenchmarkState -> Layout DefaultStyle
millionFuzzyCircles state =
    let time = view (stateBase . stateLastTime) state
    in  --translateByXY (100) (100) .
        place .
        scaleBy 0.5 .
        overlap $
        evalRand (sequence . replicate 1000000 $ fuzzyCircle (makePoint 5760 3600) 5 10) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000))

-- | A random field of transparent squares.
fuzzySquares :: BenchmarkState -> Layout DefaultStyle
fuzzySquares state =
    let time = view (stateBase . stateLastTime) state
    in  place .
        overlap $
        evalRand (sequence . replicate 50000 $ fuzzySquare (makePoint 2880 1800) 10 60) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | Smaller random field of transparent squares.
fuzzySquares2 :: BenchmarkState -> Layout DefaultStyle
fuzzySquares2 state =
    let time = view (stateBase . stateLastTime) state
    in  place .
        overlap $
        evalRand (sequence . replicate 20 $ fuzzySquare (makePoint 30 30) 10 60) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

fuzzyGlyphs :: BenchmarkState -> Layout DefaultStyle
fuzzyGlyphs state =
   let defaultGlyphs = V.fromList $ map glyph ['!'..'z']
       len = length defaultGlyphs
       time = view (stateBase . stateLastTime) state
   in  translateByXY 200 200 .
       overlap $
       evalRand (sequence . replicate 1000 $ fuzzyGlyph defaultGlyphs (makePoint 512 512) 10 300) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

fuzzyGlyphs2 :: BenchmarkState -> Layout DefaultStyle
fuzzyGlyphs2 state =
    let defaultGlyphs  = V.fromList $ map glyph ['!'..'z']
        len = length defaultGlyphs
        time = view (stateBase . stateLastTime) state
    in  overlap $
        evalRand (sequence . replicate 25000 $ fuzzyGlyph defaultGlyphs (makePoint 2880 2000) 10 200) (mkStdGen $ (round $ state ^. stateBase . statePlayhead * 2000) + (state ^. stateBase . stateStep))

-- | A grid of rotating glyphs with overlapping subtracted glyphs
benchmark1 :: BenchmarkState -> Layout DefaultStyle
benchmark1 state =
    let defaultGlyphs = map glyph "***O***X" --"ALl WoRk AnD nO pLaY mAkEs JaCk A dUlL bOy" ++ ['a'..'z']++['A'..'Z']++['0'..'9']++"!@#$%^&*()_+-={}[]:;\"'<>,./?\\|"
        subtractorGlyphs = map glyph "*"
        dSize         = state ^. stateBase . stateScale
        angle         = normalizeAngle ((((fromIntegral $ state ^. stateBase . stateStep) / 100) + (state ^. stateBase . statePlayhead)) @@ turn)
        defaultString = cycle $ defaultGlyphs
        angles        = map (normalizeAngle . (angle ^+^) . (^*0.5) . (@@ deg) ) [0..]
        textGrid  :: CompoundLayout DefaultStyle
        textGrid   = scaleBy dSize . overlap . gridOf 0.5 50 50 . zipWith (\a -> rotateBy a) angles $ defaultString
        subtractor:: CompoundLayout DefaultStyle
        subtractor = scaleBy (dSize * 15) . overlap . gridOf 0.5 4 4 . cycle $ subtractorGlyphs
    in  translateByXY 0 0 .
        scaleBy 50 .
        withColor (transparent 1.0 black) $
        subtractFrom subtractor textGrid

-- | A grid of rectangles.
rectGrid :: BenchmarkState -> Layout DefaultStyle
rectGrid state =
    let grid :: CompoundTree SubSpace
        grid =  overlap . gridOf 1 256 256 . repeat . mask . rectangle $ Point2 0.5 0.5
    in  place .
        scaleBy 2 .
        withColor (transparent 1.0 white) $
        grid

-- | A very big grid of rectangles.
bigGrid :: BenchmarkState -> Layout DefaultStyle
bigGrid state =
    let grid :: CompoundTree SubSpace
        grid =  overlap . gridOf 1 1600 1600 . repeat . mask . rectangle $ Point2 0.5 0.5
    in  place .
        withColor (transparent 1.0 white) $
        grid

-- | A grid of rectangles in direct contact
solidGrid :: BenchmarkState -> Layout DefaultStyle
solidGrid state =
    let grid  :: CompoundTree SubSpace
        grid   = overlap . gridOf 1 600 600 . repeat . mask . rectangle $ Point2 1 1
    in  place .
        withColor (transparent 1.0 white) $
        grid

-- | A grid of rectangles.
checkerBoard :: BenchmarkState -> Layout DefaultStyle
checkerBoard state =
    let grid  :: CompoundTree SubSpace
        grid = overlap .
               gridOf 1 10 10 .
               repeat .
               overlap $
               [ mask . rectangle $ Point2 0.5 0.5
               , translateByXY 0.5 0.5 . mask . rectangle $ Point2 0.5 0.5
               ]
    in  place .
        withColor (transparent 1.0 white) $
        grid

-- | A knob is a vertical curve section whose control point sticks out further in the x direction than it's other points
simpleKnob :: BenchmarkState -> Layout DefaultStyle
simpleKnob state =
        place .
        scaleBy 10 .
        withColor (transparent 1.0 (red)) .
        fromSegments $
        [ curvedXY 0 0 1 1
        , straightXY 0 2
        ]

-- | Test shape for intersecting thresholds.
hourGlass :: BenchmarkState -> Layout DefaultStyle
hourGlass state =
  let step = fromIntegral $ state ^. stateBase . stateStep
  in    place .
        scaleBy 8 .
        withColor (transparent 1.0 (dark $ dark gray)) .
        fromSegments $
        [ straightXY 0 0
        , straightXY 1 1
        , straightXY 1 0
        , straightXY 0 1
        ]

-- | Test for loading a texture.
testPict :: BenchmarkState -> Layout DefaultStyle
testPict state =
    let w = 1000
        h = 300
        f (Point2 x y) = hslColor 0 (fromIntegral x / fromIntegral w) (fromIntegral y / fromIntegral h)
        size = Point2 w h
    in  place $
        overlap [ translateByXY 100 50 $ withTexture (SharedTexture "flowers") $ subtractFrom (scaleBy 200 . mask $ circle) (addOver (translateByXY 100 100 . scaleBy 100 . mask $ circle) (scaleBy 100 . mask $ circle))
                , translateByXY 100 50 $ withTexture (NewTexture "gradient" (PictureFunction f size)) $ translateByXY 0 50 $ (scaleBy 50 . mask $ circle)
                -- , translateByXY 100 50 $ withColor (transparent 0.2 blue) $ rectangle (Point2 40 2000)
                ]

-- | Test for radial gradient rendering.
testRadialGradient :: BenchmarkState -> Layout DefaultStyle
testRadialGradient state =
    let w = 1000
        h = 300
        f (Point2 x y) = hslColor 0 (fromIntegral x / fromIntegral w) (fromIntegral y / fromIntegral h)
        size = Point2 w h
    in  place .
        translateByXY 50 50 $
        overlap [ withRadialGradient zeroPoint 0 clearBlack 100 red . mask . rectangle $ Point2 300 300 ]

-- | Test for linear gradient rendering.
testLinearGradient :: BenchmarkState -> Layout DefaultStyle
testLinearGradient state =
    let w = 1000
        h = 300
        f (Point2 x y) = hslColor 0 (fromIntegral x / fromIntegral w) (fromIntegral y / fromIntegral h)
        size = Point2 w h
    in  place $ -- rotateBy (45 @@ deg) $
        overlap [ withLinearGradient zeroPoint clearBlack (Point2 100 500) red . mask $ rectangle (Point2 500 500) ]

-- | Simple stack of squares.
stackOfSquares :: BenchmarkState -> Layout DefaultStyle
stackOfSquares state = place $
        overlap
          [ (translateByXY 0 0  . withColor (transparent 1.0 red    ) . mask $ rectangle (Point2 8 4) )
          , (translateByXY 0 4  . withColor (transparent 1.0 green  ) . mask $ rectangle (Point2 8 4) )
          , (translateByXY 0 8  . withColor (transparent 1.0 blue   ) . mask $ rectangle (Point2 8 4) )
          --, (translateByXY 0 12 . withColor (transparent 1.0 purple ) $ rectangle (Point2 4 4) )
          ]

-- | Basic test for shape subtraction.
openSquare :: BenchmarkState -> Layout DefaultStyle
openSquare state =
    place .
    withColor (transparent 1 orange) $
          subtractFrom (translateBy (Point2 1 1) . mask $ rectangle (Point2 3 3))
                    (mask $ rectangle (Point2 5 5))

-- | Basic test for shape subtraction and transparency.
openSquareOverlap2 :: BenchmarkState -> Layout DefaultStyle
openSquareOverlap2 state =
    place .
    scaleBy 20 $
    overlap [ (translateByXY 0 0 . withColor (transparent 0.25 blue  ) $ subtractFrom (translateBy (Point2 4 4) . mask . rectangle $ Point2 8 8 ) (mask . rectangle $ Point2 16 16) )
            , (translateByXY 8 8 . withColor (transparent 1.0  orange) $ subtractFrom (translateBy (Point2 4 4) . mask . rectangle $ Point2 8 8 ) (mask . rectangle $ Point2 16 16) )
            ]

-- | Basic test for shape subtraction and muliple transparency.
openSquareOverlap3 :: BenchmarkState -> Layout DefaultStyle
openSquareOverlap3 state =
    let angle = view (stateBase . statePlayhead) state @@ turn
    in  place .
        translateByXY 400 400 .
        scaleBy 50 $
        overlap [ (translateByXY 0 0 . rotateBy angle        $ withColor (transparent 0.5 blue   ) $ subtractFrom (translateBy (Point2 4 4) . mask . rectangle $ Point2 8 8 ) (mask . rectangle $ Point2 16 16) )
                , (translateByXY 4 4 . rotateBy (angle ^/ 2) $ withColor (transparent 0.5 orange ) $ subtractFrom (translateBy (Point2 4 4) . mask . rectangle $ Point2 8 8 ) (mask . rectangle $ Point2 16 16) )
                , (translateByXY 8 8 . rotateBy (angle ^/ 3) $ withColor (transparent 0.5 green  ) $ subtractFrom (translateBy (Point2 4 4) . mask . rectangle $ Point2 8 8 ) (mask . rectangle $ Point2 16 16) )
                ]

-- | Test for shape edges that abut.
concentricSquares2 :: BenchmarkState -> Layout DefaultStyle
concentricSquares2 state =
    place $
    overlap [ (translateByXY 0 0 . withColor (transparent 1.0 red ) $ subtractFrom (translateBy (Point2 1 1) . mask . rectangle $ Point2 3 3 ) (mask . rectangle $ Point2 5 5) )
            , (translateByXY 1 1 . withColor (transparent 1.0 blue) $ subtractFrom (translateBy (Point2 1 1) . mask . rectangle $ Point2 1 1 ) (mask . rectangle $ Point2 3 3) )
            ]

-- | Another test for shape edges that abut.
concentricSquares3 :: BenchmarkState -> Layout DefaultStyle
concentricSquares3 state =
    place $
    overlap [ translateByXY 0 0 . withColor (transparent 1.0 red   ) $ subtractFrom (translateBy (Point2 2 2) $ (mask $ rectangle (Point2 6 6)) ) (mask $ rectangle (Point2 10 10))
            , translateByXY 2 2 . withColor (transparent 1.0 green ) $ subtractFrom (translateBy (Point2 2 2) $ (mask $ rectangle (Point2 2 2)) ) (mask $ rectangle (Point2  6  6))
            , translateByXY 4 4 . withColor (transparent 1.0 blue  ) $                                                                            (mask $ rectangle (Point2  2  2))
            ]

-- | Simple test one glyph.
simpleGlyph :: forall m . BenchmarkState -> Layout DefaultStyle
simpleGlyph state =
    let character = chr $ state ^. stateBase . stateStep
        g = glyphOf defaultValue character
    in  translateByXY 0 0 .
        rotateBy (6.28248 @@ rad) .
        scaleBy 20 .
        withColor (transparent 1.0 white) $
        g

-- | Test for straight vertical segments with multiple colinear points.
sixPointRectangle :: BenchmarkState -> Layout DefaultStyle
sixPointRectangle state =
    place .
    withColor (transparent 1.0 (dark gray)) $
    fromSegments [straightXY 0 0, straightXY 1 0, straightXY 2 0
                 ,straightXY 2 1, straightXY 1 1, straightXY 0 1
                 ]

-- | Simple Triangle
triangle :: BenchmarkState -> Layout DefaultStyle
triangle state =
    place .
    withColor (transparent 1.0 (dark green)) $
    fromSegments [ straightXY 0 0
                 , straightXY 5 0
                 , straightXY 0 5
                 ]
-- | Very tiny square with no rotation. Usually the first thing tested for a new build.
tinySquare :: BenchmarkState -> Layout DefaultStyle
tinySquare state =
    place .
    translateByXY 0.0 0.5 .
    --rotateBy (45 @@ deg) .
    withColor red .
    mask .
    rectangle $
    Point2 4 4

-- | Medium sized square with no rotation.
mediumSquare :: BenchmarkState -> Layout DefaultStyle
mediumSquare state =
    place .
    translateByXY 0.0 1.0 .
    withColor red .
    mask .
    rectangle $
    Point2 10 10

-- | Medium sized square with no rotation.
fullRectangle :: BenchmarkState -> Layout DefaultStyle
fullRectangle state =
    place .
    translateByXY 0 0 .
    withColor red .
    mask .
    rectangle $
    makePoint 2880 1800

-- | Very simple rotated box.
simpleRectangle :: BenchmarkState -> Layout DefaultStyle
simpleRectangle state =
    place .
    translateByXY (0.35 + 0.1) 0 .
    rotateBy (45 @@ deg) .
    scaleBy 1 .
    withColor (transparent 1.0 white) .
    mask .
    rectangle $
    Point2 2 2

-- | Simple rectangle test across multiple tiles.
tallRectangle :: BenchmarkState -> Layout DefaultStyle
tallRectangle state =
    place .
    translateByXY 0 1 .
    rotateBy (3 @@ deg) .
    withColor (transparent 1.0 white) .
    mask .
    rectangle $
    Point2 0.5 2000

-- | Simple multishape test useful for subpixel geometry.
twoBrackets :: BenchmarkState -> Layout DefaultStyle
twoBrackets state =
    place .
    withColor white $
    overlap [ fromSegments [straightXY (-0.2) 0.0, straightXY   1.2  1.4
                           ,straightXY   1.2  1.6, straightXY (-0.2) 0.2
                           ]
            , fromSegments [straightXY (-0.2) 0.4, straightXY   1.2  1.8
                           ,straightXY   1.2  2.0, straightXY (-0.2) 0.6
                           ]
            ]

unitSquare = mask . rectangle $ Point2 1 1
-- | Very simple subtraction test.
subtractDiamond :: BenchmarkState -> Layout DefaultStyle
subtractDiamond state =
    place .
    --rotateBy (45 @@ deg) .
    withColor white $
    (scaleBy 2 unitSquare) `subtractFrom` (translateByXY 1 1 . scaleBy 2 $ unitSquare)

subtractDiamond2 :: BenchmarkState -> Layout DefaultStyle
subtractDiamond2 state =
    place .
    --rotateBy (45 @@ deg) .
    withColor white $
    (scaleBy 2 . overlap . replicate 200 $ unitSquare) `subtractFrom` (translateByXY 1 1 . scaleBy 2 . overlap . replicate 200 $ unitSquare)

instance Show BenchmarkState where
  show state =
     "BenchmarkState { " ++
     show (state ^. stateBase       ) ++ ", " ++
     show (state ^. stateCurrentTest) ++  " }"
