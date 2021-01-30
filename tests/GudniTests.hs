{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GudniTests
 ( testList
 , findTest
 )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw
import Graphics.Gudni.Application
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Segment

import Data.Word
import Data.Maybe
import Data.Char
import Data.List
import Control.Lens
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

import Control.Monad.Random
import System.Random

testList = [ ("openSquareOverlap3"  , openSquareOverlap3  ) --  0 -
           , ("benchmark1"          , benchmark1          ) --  1 -
           , ("fuzzyDonut"          , fuzzyDonut          ) --  2 -
           , ("fuzzyBasic"          , fuzzyBasic          ) --  3 -
           , ("fuzzyCircles"        , fuzzyCircles        ) --  4 -
           , ("fuzzySquares"        , fuzzySquares        ) --  5 -
           , ("fuzzyGlyphs"         , fuzzyGlyphs         ) --  6 -
           , ("fuzzyGlyphs2"        , fuzzyGlyphs2        ) --  7 -
           , ("testPict"            , testPict            ) --  8 -
           , ("testRadialGradient"  , testRadialGradient  ) --  9 -
           , ("testLinearGradient"  , testLinearGradient  ) -- 10 -
           , ("rectGrid"            , rectGrid            ) -- 11 -
           , ("solidGrid"           , solidGrid           ) -- 12 -
           , ("checkerBoard"        , checkerBoard        ) -- 13 -
           , ("openSquare"          , openSquare          ) -- 14 -
           , ("openSquareOverlap2"  , openSquareOverlap2  ) -- 15 -
           , ("stackOfSquares"      , stackOfSquares      ) -- 16 -
           , ("concentricSquares2"  , concentricSquares2  ) -- 17 -
           , ("concentricSquares3"  , concentricSquares3  ) -- 18 -
           , ("subtractDiamond"     , subtractDiamond     ) -- 19 -
           , ("subtractDiamond2"    , subtractDiamond2    ) -- 20 -
           , ("simpleKnob"          , simpleKnob          ) -- 21 -
           , ("hourGlass"           , hourGlass           ) -- 22 -
           , ("simpleGlyph"         , simpleGlyph         ) -- 23 -
           , ("triangle"            , triangle            ) -- 24 -
           , ("sixPointRectangle"   , sixPointRectangle   ) -- 25 -
           , ("tinySquare"          , tinySquare          ) -- 26 -
           , ("mediumSquare"        , mediumSquare        ) -- 27 -
           , ("simpleRectangle"     , simpleRectangle     ) -- 28 -
           , ("tallRectangle"       , tallRectangle       ) -- 29 -
           , ("twoBrackets"         , twoBrackets         ) -- 30 -
           , ("fuzzySquares2"       , fuzzySquares2       ) -- 31 -
           , ("maxThresholdTest"    , maxThresholdTest    ) -- 32 -
           , ("maxShapeTest"        , maxShapeTest        ) -- 33 -
           , ("fuzzyCircles2"       , fuzzyCircles2       ) -- 34 -
           , ("fuzzyCirclesGrad"    , fuzzyCirclesGrad    ) -- 35 -
           , ("fullRectangle"       , fullRectangle       ) -- 36 -
           , ("overlappingSquares"  , overlappingSquares  ) -- 37 -
           , ("overlappingCircles"  , overlappingCircles  ) -- 38 -
           , ("millionFuzzyCircles" , millionFuzzyCircles ) -- 39 -
          -- , ("bigGrid"             , bigGrid             )
           ]

findTest :: String -> [(String, a)] -> Int
findTest name = fromJust . findIndex ((== name) . fst)

maxThresholdTest :: SubSpace -> Int -> Layout Rgba DefaultStyle
maxThresholdTest playhead step =
    translateByXY (-1) 1 .
    rotateBy (0.2 @@ rad) .
    scaleBy 0.1 .
    withColor (transparent 1.0 . light $ purple) .
    overlap .
    gridOf 2 1 2 .
    repeat .
    place .
    rectangle $
    Point2 1000 1

-- | Stack of very wide thin RGB rectangles useful for testing large numbers of shapes per tile and
-- subpixel geometry.
maxShapeTest ::  SubSpace -> Int -> Layout Rgba DefaultStyle
maxShapeTest playhead step =
    rotateBy (playhead @@ deg) .
    overlap .
    gridOf 1 1 (step + 400 + 1) .
    concat .
    repeat $
        [ withColor (transparent 1.0 (pureRed    )) . place . rectangle $ Point2 10000 1
        , withColor (transparent 1.0 (pureGreen  )) . place . rectangle $ Point2 10000 1
        , withColor (transparent 1.0 (pureBlue   )) . place . rectangle $ Point2 10000 1
        ]

-- | A fuzz test of random curves where the random points are all within a donut shape.
fuzzyDonut :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzyDonut playhead step =
        translateByXY 500 500 .
        overlap $ evalRand (sequence . replicate 16 $ fuzzyRadial 400 500 100) (mkStdGen $ (round $ playhead * 2000) + step)

-- | A basic fuzz test of random curves contained in a rectagular area.
fuzzyBasic :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzyBasic playhead step =
        translateByXY 100 100 .
        overlap $
        evalRand (sequence . replicate 16 $ fuzzyCurve (makePoint 1440 900) 10) (mkStdGen $ (round $ playhead * 2000) + step)

-- | Just a lot of very transparent squares all on top of one and other.
overlappingSquares :: SubSpace -> Int -> Layout Rgba DefaultStyle
overlappingSquares playhead step =
        translateByXY 5 5 .
        rotateBy (3 @@ deg) .
        --scaleBy 0.5 .
        overlap $
        replicate 2000 $ {-translateBy (Point2 (-22) (-22)) .-} withColor (transparent 0.001 blue) $ place $ rectangle (50 `by` 50)

-- | Lot of circles stacked on top of one another in nearly the same place.
overlappingCircles :: SubSpace -> Int -> Layout Rgba DefaultStyle
overlappingCircles playhead step =
      --translateByXY (-4) (-3) .
        --scaleBy 0.5 .
        overlap $
        evalRand (sequence . replicate 100 $ fuzzyCircle (makePoint 2 2) 5 5) (mkStdGen $ (round $ playhead * 2000))

-- | A random field of transparent circles.
fuzzyCircles :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzyCircles playhead step =
        --translateByXY (-4) (-3) .
         --scaleBy 0.5 .
         overlap $
         --evalRand (sequence . replicate 100000 $ fuzzyCircle (makePoint 200 200) 5 50) (mkStdGen $ (round $ playhead * 2000))
         evalRand (sequence . replicate 200 $ fuzzyCircle (makePoint 100 100) 5 50) (mkStdGen $ (round $ playhead * 2000))

-- | A random field of transparent circles.
fuzzyCirclesGrad :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzyCirclesGrad playhead step =
        translateByXY (-4) (-3) .
        --scaleBy 0.5 .
        overlap $
        --evalRand (sequence . replicate 100000 $ fuzzyCircle (makePoint 200 200) 5 50) (mkStdGen $ (round $ playhead * 2000))
        evalRand (sequence . replicate 10000 $ fuzzyCircleGradient (makePoint 5760 3600) 5 50) (mkStdGen $ (round $ playhead * 2000))

-- | Smaller random field of transparent circles.
fuzzyCircles2 :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzyCircles2 playhead step =
      translateByXY 0 0 .
        overlap $
        evalRand (sequence . replicate step $ fuzzyCircle (makePoint 20 20) 5 10) (mkStdGen $ (round $ playhead * 2000))

-- | A random field of transparent circles.
millionFuzzyCircles :: SubSpace -> Int -> Layout Rgba DefaultStyle
millionFuzzyCircles playhead step =
      --translateByXY (100) (100) .
        scaleBy 0.5 .
        overlap $
        evalRand (sequence . replicate 1000000 $ fuzzyCircle (makePoint 5760 3600) 5 10) (mkStdGen $ (round $ playhead * 2000))

-- | A random field of transparent squares.
fuzzySquares :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzySquares playhead step =
        overlap $
        evalRand (sequence . replicate 50000 $ fuzzySquare (makePoint 2880 1800) 10 60) (mkStdGen $ (round $ playhead * 2000) + step)

-- | Smaller random field of transparent squares.
fuzzySquares2 :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzySquares2 playhead step =
        overlap $
        evalRand (sequence . replicate 20 $ fuzzySquare (makePoint 30 30) 10 60) (mkStdGen $ (round $ playhead * 2000) + step)

fuzzyGlyphs :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzyGlyphs playhead step =
   let defaultGlyphs = V.fromList $ map glyph ['!'..'z']
       len = length defaultGlyphs
   in  -- translateByXY 200 200 .
       overlap $
       evalRand (sequence . replicate 1 $ fuzzyGlyph defaultGlyphs (makePoint 64 64) 100 100) (mkStdGen $ (round $ playhead * 2000) + step)

fuzzyGlyphs2 :: SubSpace -> Int -> Layout Rgba DefaultStyle
fuzzyGlyphs2 playhead step =
    let defaultGlyphs  = V.fromList $ map glyph ['!'..'z']
        len = length defaultGlyphs
    in  overlap $
        evalRand (sequence . replicate 25000 $ fuzzyGlyph defaultGlyphs (makePoint 2880 2000) 10 200) (mkStdGen $ (round $ playhead * 2000) + step)

-- | A grid of rotating glyphs with overlapping subtracted glyphs
benchmark1 :: SubSpace -> Int -> Layout Rgba DefaultStyle
benchmark1 playhead step =
    let defaultGlyphs = map glyph "***O***X" --"ALl WoRk AnD nO pLaY mAkEs JaCk A dUlL bOy" ++ ['a'..'z']++['A'..'Z']++['0'..'9']++"!@#$%^&*()_+-={}[]:;\"'<>,./?\\|"
        subtractorGlyphs = map glyph "*"
        dSize         = 10
        angle         = normalizeAngle ((((fromIntegral $ step) / 100) + (realToFrac playhead)) @@ turn)
        defaultString = cycle $ defaultGlyphs
        angles :: [Angle SubSpace]
        angles        = map (normalizeAngle . (angle ^+^) . (^*0.5) . (@@ deg) ) [0..]
        textGrid  :: Layout Mono DefaultStyle
        textGrid   = scaleBy dSize . overlap . gridOf 0.5 50 50 . zipWith (\a -> rotateBy a) angles $ defaultString
        subtractor:: Layout Mono DefaultStyle
        subtractor = scaleBy (dSize * 15) . overlap . gridOf 0.5 4 4 . cycle $ subtractorGlyphs
    in  translateByXY 0 0 .
        scaleBy 10 .
        withColor (transparent 0.2 black) $
        subtractFrom subtractor textGrid

-- | A grid of rectangles.
rectGrid :: SubSpace -> Int -> Layout Rgba DefaultStyle
rectGrid playhead step =
    let grid :: Layout Mono DefaultStyle
        grid =  overlap . gridOf 1 8 8 {-256 256-} . repeat . place . rectangle $ Point2 0.5 0.5
    in  -- scaleBy 30 .
        scaleBy 50 .
        withColor (transparent 0.2 purple) $
        grid

-- | A very big grid of rectangles.
bigGrid :: SubSpace -> Int -> Layout Rgba DefaultStyle
bigGrid playhead step =
    let grid :: Layout Mono DefaultStyle
        grid =  overlap . gridOf 1 1600 1600 . repeat . place . rectangle $ Point2 0.5 0.5
    in  withColor (transparent 0.2 purple) $
        grid

-- | A grid of rectangles in direct contact
solidGrid :: SubSpace -> Int -> Layout Rgba DefaultStyle
solidGrid playhead step =
    let grid  :: Layout Mono DefaultStyle
        grid   = overlap . gridOf 1 600 600 . repeat . place . rectangle $ Point2 1 1
    in  withColor (transparent 0.2 purple) $
        grid

-- | A grid of rectangles.
checkerBoard :: SubSpace -> Int -> Layout Rgba DefaultStyle
checkerBoard playhead step =
    let grid  :: Layout Mono DefaultStyle
        grid = scaleBy 4 .
               overlap .
               gridOf 1 600 600 .
               repeat .
               overlap $
               [ place . rectangle $ Point2 0.5 0.5
               , translateByXY 0.5 0.5 . place . rectangle $ Point2 0.5 0.5
               ]
    in  withColor (transparent 1.0 white) $
        grid

-- | A knob is a vertical curve section whose control point sticks out further in the x direction than it's other points
simpleKnob :: SubSpace -> Int -> Layout Rgba DefaultStyle
simpleKnob playhead step =
        scaleBy 10 .
        withColor (transparent 1.0 (red)) .
        place .
        fromSegments $
        [ curvedXY 0 0 1 1
        , straightXY 0 2
        ]

-- | Test shape for intersecting thresholds.
hourGlass :: SubSpace -> Int -> Layout Rgba DefaultStyle
hourGlass playhead step =
  let step = fromIntegral $ step
  in    scaleBy 8 .
        withColor (transparent 1.0 (dark $ dark gray)) .
        place .
        fromSegments $
        [ straightXY 0 0
        , straightXY 1 1
        , straightXY 1 0
        , straightXY 0 1
        ]

-- | Test for loading a texture.
testPict :: SubSpace -> Int -> Layout Rgba DefaultStyle
testPict playhead step =
    let w = 1000
        h = 300
        f (Point2 x y) = hsvaColor 0 (fromIntegral x / fromIntegral w) (fromIntegral y / fromIntegral h) 1
        size = Point2 w h
    in  overlap [ translateByXY 100 50 $ withTexture (SharedTexture "flowers") $ subtractFrom (scaleBy 200 . place $ circle) (addOver (translateByXY 100 100 . scaleBy 100 . place $ circle) (scaleBy 100 . place $ circle))
                -- , translateByXY 100 50 $ withTexture (NewTexture "gradient" (PictureFunction f size)) $ translateByXY 0 50 $ (scaleBy 50 . place $ circle)
                -- , translateByXY 100 50 $ withColor (transparent 0.2 blue) $ rectangle (Point2 40 2000)
                ]

-- | Test for radial gradient rendering.
testRadialGradient :: SubSpace -> Int -> Layout Rgba DefaultStyle
testRadialGradient playhead step =
    let w = 1000
        h = 300
        f (Point2 x y) = hsvaColor 0 (fromIntegral x / fromIntegral w) (fromIntegral y / fromIntegral h) 1
        size = Point2 w h
    in  translateByXY 50 50 $
        overlap [ withRadialGradient zeroPoint 0 clearBlack 100 red . place . rectangle $ Point2 300 300 ]

-- | Test for linear gradient rendering.
testLinearGradient :: SubSpace -> Int -> Layout Rgba DefaultStyle
testLinearGradient playhead step =
    let w = 1000
        h = 300
        f (Point2 x y) = hsvaColor 0 (fromIntegral x / fromIntegral w) (fromIntegral y / fromIntegral h) 1
        size = Point2 w h
    in  -- rotateBy (45 @@ deg) $
        overlap [ withLinearGradient zeroPoint clearBlack (Point2 100 500) red . place $ rectangle (Point2 500 500) ]

-- | Simple stack of squares.
stackOfSquares :: SubSpace -> Int -> Layout Rgba DefaultStyle
stackOfSquares playhead step =
        overlap
          [ (translateByXY 0 0  . withColor (transparent 1.0 red    ) . place $ rectangle (Point2 8 4) )
          , (translateByXY 0 4  . withColor (transparent 1.0 green  ) . place $ rectangle (Point2 8 4) )
          , (translateByXY 0 8  . withColor (transparent 1.0 blue   ) . place $ rectangle (Point2 8 4) )
          --, (translateByXY 0 12 . withColor (transparent 1.0 purple ) $ rectangle (Point2 4 4) )
          ]

-- | Basic test for shape subtraction.
openSquare :: SubSpace -> Int -> Layout Rgba DefaultStyle
openSquare playhead step =
    withColor (transparent 1 orange) $
          subtractFrom (translateBy (Point2 1 1) . place $ rectangle (Point2 3 3))
                    (place $ rectangle (Point2 5 5))

-- | Basic test for shape subtraction and transparency.
openSquareOverlap2 :: SubSpace -> Int -> Layout Rgba DefaultStyle
openSquareOverlap2 playhead step =
    scaleBy 20 $
    overlap [ (translateByXY 0 0 . withColor (transparent 0.25 blue  ) $ subtractFrom (translateBy (Point2 4 4) . place .rectangle $ Point2 8 8 ) (place . rectangle $ Point2 16 16) )
            , (translateByXY 8 8 . withColor (transparent 1.0  orange) $ subtractFrom (translateBy (Point2 4 4) . place .rectangle $ Point2 8 8 ) (place . rectangle $ Point2 16 16) )
            ]

-- | Basic test for shape subtraction and muliple transparency.
openSquareOverlap3 :: SubSpace -> Int -> Layout Rgba DefaultStyle
openSquareOverlap3 playhead step =
    let angle = realToFrac playhead @@ turn :: Angle SubSpace
    in  --translateByXY 400 400 .
        scaleBy 50 $
        overlap [ (translateByXY 0 0 . rotateBy angle        $ withColor (transparent 0.5 blue   ) $ flip subtractFrom (translateBy (Point2 4 4) . place . rectangle $ Point2 8 8 ) (place . rectangle $ Point2 16 16) )
                -- , (translateByXY 4 4 . rotateBy (angle ^/ 2) $ withColor (transparent 0.5 orange ) $ subtractFrom (translateBy (Point2 4 4) . place . rectangle $ Point2 8 8 ) (place . rectangle $ Point2 16 16) )
                -- , (translateByXY 8 8 . rotateBy (angle ^/ 3) $ withColor (transparent 0.5 green  ) $ subtractFrom (translateBy (Point2 4 4) . place . rectangle $ Point2 8 8 ) (place . rectangle $ Point2 16 16) )
                ]

-- | Test for shape edges that abut.
concentricSquares2 :: SubSpace -> Int -> Layout Rgba DefaultStyle
concentricSquares2 playhead step =
    overlap [ (translateByXY 0 0 . withColor (transparent 1.0 red ) $ subtractFrom (translateBy (Point2 1 1) . place . rectangle $ Point2 3 3 ) (place . rectangle $ Point2 5 5) )
            , (translateByXY 1 1 . withColor (transparent 1.0 blue) $ subtractFrom (translateBy (Point2 1 1) . place . rectangle $ Point2 1 1 ) (place . rectangle $ Point2 3 3) )
            ]

-- | Another test for shape edges that abut.
concentricSquares3 :: SubSpace -> Int -> Layout Rgba DefaultStyle
concentricSquares3 playhead step =
    scaleBy 30 $
    overlap [ translateByXY 0 0 . withColor (transparent 1.0 red   ) $ subtractFrom (translateBy (Point2 2 2) $ (place $ rectangle (Point2 6 6)) ) (place $ rectangle (Point2 10 10))
            , translateByXY 2 2 . withColor (transparent 1.0 green ) $ subtractFrom (translateBy (Point2 2 2) $ (place $ rectangle (Point2 2 2)) ) (place $ rectangle (Point2  6  6))
            , translateByXY 4 4 . withColor (transparent 1.0 blue  ) $                                                                            (place $ rectangle (Point2  2  2))
            ]

-- | Simple test one glyph.
simpleGlyph :: forall m . SubSpace -> Int -> Layout Rgba DefaultStyle
simpleGlyph playhead step =
    let character = chr $ step
        g = glyphOf defaultValue character
    in  translateByXY 0 0 .
        rotateBy (6.28248 @@ rad) .
        scaleBy 20 .
        withColor (transparent 1.0 white) $
        g

-- | Test for straight vertical segments with multiple colinear points.
sixPointRectangle :: SubSpace -> Int -> Layout Rgba DefaultStyle
sixPointRectangle playhead step =
    withColor (transparent 1.0 (dark gray)) .
    place $
    fromSegments [straightXY 0 0, straightXY 1 0, straightXY 2 0
                 ,straightXY 2 1, straightXY 1 1, straightXY 0 1
                 ]

-- | Simple Triangle
triangle :: SubSpace -> Int -> Layout Rgba DefaultStyle
triangle playhead step =
    withColor (transparent 1.0 (dark green)) .
    place $
    fromSegments [ straightXY 0 0
                 , straightXY 5 0
                 , straightXY 0 5
                 ]
-- | Very tiny square with no rotation. Usually the first thing tested for a new build.
tinySquare :: SubSpace -> Int -> Layout Rgba DefaultStyle
tinySquare playhead step =
    translateByXY 0.0 0.5 .
    --rotateBy (45 @@ deg) .
    withColor red .
    place .
    rectangle $
    Point2 4 4

-- | Medium sized square with no rotation.
mediumSquare :: SubSpace -> Int -> Layout Rgba DefaultStyle
mediumSquare playhead step =
    translateByXY 0.0 1.0 .
    withColor red .
    place .
    rectangle $
    Point2 10 10

-- | Medium sized square with no rotation.
fullRectangle :: SubSpace -> Int -> Layout Rgba DefaultStyle
fullRectangle playhead step =
    translateByXY 0 0 .
    withColor red .
    place .
    rectangle $
    makePoint 2880 1800

-- | Very simple rotated box.
simpleRectangle :: SubSpace -> Int -> Layout Rgba DefaultStyle
simpleRectangle playhead step =
    --translateByXY (0.35 + 0.1) 0 .
   -- rotateBy (45 @@ deg) .
    scaleBy 20 .
    withColor (transparent 1.0 white) .
    place .
    rectangle $
    Point2 2 2

-- | Simple rectangle test across multiple tiles.
tallRectangle :: SubSpace -> Int -> Layout Rgba DefaultStyle
tallRectangle playhead step =
    translateByXY 0 1 .
    rotateBy (3 @@ deg) .
    withColor (transparent 1.0 white) .
    place .
    rectangle $
    Point2 10 2000

-- | Simple multishape test useful for subpixel geometry.
twoBrackets :: SubSpace -> Int -> Layout Rgba DefaultStyle
twoBrackets playhead step =
    withColor white $
    overlap [ place . fromSegments $ [ straightXY (-0.2) 0.0, straightXY   1.2  1.4
                                     , straightXY   1.2  1.6, straightXY (-0.2) 0.2
                                     ]
            , place . fromSegments $ [ straightXY (-0.2) 0.4, straightXY   1.2  1.8
                                     , straightXY   1.2  2.0, straightXY (-0.2) 0.6
                                     ]
            ]

unitSquare = place . rectangle $ Point2 1 1
-- | Very simple subtraction test.
subtractDiamond :: SubSpace -> Int -> Layout Rgba DefaultStyle
subtractDiamond playhead step =
    --rotateBy (45 @@ deg) .
    withColor white $
    (scaleBy 2 unitSquare) `subtractFrom` (translateByXY 1 1 . scaleBy 2 $ unitSquare)

subtractDiamond2 :: SubSpace -> Int -> Layout Rgba DefaultStyle
subtractDiamond2 playhead step =
    --rotateBy (45 @@ deg) .
    withColor white $
    (scaleBy 2 . overlap . replicate 200 $ unitSquare) `subtractFrom` (translateByXY 1 1 . scaleBy 2 . overlap . replicate 200 $ unitSquare)
