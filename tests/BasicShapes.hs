module BasicShapes
 ( basicShapes
 )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Application

import Graphics.Gudni.Util.Segment

import Control.Monad.Random
import System.Random

basicShapes :: [(String, SubSpace -> Int -> Layout DefaultStyle)]
basicShapes =
    [ ("randomCurves",
      \ playhead step ->
      let range = makePoint 2 2
          randomCurves = evalRand (sequence . replicate 5 $ fuzzyCurve range 20) (mkStdGen $ step) :: [ShapeTree Int SubSpace]
      in  place .
          scaleBy 500 .
          rotateBy  (playhead @@ deg) .
          translateByXY 1 (0.5)
          $
          overlap randomCurves
       )
      ,
      ("twoTriangles",
      \ playhead step ->
      scaleBy 100 .
      translateByXY 3 3 $
      overlap [ withColor (transparent 0.25 red) .
                translateByXY 0 0 .
                rotateBy (playhead @@ deg) .
                scaleBy 6 .
                mask
                $
                triangle
                ,
                withColor (transparent 0.25 $ light green) .
                translateByXY 0.75 0.75 .
                rotateBy (playhead @@ deg) .
                scaleBy 6 .
                mask
                $
                triangle
              ]
       )
      ,
      ("triRed",
      \ playhead step ->
      scaleBy 100 .
      translateByXY 1 1 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 red) .
      mask
      $
      triangle2
      )
      ,
      ("diamondBox",
      \ playhead step ->
      scaleBy 100 .
      translateByXY 4 4 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 purple) .
      mask
      $
      Shape . pure . fromSegments $
      [ straightXY 2 2
      , straightXY 1 3
      , straightXY (-1) 1
      , straightXY 0 0
      ]
      )
      ,
      ("tallBox",
      \ playhead step ->
      scaleBy 100 .
      translateByXY 6 1 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 purple) .
      mask
      $
      Shape . pure . fromSegments $
      [ straightXY 2 1
      , straightXY 2 5
      , straightXY 1 5
      , straightXY 1 1
      ]
      )
      ,
      ("triPurple",
      \ playhead step ->
      scaleBy 100 .
      translateByXY 4 4 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 purple) .
      mask
      $
      triangle3
      )
      ,
      ("knob",
      \ playhead step ->
      scaleBy 100 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 purple) .
      mask
      $
      knob
      )
      ,
      ("overhangs",
      \ playhead step ->
      scaleBy 100 .
      translateByXY 1 0 $
      overlap [
          rotateBy (playhead @@ deg) .
          withColor (transparent 0.25 orange) .
          mask
          $
          Shape . pure . fromSegments $
          [ straightXY 0 0
          , straightXY 2 2
          , straightXY 0.5 1.5
          ]
          -- ,
          -- rotateBy (playhead @@ deg) .
          -- withColor (transparent 0.25 (light blue)) .
          -- mask
          -- $
          -- Shape . pure . fromSegments $
          -- [ straightXY   1.5  0.5
          -- , straightXY   0.5  2.5
          -- , straightXY (-0.5) 1.5
          -- ]
      ]
      )
    ]

sixPointRectangle :: Space s => Shape s
sixPointRectangle =
  Shape . pure . fromSegments $
  [ straightXY 0 0, straightXY 1 0, straightXY 2 0
  , straightXY 2 1, straightXY 1 1, straightXY 0 1
  ]

triangle :: Space s => Shape s
triangle =
    Shape . pure . fromSegments $
    [ straightXY 0 0
    , straightXY 1 0
    , straightXY 0 1
    ]

triangle2 :: Space s => Shape s
triangle2 =
    Shape . pure . fromSegments $
    [ straightXY    0 0
    , straightXY    0 3
    , straightXY    3 0
    ]

triangle3 :: Space s => Shape s
triangle3 =
    Shape . pure . fromSegments $
    [ straightXY   0   0
    , straightXY (-3)  0
    , straightXY   0 (-3)
    ]

knob :: Space s => Shape s
knob =
    Shape . pure . fromSegments $
    [ straightXY    0   0
    , curvedXY 0 4 2 2
    ]
