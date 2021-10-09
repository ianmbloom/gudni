module BasicShapes
 ( basicShapes
 )
where

import Graphics.Gudni.Interface
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw
import Graphics.Gudni.Application

import Graphics.Gudni.Util.Segment

import Control.Monad.Random
import System.Random

basicShapes :: [(String, SubSpace -> Int -> Layout Rgba DefaultStyle)]
basicShapes =
    [ ("randomCurves",
      \ playhead step ->
      let range = makePoint 2 2
          randomCurves = evalRand (sequence . replicate 6 $ fuzzyCurve range 20) (mkStdGen $ step) :: [Layout Rgba DefaultStyle]
      in  scaleBy 100 .
          rotateBy (playhead @@ deg)
          $
          overlap randomCurves
       )
     ,
     ("onTriangles",
     \ playhead step ->
     --translateByXY 10 10 .
     --rotateBy (playhead @@ deg) .
     scaleBy 100
     $
     overlap [ withColor (transparent 0.5 red) .
               place
               $
               triangle
               ,
               withColor (transparent 0.5 cyan) .
               place
               $
               triangleOff
               -- ,
               -- withColor (transparent 0.5 purple) .
               -- translateByXY 0.5 0.5 .
               -- place
               -- $
               -- triangle
               -- ,
               -- withColor (transparent 0.5 green) .
               -- translateByXY 1 1 .
               -- place
               -- $
               -- triangle
             ]
      )
      ,
      ("twoTriangles",
      \ playhead step ->
      scaleBy 7 .
      translateByXY 1 1
      $
      overlap [ withColor (transparent 0.5 red) .
                translateByXY 0 0 .
                rotateBy (playhead @@ deg) .
                scaleBy 10 .
                place
                $
                triangle
                ,
                withColor (transparent 0.5 cyan) .
                translateByXY 1 1 .
                rotateBy (playhead @@ deg) .
                scaleBy 10 .
                place
                $
                triangle
                ,
                withColor (transparent 0.5 purple) .
                translateByXY 2 2  .
                rotateBy (playhead @@ deg) .
                scaleBy 10 .
                place
                $
                triangle
                ,
                withColor (transparent 0.5 green) .
                translateByXY 3 3  .
                rotateBy (playhead @@ deg) .
                scaleBy 10 .
                place
                $
                triangle
              ]
       )
      ,
      ("bigTriangle",
      \ playhead step ->
      -- rotateBy ((negate playhead) @@ deg) .
      -- rotateBy (playhead @@ deg) .
      withColor ({-transparent 0.25-} red) .
      place
      $
      bigTriangle
      )
      ,
      ("triRed",
      \ playhead step ->
      -- rotateBy ((negate playhead) @@ deg) .
      translateByXY 0 0 .
      scaleBy 5 .
      -- rotateBy (playhead @@ deg) .
      withColor ({-transparent 0.25-} red) .
      place
      $
      triangle2
      )
      ,
      ("diamondBox",
      \ playhead step ->
      scaleBy 10 .
      translateByXY 4 4 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 purple) .
      place
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
      scaleBy 10 .
      translateByXY 6 1 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 purple) .
      place
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
      scaleBy 10 .
      translateByXY 4 4 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 purple) .
      place
      $
      triangle3
      )
      ,
      ("knob",
      \ playhead step ->
      scaleBy 10 .
      rotateBy (playhead @@ deg) .
      withColor (transparent 0.25 purple) .
      place
      $
      knob
      )
      ,
      ("overhangs",
      \ playhead step ->
      scaleBy 10 .
      translateByXY 1 0 $
      overlap [
          rotateBy (playhead @@ deg) .
          withColor (transparent 0.25 orange) .
          place
          $
          Shape . pure . fromSegments $
          [ straightXY 0 0
          , straightXY 2 2
          , straightXY 0.5 1.5
          ]
          -- ,
          -- rotateBy (playhead @@ deg) .
          -- withColor (transparent 0.25 (light blue)) .
          -- place
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

bigTriangle :: Space s => Shape s
bigTriangle =
    Shape . pure . fromSegments $
    [ straightXY 0  0
    , straightXY 32 0
    , straightXY 0  32
    ]



triangle :: Space s => Shape s
triangle =
    Shape . pure . fromSegments $
    [ straightXY 0 0
    , straightXY 1 0
    , straightXY 0 1
    ]

triangleOff :: Space s => Shape s
triangleOff =
    Shape . pure . fromSegments $
    [ straightXY 0.05 0.05
    , straightXY 1.05 0.05
    , straightXY 0.05 1.05
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
