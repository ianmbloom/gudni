module Graphics.Gudni.Util.Draw
  ( lPath
  , glyphR
  , glyph
  , rectangle
  , unitSquare
  , openRectangle
  , diamond
  , line
  , makeRow
  , makeColumn
  , makeGrid
  , increasingAngles
  , arc
  , makeLine
  , paraGrid
  , arcSet
  , overlap
  , circle
  , solid
  , textureWith
  , raw
  , rawPlot
  )
where

import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util
import Data.Char (ord)

lPath :: String -> CompoundTree
lPath name =
  case plotLibrary name of
    Just path -> ShapeGroup $ RawPlot path
    Nothing -> glyph 'X'

glyphR :: Angle DisplaySpace -> Point2 DisplaySpace -> Char -> CompoundTree
glyphR a p x = tTranslate p . tRotate a . ShapeGroup $ let codepoint = CodePoint . ord $ x in RawGlyph codepoint

glyph :: Char -> CompoundTree
glyph x = ShapeGroup $ let codepoint = CodePoint . ord $ x in RawGlyph codepoint

rectangle :: Point2 DisplaySpace -> CompoundTree
rectangle p = ShapeGroup $ RawRectangle p

unitSquare :: CompoundTree
unitSquare = ShapeGroup $ RawRectangle (Point2 1 1)

openRectangle :: DisplaySpace -> Point2 DisplaySpace -> CompoundTree
openRectangle s p = let strokeDelta = Point2 s s in
                    cSubtract (rectangle p)
                              (tTranslate strokeDelta $ rectangle (p ^-^ (strokeDelta ^* 2)))

diamond :: Point2 DisplaySpace -> CompoundTree
diamond point = let box = makeBox zeroPoint point :: Box DisplaySpace
                    t = lerp 0.5 (topLeftBox     box) (topRightBox    box)
                    r = lerp 0.5 (topRightBox    box) (bottomRightBox box)
                    b = lerp 0.5 (bottomRightBox box) (bottomLeftBox  box)
                    l = lerp 0.5 (bottomLeftBox  box) (topLeftBox     box)
                in raw  [Vert True t, Vert True r, Vert True b, Vert True l]

line :: DisplaySpace -> Point2 DisplaySpace -> Point2 DisplaySpace -> CompoundTree
line s a b = ShapeGroup $ RawLine a b s

makeRow :: Transformable f => DisplaySpace -> [f] -> [f]
makeRow s = zipWith ($) (map tTranslate $ iterate (^+^ Point2 s 0) zeroPoint)

makeColumn :: Transformable f => DisplaySpace -> [f] -> [f]
makeColumn s = zipWith ($) (map tTranslate $ iterate (^+^ Point2 0 s) zeroPoint)

makeGrid :: Transformable f => DisplaySpace -> Int -> Int -> [f] -> [f]
makeGrid s width height = take (width * height) . concat . makeColumn s . map (makeRow s) . breakList width

increasingAngles :: (Floating s, Num s) => [Angle s]
increasingAngles = take 23 $ iterate ( ^+^ (15 @@ deg)) (15 @@ deg)

arc :: Angle DisplaySpace -> CompoundTree
arc = rawPlot . plotArc

makeLine :: DisplaySpace -> String -> [CompoundTree]
makeLine y = makeRow y . map glyph

paraGrid :: DisplaySpace -> String -> [CompoundTree]
paraGrid s = concat . makeColumn s . map (makeRow s . map glyph) . lines

arcSet :: CompoundTree
arcSet = foldl1 cAdd . makeGrid 3 6 4 . map arc $ increasingAngles

overlap :: DefaultOverlap o => [STree o t] -> STree o t
overlap = foldl1 (ShapeOverlap defaultOverlap)

circle :: CompoundTree
circle = tTranslateXY 0.0 (-0.5) $ arc fullTurn

solid :: Color -> CompoundTree -> ShapeTree
solid color = ShapeGroup . SRep 0 (Solid color)

textureWith :: PictureRef PictId -> CompoundTree -> ShapeTree
textureWith pict = ShapeGroup . SRep 0 (Texture pict)

raw :: [Vertex DisplaySpace] -> CompoundTree
raw = ShapeGroup . Raw

rawPlot :: Plot DisplaySpace -> CompoundTree
rawPlot = ShapeGroup . RawPlot
