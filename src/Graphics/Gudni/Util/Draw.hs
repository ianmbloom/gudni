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
  , overlap
  , circle
  , solid
  , textureWith
  , raw
  , rawCurve
  )
where

import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Plot

import Data.Char (ord)
import Control.Lens

lPath :: String -> CompoundTree
lPath name =
  case curveLibrary name of
    Just path -> SLeaf $ RawCurve path
    Nothing -> rectangle (Point2 1 1)

glyphR :: Angle DisplaySpace -> Point2 DisplaySpace -> Glyph DisplaySpace -> CompoundTree
glyphR a p = sTranslate p . sRotate a . SLeaf . RawGlyph

glyph :: Glyph DisplaySpace -> CompoundTree
glyph = SLeaf . RawGlyph

rectangle :: Point2 DisplaySpace -> CompoundTree
rectangle p = SLeaf $ RawRectangle p

unitSquare :: CompoundTree
unitSquare = SLeaf $ RawRectangle (Point2 1 1)

openRectangle :: DisplaySpace -> Point2 DisplaySpace -> CompoundTree
openRectangle s p = let strokeDelta = Point2 s s in
                    cSubtract (rectangle p)
                              (sTranslate strokeDelta $ rectangle (p ^-^ (strokeDelta ^* 2)))

diamond :: Point2 DisplaySpace -> CompoundTree
diamond point = let box = Box zeroPoint point :: BoundingBox
                    t = lerp 0.5 (box ^. topLeftBox    ) (box ^. topRightBox   )
                    r = lerp 0.5 (box ^. topRightBox   ) (box ^. bottomRightBox)
                    b = lerp 0.5 (box ^. bottomRightBox) (box ^. bottomLeftBox )
                    l = lerp 0.5 (box ^. bottomLeftBox ) (box ^. topLeftBox    )
                in raw  [Straight t, Straight r, Straight b, Straight l]

line :: DisplaySpace -> Point2 DisplaySpace -> Point2 DisplaySpace -> CompoundTree
line s a b = SLeaf $ RawLine a b s

makeRow :: Num s => s -> [STree o (TransformType s) rep] -> [STree o (TransformType s) rep]
makeRow s = zipWith ($) (map sTranslate $ iterate (^+^ Point2 s 0) zeroPoint)

makeColumn :: Num s => s -> [STree o (TransformType s) rep] -> [STree o (TransformType s) rep]
makeColumn s = zipWith ($) (map sTranslate $ iterate (^+^ Point2 0 s) zeroPoint)

makeGrid :: (DefaultOverlap o, Num s) => s -> Int -> Int -> [STree o (TransformType s) rep] -> STree o (TransformType s) rep
makeGrid s width height = overlap . take height . makeColumn s . map (overlap . take width . makeRow s) . breakList width

increasingAngles :: (Floating s, Num s) => [Angle s]
increasingAngles = take 23 $ iterate ( ^+^ (15 @@ deg)) (15 @@ deg)

arc :: Angle DisplaySpace -> CompoundTree
arc = rawCurve . makeArc

makeLine :: DisplaySpace -> [Glyph DisplaySpace]-> CompoundTree
makeLine y = overlap . makeRow y . map glyph

paraGrid :: DisplaySpace -> [[Glyph DisplaySpace]] -> CompoundTree
paraGrid s = overlap . makeColumn s . map (overlap . makeRow s . map glyph)

overlap :: DefaultOverlap o => [STree o t rep] -> STree o t rep
overlap = foldl1 (SOverlap defaultOverlap)

circle :: CompoundTree
circle = sTranslateXY 0.0 (-0.5) $ arc fullTurn

solid :: Color -> CompoundTree -> ShapeTree
solid color = SLeaf . SRep 0 (Solid color)

textureWith :: PictureRef PictId -> CompoundTree -> ShapeTree
textureWith pict = SLeaf . SRep 0 (Texture pict)

raw :: [Segment DisplaySpace] -> CompoundTree
raw = SLeaf . Raw

rawCurve :: OpenCurve DisplaySpace -> CompoundTree
rawCurve = SLeaf . RawCurve
