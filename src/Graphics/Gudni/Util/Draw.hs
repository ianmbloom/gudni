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

glyphR :: Angle SubSpace -> Point2 SubSpace -> Glyph SubSpace -> CompoundTree
glyphR a p = sTranslate p . sRotate a . SLeaf . RawGlyph

glyph :: Glyph SubSpace -> CompoundTree
glyph = SLeaf . RawGlyph

rectangle :: Point2 SubSpace -> CompoundTree
rectangle p = SLeaf $ RawRectangle p

unitSquare :: CompoundTree
unitSquare = SLeaf $ RawRectangle (Point2 1 1)

openRectangle :: SubSpace -> Point2 SubSpace -> CompoundTree
openRectangle s p = let strokeDelta = Point2 s s in
                    cSubtract (rectangle p)
                              (sTranslate strokeDelta $ rectangle (p ^-^ (strokeDelta ^* 2)))

diamond :: Point2 SubSpace -> CompoundTree
diamond point = let box = Box zeroPoint point :: BoundingBox
                    t = lerp 0.5 (box ^. topLeftBox    ) (box ^. topRightBox   )
                    r = lerp 0.5 (box ^. topRightBox   ) (box ^. bottomRightBox)
                    b = lerp 0.5 (box ^. bottomRightBox) (box ^. bottomLeftBox )
                    l = lerp 0.5 (box ^. bottomLeftBox ) (box ^. topLeftBox    )
                in raw  [Straight t, Straight r, Straight b, Straight l]

line :: SubSpace -> Point2 SubSpace -> Point2 SubSpace -> CompoundTree
line s a b = SLeaf $ RawLine a b s

makeRow :: Num s => s -> [STree o (Transformer s) rep] -> [STree o (Transformer s) rep]
makeRow s = zipWith ($) (map sTranslate $ iterate (^+^ Point2 s 0) zeroPoint)

makeColumn :: Num s => s -> [STree o (Transformer s) rep] -> [STree o (Transformer s) rep]
makeColumn s = zipWith ($) (map sTranslate $ iterate (^+^ Point2 0 s) zeroPoint)

makeGrid :: (HasDefault o, Num s) => s -> Int -> Int -> [STree o (Transformer s) rep] -> STree o (Transformer s) rep
makeGrid s width height = overlap . take height . makeColumn s . map (overlap . take width . makeRow s) . breakList width

increasingAngles :: (Floating s, Num s) => [Angle s]
increasingAngles = take 23 $ iterate ( ^+^ (15 @@ deg)) (15 @@ deg)

arc :: Angle SubSpace -> CompoundTree
arc = rawCurve . makeArc

makeLine :: SubSpace -> [Glyph SubSpace]-> CompoundTree
makeLine y = overlap . makeRow y . map glyph

paraGrid :: SubSpace -> [[Glyph SubSpace]] -> CompoundTree
paraGrid s = overlap . makeColumn s . map (overlap . makeRow s . map glyph)

overlap :: HasDefault o => [STree o t rep] -> STree o t rep
overlap = foldl1 (SMeld defaultValue)

circle :: CompoundTree
circle = sTranslateXY 0.0 (-0.5) $ arc fullTurn

solid :: Color -> CompoundTree -> ShapeTree Int
solid color = SLeaf . SRep 0 (Solid color)

textureWith :: PictureUsage PictId -> CompoundTree -> ShapeTree Int
textureWith pict = SLeaf . SRep 0 (Texture pict)

raw :: [Segment SubSpace] -> CompoundTree
raw = SLeaf . Raw

rawCurve :: OpenCurve SubSpace -> CompoundTree
rawCurve = SLeaf . RawCurve
