{-# LANGUAGE FlexibleContexts #-}

module Graphics.Gudni.Util.Draw
  ( lPath
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
  , cAdd
  , cSubtract
  , cContinue
  , wrapGlyph
  )
where

import Graphics.Gudni.Interface.Input
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Plot
import Graphics.Gudni.Util.Scaffolding

import Data.Char (ord)
import Control.Lens

-- | Combine two subtrees by adding them.
cAdd :: HasSpace leaf
     => Glyph (STree Compound leaf)
     -> Glyph (STree Compound leaf)
     -> Glyph (STree Compound leaf)
cAdd = combineGlyph (SMeld CompoundAdd)

-- | Combine two subtrees by substracting the first from the second.
cSubtract :: HasSpace leaf
          => Glyph (STree Compound leaf)
          -> Glyph (STree Compound leaf)
          -> Glyph (STree Compound leaf)
cSubtract = combineGlyph (SMeld CompoundSubtract)

-- | Combine two subtrees by nuetrally concatenating their component outlines.
cContinue :: HasSpace leaf
          => Glyph (STree Compound leaf)
          -> Glyph (STree Compound leaf)
          -> Glyph (STree Compound leaf)
cContinue = combineGlyph (SMeld CompoundContinue)

lPath :: (Space s)
      => String
      -> Glyph (CompoundTree s)
lPath name =
  case curveLibrary name of
    Just path -> wrapGlyph $ closeOpenCurve path
    Nothing -> unitSquare

rectangle :: (Floating s, Real s, Fractional s)
          => Point2 s
          -> Glyph (CompoundTree s)
rectangle v = Glyph v
            . SLeaf
            . segmentsToOutline
            $ [[ straight 0      0
               , straight (v ^. pX) 0
               , straight (v ^. pX) (v ^. pY)
               , straight 0         (v ^. pY)
               ]]

unitSquare :: (Space s)
           => Glyph (CompoundTree s)
unitSquare = rectangle (Point2 1 1)

openRectangle :: (Space s)
              => s
              -> Point2 s
              -> Glyph (CompoundTree s)
openRectangle s p = let strokeDelta = Point2 s s in
                    cSubtract (rectangle p)
                              (mapGlyph (tTranslate strokeDelta) $ rectangle (p ^-^ (strokeDelta ^* 2)))

diamond :: (Space s)
        => Point2 s
        -> Glyph (CompoundTree s)
diamond point = let box = Box zeroPoint point
                    t = lerp 0.5 (box ^. topLeftBox    ) (box ^. topRightBox   )
                    r = lerp 0.5 (box ^. topRightBox   ) (box ^. bottomRightBox)
                    b = lerp 0.5 (box ^. bottomRightBox) (box ^. bottomLeftBox )
                    l = lerp 0.5 (box ^. bottomLeftBox ) (box ^. topLeftBox    )
                in  raw  [Straight t, Straight r, Straight b, Straight l]

line :: (Space s)
     => s
     -> Point2 s
     -> Point2 s
     -> Glyph (CompoundTree s)
line stroke p0 p1 =
      let vector = p0 ^-^ p1
          normal = vector ^/ norm vector
          leftNormal = rotate90 normal ^* stroke
          rightNormal = rotate270 normal ^* stroke
      in  wrapGlyph . segmentsToOutline $
        [ [ Seg (p0 ^+^ rightNormal) Nothing
          , Seg (p0 ^+^ leftNormal ) Nothing
          , Seg (p1 ^+^ leftNormal ) Nothing
          , Seg (p1 ^+^ rightNormal) Nothing
          ]]

makeRow :: HasSpace rep
        => SpaceOf rep
        -> [Glyph (STree o rep)]
        -> [Glyph (STree o rep)]
makeRow s = zipWith ($) (map (mapGlyph . tTranslate) $ iterate (^+^ Point2 s 0) zeroPoint)

makeColumn :: HasSpace rep
           => SpaceOf rep
           -> [Glyph (STree o rep)]
           -> [Glyph (STree o rep)]
makeColumn s = zipWith ($) (map (mapGlyph . tTranslate) $ iterate (^+^ Point2 0 s) zeroPoint)

makeGrid :: (HasSpace rep, HasDefault o)
         => SpaceOf rep
         -> Int
         -> Int
         -> [Glyph (STree o rep)]
         -> Glyph (STree o rep)
makeGrid s width height = overlap . take height . makeColumn s . map (overlap . take width . makeRow s) . breakList width

increasingAngles :: (Floating s, Num s) => [Angle s]
increasingAngles = take 23 $ iterate ( ^+^ (15 @@ deg)) (15 @@ deg)

arc :: (Space s)
    => Angle s
    -> Glyph (CompoundTree s)
arc = rawCurve . makeArc

makeLine :: (SimpleSpace s)
         => s
         -> [Glyph (CompoundTree s)]
         -> Glyph (CompoundTree s)
makeLine y = overlap . makeRow y

paraGrid :: (SimpleSpace s)
         => s
         -> [[Glyph (CompoundTree s)]]
         -> Glyph (CompoundTree s)
paraGrid s = overlap . makeColumn s . map (overlap . makeRow s)

circle :: (Space s)
       => Glyph (CompoundTree s)
circle = overlap [ openRectangle 0.025 (Point2 1 1)
                 , tScale 0.5 $ mapGlyph (tTranslateXY 1 (1)) $ arc fullTurn
                 ]

solid :: Color
      -> Glyph (CompoundTree s)
      -> Glyph (ShapeTree Int s)
solid color = mapGlyph (SLeaf . SRep 0 (Solid color))

textureWith :: PictureUsage PictId
            -> Glyph (CompoundTree s )
            -> Glyph (ShapeTree Int s)
textureWith pict = mapGlyph (SLeaf . SRep 0 (Texture pict))

raw :: (Space s)
    => [Segment s] -> Glyph (CompoundTree s)
raw = wrapGlyph . segmentsToOutline . pure

rawCurve :: (Space s)
         => OpenCurve s
         -> Glyph (CompoundTree s)
rawCurve = wrapGlyph . closeOpenCurve

wrapGlyph :: (SimpleSpace s)
          => [Outline s]
          -> Glyph (CompoundTree s)
wrapGlyph outlines =
  let box = boxOf outlines
  in  Glyph (makePoint (widthOf box) (heightOf box)) $
      SLeaf outlines
