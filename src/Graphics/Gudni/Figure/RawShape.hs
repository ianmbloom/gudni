{-# LANGUAGE GADTs, TemplateHaskell, LambdaCase, FlexibleContexts, ScopedTypeVariables, KindSignatures #-}

module Graphics.Gudni.Figure.RawShape
  ( RawShape (..)
  , addFont
  , rawShapeToOutlines
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Segment
import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Glyph
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Outline

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Monad.Memo
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Memo.Class
import Control.Monad.Random
import Control.Lens
import Control.DeepSeq

import Data.Hashable
import Data.Int
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B

import System.Random

data RawShape where
     RawBox       :: DisplaySpace -> Point2 DisplaySpace -> RawShape
     RawRectangle :: Point2 DisplaySpace -> RawShape
     RawGlyph     :: Glyph DisplaySpace -> RawShape
     RawLine      :: Point2 DisplaySpace -> Point2 DisplaySpace -> DisplaySpace -> RawShape
     RawCurve     :: OpenCurve DisplaySpace -> RawShape
     Raw          :: [Segment DisplaySpace] -> RawShape
     deriving (Show, Eq, Ord)


-- remove duplicates
removeDuplicateVertices :: (Eq s) => [Segment s] -> [Segment s]
removeDuplicateVertices (a:b:vs) =
  let rest = removeDuplicateVertices (b:vs) in
  if a ^. anchor == b ^. anchor then rest else a:rest
removeDuplicateVertices v = v

expandVerts :: (Ord s, Show s, Fractional s, Num s) => [Segment s] -> [CurvePair (Point2 s)]
expandVerts vs = let removed = removeDuplicateVertices vs
                 in  if length removed <= 1
                     then []
                     else segmentsToCurvePairs removed

mid :: (Fractional s, Num s) => Point2 s -> Point2 s -> Point2 s
mid v0 v1 = lerp 0.5 v0 v1

segmentsToCurvePairs segments = segmentsToCurvePairs' (head segments ^. anchor) segments

segmentsToCurvePairs' :: (Fractional s, Num s) => (Point2 s) -> [Segment s] -> [CurvePair (Point2 s)]
segmentsToCurvePairs' first segs = case segs of
      (Seg v0 Nothing:[])             -> CurvePair v0 (mid v0 first):[]
      (Seg v0 Nothing:Seg v1 mC:rest) -> CurvePair v0 (mid v0 v1):segmentsToCurvePairs' first (Seg v1 mC:rest)
      (Seg v0 (Just c):rest)          -> CurvePair v0 c:segmentsToCurvePairs' first rest
      []                              -> []


segmentsToOutline :: [[Segment DisplaySpace]] -> [Outline DisplaySpace]
segmentsToOutline = map (Outline . segmentsToCurvePairs)

rawShapeToOutlines :: RawShape -> [Outline DisplaySpace]
rawShapeToOutlines shape =
  case shape of
    RawBox stroke v ->
      segmentsToOutline
        [ [ straight 0      0
          , straight (v ^. pX) 0
          , straight (v ^. pX) (v ^. pY)
          , straight 0      (v ^. pY)
          ],
          [ straight (0    + toXOrtho stroke) (0    + toYOrtho stroke)
          , straight (0    + toXOrtho stroke) (v ^. pY - toYOrtho stroke)
          , straight (v ^. pX - toXOrtho stroke) (v ^. pY - toYOrtho stroke)
          , straight (v ^. pX - toXOrtho stroke) (0    + toYOrtho stroke)
          ] ]
    RawRectangle v ->
        segmentsToOutline
        [[ straight 0      0
         , straight (v ^. pX) 0
         , straight (v ^. pX) (v ^. pY)
         , straight 0         (v ^. pY)
         ] ]
    RawGlyph glyph -> glyphVertices glyph
    Raw segments -> segmentsToOutline [segments]
    RawLine p0 p1 stroke ->
      let vector = p0 ^-^ p1
          normal = vector ^/ norm vector
          leftNormal = rotate90 normal ^* stroke
          rightNormal = rotate270 normal ^* stroke
      in  segmentsToOutline
        [ [ Seg (p0 ^+^ rightNormal) Nothing
          , Seg (p0 ^+^ leftNormal ) Nothing
          , Seg (p1 ^+^ leftNormal ) Nothing
          , Seg (p1 ^+^ rightNormal) Nothing
          ]]
    RawCurve p -> segmentsToOutline [curveToOutline p]

curveToOutline = undefined

instance Hashable RawShape where
    hashWithSalt s (RawBox a b     ) = s `hashWithSalt` (0::Int) `hashWithSalt` a `hashWithSalt` b
    hashWithSalt s (RawRectangle a ) = s `hashWithSalt` (1::Int) `hashWithSalt` a
    hashWithSalt s (RawGlyph  a    ) = s `hashWithSalt` (3::Int) `hashWithSalt` a
    hashWithSalt s (RawLine   a b c) = s `hashWithSalt` (4::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
    hashWithSalt s (RawCurve  a    ) = s `hashWithSalt` (6::Int) `hashWithSalt` a
    hashWithSalt s (Raw       a    ) = s `hashWithSalt` (7::Int) `hashWithSalt` a

instance NFData RawShape where
  rnf = \case
     RawBox a b      -> a `deepseq` b `deepseq`             ()
     RawRectangle a  -> a `deepseq`                         ()
     RawGlyph  a     -> a `deepseq`                         ()
     RawLine   a b c -> a `deepseq` b `deepseq` c `deepseq` ()
     RawCurve  a     -> a `deepseq`                         ()
     Raw       a     -> a `deepseq`                         ()
