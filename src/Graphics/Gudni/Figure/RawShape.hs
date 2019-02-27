{-# LANGUAGE GADTs, TemplateHaskell, LambdaCase, FlexibleContexts, ScopedTypeVariables, KindSignatures #-}

module Graphics.Gudni.Figure.RawShape
  ( RawShape (..)
  , addFont
  , rawShapeToCurve
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Glyph
import Graphics.Gudni.Figure.Plot
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
     RawPlot      :: Plot DisplaySpace -> RawShape
     Raw          :: [Vertex DisplaySpace] -> RawShape
     deriving (Show, Eq, Ord)

instance Hashable RawShape where
    hashWithSalt s (RawBox a b     ) = s `hashWithSalt` (0::Int) `hashWithSalt` a `hashWithSalt` b
    hashWithSalt s (RawRectangle a ) = s `hashWithSalt` (1::Int) `hashWithSalt` a
    hashWithSalt s (RawGlyph  a    ) = s `hashWithSalt` (3::Int) `hashWithSalt` a
    hashWithSalt s (RawLine   a b c) = s `hashWithSalt` (4::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
    hashWithSalt s (RawPlot   a    ) = s `hashWithSalt` (6::Int) `hashWithSalt` a
    hashWithSalt s (Raw       a    ) = s `hashWithSalt` (7::Int) `hashWithSalt` a

instance NFData RawShape where
  rnf = \case
     RawBox a b      -> a `deepseq` b `deepseq`             ()
     RawRectangle a  -> a `deepseq`                         ()
     RawGlyph  a     -> a `deepseq`                         ()
     RawLine   a b c -> a `deepseq` b `deepseq` c `deepseq` ()
     RawPlot   a     -> a `deepseq`                         ()
     Raw       a     -> a `deepseq`                         ()

-- remove duplicates
removeDuplicateVertices :: (Eq s) => [Vertex s] -> [Vertex s]
removeDuplicateVertices (a:b:vs) =
  let rest = removeDuplicateVertices (b:vs) in
  if a == b then rest else a:rest
removeDuplicateVertices v = v

expandVerts :: (Ord s, Show s, Fractional s, Num s) => [Vertex s] -> [Point2 s]
expandVerts vs = let removed = removeDuplicateVertices vs
                 in  if length removed <= 1
                     then []
                     else let (s, rest, l) = takeFirstLast removed
                          in  expandVerts' $
                              case (s ^. isOnCurve, l ^. isOnCurve) of
                                 (True , True)  -> vs ++ [s]
                                 (True , False) -> vs ++ [s]
                                 (False, True)  -> l:vs
                                 (False, False) -> error "bicubic splines not implemented."

mid :: (Fractional s, Num s) => Point2 s -> Point2 s -> Point2 s
mid v0 v1 = lerp 0.5 v0 v1

expandVerts' :: (Fractional s, Num s) => [Vertex s] -> [Point2 s]
expandVerts' (Vert True v0: Vert True  v1:rest) = v0:mid v0 v1:expandVerts' (Vert True v1:rest)
expandVerts' (Vert True v0: Vert False v1:rest) = v0:v1:expandVerts' rest
expandVerts' [Vert True v0]       = []
expandVerts' []                   = error "reached end of vert list"
expandVerts' (Vert False v0:rest) = error "double off curve vertices"

pairPoints :: [Point2 s] -> [CurvePair (Point2 s)]
pairPoints (v0:v1:rest) = (CurvePair v0 v1):pairPoints rest
pairPoints [] = []
pairPoints [v0] = []

rawShapeToCurve :: RawShape -> [Outline DisplaySpace]
rawShapeToCurve = (map (Outline . pairPoints)) . rawShapeToCurve'

rawShapeToCurve' :: RawShape -> [[Point2 DisplaySpace]]
rawShapeToCurve' shape =
  case shape of
    RawBox stroke v ->
      map expandVerts
        [ [ Vert True $ makePoint 0      0
          , Vert True $ makePoint (v ^. pX) 0
          , Vert True $ makePoint (v ^. pX) (v ^. pY)
          , Vert True $ makePoint 0      (v ^. pY)
          ],
          [ Vert True $ makePoint (0    + toXOrtho stroke) (0    + toYOrtho stroke)
          , Vert True $ makePoint (0    + toXOrtho stroke) (v ^. pY - toYOrtho stroke)
          , Vert True $ makePoint (v ^. pX - toXOrtho stroke) (v ^. pY - toYOrtho stroke)
          , Vert True $ makePoint (v ^. pX - toXOrtho stroke) (0    + toYOrtho stroke)
          ] ]
    RawRectangle v ->
        [expandVerts [ Vert True $ makePoint 0      0
                              , Vert True $ makePoint (v ^. pX) 0
                              , Vert True $ makePoint (v ^. pX) (v ^. pY)
                              , Vert True $ makePoint 0         (v ^. pY)
                              ] ]
    RawGlyph glyph -> glyphVertices glyph
    Raw vertices ->
        [expandVerts vertices]
    RawLine p0 p1 stroke ->
      let vector = p0 ^-^ p1
          normal = vector ^/ norm vector
          leftNormal = rotate90 normal ^* stroke
          rightNormal = rotate270 normal ^* stroke
      in
        [
              expandVerts  [ Vert True (p0 ^+^ rightNormal)
                           , Vert True (p0 ^+^ leftNormal)
                           , Vert True (p1 ^+^ leftNormal)
                           , Vert True (p1 ^+^ rightNormal)
                           ]]
    RawPlot p -> [expandVerts $ expandPlot p]
