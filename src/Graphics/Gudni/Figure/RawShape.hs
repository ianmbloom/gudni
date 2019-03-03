{-# LANGUAGE GADTs, TemplateHaskell, LambdaCase, FlexibleContexts, ScopedTypeVariables, KindSignatures #-}

module Graphics.Gudni.Figure.RawShape
  ( RawShape (..)
  , RawShape_(..)
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

type RawShape = RawShape_ DisplaySpace

data RawShape_ s where
     RawBox       :: s -> Point2 s -> RawShape_ s
     RawRectangle :: Point2 s -> RawShape_ s
     RawGlyph     :: Glyph s -> RawShape_ s
     RawLine      :: Point2 s -> Point2 s -> s -> RawShape_ s
     RawCurve     :: OpenCurve s -> RawShape_ s
     Raw          :: [Segment s] -> RawShape_ s
     deriving (Show, Eq, Ord)

rawShapeToOutlines :: (Eq s, Num s, Floating s) => RawShape_ s -> [Outline s]
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
    RawCurve p -> openCurveToOutline p

instance Hashable s => Hashable (RawShape_ s) where
    hashWithSalt s (RawBox a b     ) = s `hashWithSalt` (0::Int) `hashWithSalt` a `hashWithSalt` b
    hashWithSalt s (RawRectangle a ) = s `hashWithSalt` (1::Int) `hashWithSalt` a
    hashWithSalt s (RawGlyph  a    ) = s `hashWithSalt` (3::Int) `hashWithSalt` a
    hashWithSalt s (RawLine   a b c) = s `hashWithSalt` (4::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
    hashWithSalt s (RawCurve  a    ) = s `hashWithSalt` (6::Int) `hashWithSalt` a
    hashWithSalt s (Raw       a    ) = s `hashWithSalt` (7::Int) `hashWithSalt` a

instance NFData s => NFData (RawShape_ s) where
  rnf = \case
     RawBox a b      -> a `deepseq` b `deepseq`             ()
     RawRectangle a  -> a `deepseq`                         ()
     RawGlyph  a     -> a `deepseq`                         ()
     RawLine   a b c -> a `deepseq` b `deepseq` c `deepseq` ()
     RawCurve  a     -> a `deepseq`                         ()
     Raw       a     -> a `deepseq`                         ()
