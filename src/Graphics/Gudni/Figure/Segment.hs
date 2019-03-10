{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Angle
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Segments are a typesafe way to describe the components of a bezier curve sequence.

module Graphics.Gudni.Figure.Segment
  ( Segment (..)
  , anchor
  , control
  , pattern Straight
  , straight
  , pattern Curved
  , curved
  , hasControl
  , overSegment
  , randomSegmentFromPoints
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point

import Control.Lens

import System.Random
import Control.Monad.Random
import Control.DeepSeq
import Data.Hashable

-- | A segment is a typesafe way to describe on link in a quadradic bezier curve.
-- multiple on curves points can occur in sequence but multiple control points cannot.
data Segment s = Seg
  { _anchor  :: Point2 s
  , _control :: Maybe (Point2 s) }
  deriving (Eq, Ord)
makeLenses ''Segment

-- | Pattern synonym for a segment with no control point.
pattern Straight p = Seg p Nothing
-- | Make a straight segment from the component dimensions
straight :: Ortho XDimension s -> Ortho YDimension s -> Segment s
straight x y = Seg (makePoint x y) Nothing

-- | Pattern synonym for a segment with a control point.
pattern Curved p c = Seg p (Just c)

-- | Make a curved segment from the component dimensions.
curved :: Ortho XDimension s -> Ortho YDimension s -> Ortho XDimension s -> Ortho YDimension s -> Segment s
curved x y cx cy = Seg (makePoint x y) (Just (makePoint cx cy))

-- | Map over the points of a segment.
overSegment :: (Point2 s -> Point2 z) -> Segment s -> Segment z
overSegment f (Seg o c) = Seg (f o) (fmap f c)

-- | Return true if the segment has a control point.
hasControl :: Segment s -> Bool
hasControl (Seg _ (Just _)) = True
hasControl (Seg _ Nothing ) = False

instance Random s => Random (Segment s) where
  random = runRand $ do o <- getRandom
                        v <- getRandom
                        if o
                        then do c <- getRandom
                                return $ Seg v (Just c)
                        else    return $ Seg v Nothing
  randomR (Seg p0 _, Seg p1 _) = runRand $ do o <- getRandom
                                              v <- getRandomR (p0, p1)
                                              if o
                                              then do c <- getRandomR (p0, p1)
                                                      return $ Seg v (Just c)
                                              else    return $ Seg v Nothing

-- | Generate a random segment from inside the boundaries of a range of points.
randomSegmentFromPoints :: (RandomGen g, Random s) => (Point2 s, Point2 s) -> g -> (Segment s, g)
randomSegmentFromPoints (p0, p1) = randomR (Seg p0 Nothing, Seg p1 Nothing)

instance NFData s => NFData (Segment s) where
  rnf (Seg o c) = o `deepseq` c `deepseq` ()
instance Hashable s => Hashable (Segment s) where
  hashWithSalt s (Seg o c) = s `hashWithSalt` o `hashWithSalt` c
instance Show s => Show (Segment s) where
  show (Seg o mc) = "V-" ++ show o ++ (case mc of Nothing -> ""; Just c -> "C-" ++ show c)
