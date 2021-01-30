{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module Graphics.Gudni.Draw.Fuzzy
  ( Fuzzy (..)
  , fuzzyBreak
  , fuzzyCurve
  , fuzzyRadial
  , fuzzyGray
  , fuzzyCircle
  , fuzzyCircleGradient
  , fuzzyGlyph
  , fuzzySquare
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw.Elipse
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Raster.Fabric.Type

import Graphics.Gudni.Util.Segment
import Graphics.Gudni.Util.Debug

import Control.Monad.Random
import qualified Data.Vector as V
import Control.Lens

class Fuzzy a where
  fuzz :: RandomGen g => Int -> Rand g a

fuzzyBreak :: RandomGen g
           => Int
           -> Rand g (Int, Int)
fuzzyBreak size = do b <- getRandomR (0,size)
                     return (b, size - b)

instance (Random s, Space s) => Random (Color s) where
  random = runRand $ do hue   <- getRandomR(0,360)
                        sat   <- getRandomR(0.3,1)
                        value <- getRandomR(0.4,0.9)
                        alpha <- getRandomR(0.2,0.5)
                        return $ hsvaColor hue sat value alpha
  randomR _ = random

instance Random CodePoint where
  random = runRand $ do c <- getRandom; return $ CodePoint c
  randomR (CodePoint l, CodePoint h) = runRand $ do c <- getRandomR (l, h); return $ CodePoint c

removeDoubleFalse (False:False:bs) = removeDoubleFalse (False:bs)
removeDoubleFalse (a:bs)           = a:removeDoubleFalse bs
removeDoubleFalse []               = []

fuzzyCurve :: ( IsStyle s
              , Random (SpaceOf s)
              , Num (TokenOf s)
              , Random (TokenOf s)
              , RandomGen g)
           => Point2 (SpaceOf s)
           -> Int
           -> Rand g (Layout Rgba s)
fuzzyCurve range len = do
  color <- getRandom
  -- token <- getRandomR(0,32768)
  segmentList <- take len <$> getRandomRs(straightXY 0 0, Straight range)
  return $ {-setToken token . -} withColor color . place . fromSegments $ segmentList

makePairs :: [a] -> [(a,a)]
makePairs (a:b:cs) = (a,b):makePairs cs
makePairs (a:[]) = error "shoudn't happen"
makePairs [] = error "shoudn't happen"

fuzzyRadial :: forall s g
            .  ( IsStyle s
               , Random (SpaceOf s)
               , RandomGen g)
            => SpaceOf s
            -> SpaceOf s
            -> Int
            -> Rand g (Layout Rgba s)
fuzzyRadial minRad maxRad len = do
  color <- getRandom
  -- token <- getRandomR(0,32768)
  boolList <- removeDoubleFalse <$> getRandomRs(True, False)
  angleList <- getRandomRs(0,1)
  radiusList <- getRandomRs(minRad,maxRad)
  let pointPairList   = makePairs $ zipWith (\ a r -> Point2 (cosA (a @@ turn) * r) (sinA (a @@ turn) * r)) angleList radiusList
      makeSegment isCurved (onCurve, offCurve) = if isCurved then Straight onCurve else Curved onCurve offCurve

      segmentList = take len $ zipWith makeSegment boolList pointPairList
  return . {- setToken token .-} withColor color . place . fromSegments $ segmentList

fuzzyGray :: ( IsStyle s
             , Random (SpaceOf s)
             , RandomGen g)
          => Point2 (SpaceOf s)
          -> Int
          -> Rand g (Layout Rgba s)
fuzzyGray range len = do
  -- token <- getRandomR(0,32768)
  segmentList <- take len <$> getRandomRs(straightXY 0 0, straightXY 10 10)
  return . {-setToken token .-} withColor (transparent 0.3 white) . place . fromSegments $ segmentList

fuzzyCircle :: ( IsStyle s
               , Random (SpaceOf s)
               , RandomGen g)
            => Point2 (SpaceOf s)
            -> SpaceOf s
            -> SpaceOf s
            -> Rand g (Layout Rgba s)
fuzzyCircle range minRad maxRad =
  do  color <- getRandom
      -- token <- getRandomR(0,32768)
      radius<- getRandomR(minRad,maxRad)
      point <- getRandomR(makePoint 0 0, range)
      return . translateBy point . scaleBy radius . {- setToken token .-} withColor color . place $ circle

fuzzyCircleGradient :: ( IsStyle s
                       , Random (SpaceOf s)
                       , RandomGen g)
                    => Point2 (SpaceOf s)
                    -> SpaceOf s
                    -> SpaceOf s
                    -> Rand g (Layout Rgba s)
fuzzyCircleGradient range minRad maxRad =
  do  color <- getRandom
      color2 <- getRandom
      -- token <- getRandomR(0,32768)
      radius<- getRandomR(minRad,maxRad)
      point <- getRandomR(makePoint 0 0, range)
      return . translateBy point .
               scaleBy radius .
               -- setToken token .
               withRadialGradient zeroPoint 0 color 1 color2 .
               place $
               circle

fuzzyGlyph :: ( IsStyle style
              , Random (SpaceOf style)
              , Num (TokenOf style)
              , Random (TokenOf style), RandomGen g)
           => V.Vector (Layout Mono style)
           -> Point2 (SpaceOf style)
           -> SpaceOf style
           -> SpaceOf style
           -> Rand g (Layout Rgba style)
fuzzyGlyph glyphs range minRad maxRad =
  do  i <- getRandomR(0, V.length glyphs - 1)
      let g = (V.!) glyphs i
      color  <- getRandom
      -- token  <- getRandomR(0,32768)
      angle  <- getRandomR(0,360)
      radius <- getRandomR(minRad,maxRad)
      point  <- getRandomR(makePoint 0 0, range)
      return . translateBy point . scaleBy radius . rotateBy (angle @@ deg) . {- setToken token .-} withColor color $ g

fuzzySquare :: ( IsStyle s
               , Random (SpaceOf s)
               , RandomGen g)
            => Point2 (SpaceOf s)
            -> SpaceOf s
            -> SpaceOf s
            -> Rand g (Layout Rgba s)
fuzzySquare range minRad maxRad =
    do  color <- getRandom
        --- token <- getRandomR(0,32768)
        radius<- getRandomR(minRad,maxRad)
        point <- getRandomR(makePoint 0 0, range)
        return . translateBy point . scaleBy radius . {- setToken token .-} withColor color . place $ rectangle (Point2 1 1)
