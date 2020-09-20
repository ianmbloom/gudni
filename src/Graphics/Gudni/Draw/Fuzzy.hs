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
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw.Elipse
import Graphics.Gudni.Draw.Rectangle


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

instance Random Compound where
  random = runRand $ do r :: Int <- getRandomR(0,1)
                        return $ case r of
                                  0 -> CompoundAdd
                                  1 -> CompoundSubtract
  randomR _ = random

instance Random Color where
  random = runRand $ do hue       <- getRandomR(0,360)
                        sat       <- getRandomR(0.3,1)
                        lightness <- getRandomR(0.4,0.9)
                        alpha     <- getRandomR(0.2,0.5)
                        return $ transparent alpha $ hslColor hue sat lightness
  randomR _ = random

instance Random CodePoint where
  random = runRand $ do c <- getRandom; return $ CodePoint c
  randomR (CodePoint l, CodePoint h) = runRand $ do c <- getRandomR (l, h); return $ CodePoint c

removeDoubleFalse (False:False:bs) = removeDoubleFalse (False:bs)
removeDoubleFalse (a:bs)           = a:removeDoubleFalse bs
removeDoubleFalse []               = []

fuzzyCurve :: ( Space s
              , Random s
              , RandomGen g)
           => Point2 s
           -> Int
           -> Rand g (ShapeTree Int s)
fuzzyCurve range len = do
  color <- getRandom
  token <- getRandomR(0,32768)
  segmentList <- take len <$> getRandomRs(straightXY 0 0, Straight range)
  return $ setToken token . withColor color . fromSegments $ segmentList

makePairs :: [a] -> [(a,a)]
makePairs (a:b:cs) = (a,b):makePairs cs
makePairs (a:[]) = error "shoudn't happen"
makePairs [] = error "shoudn't happen"

fuzzyRadial :: forall s g
            .  ( Space s
               , Random s
               , RandomGen g)
            => s
            -> s
            -> Int
            -> Rand g (ShapeTree Int s)
fuzzyRadial minRad maxRad len = do
  color <- getRandom
  token <- getRandomR(0,32768)
  boolList <- removeDoubleFalse <$> getRandomRs(True, False)
  angleList <- getRandomRs(0,1)
  radiusList <- getRandomRs(minRad,maxRad)
  let pointPairList   = makePairs $ zipWith (\ a r -> Point2 (cosA (a @@ turn) * r) (sinA (a @@ turn) * r)) angleList radiusList
      makeSegment isCurved (onCurve, offCurve) = if isCurved then Straight onCurve else Curved onCurve offCurve

      segmentList = take len $ zipWith makeSegment boolList pointPairList
  return . setToken token . withColor color . fromSegments $ segmentList

fuzzyGray :: ( Space s
             , Random s
             , RandomGen g)
          => Point2 s
          -> Int
          -> Rand g (ShapeTree Int s)
fuzzyGray range len = do
  token <- getRandomR(0,32768)
  segmentList <- take len <$> getRandomRs(straightXY 0 0, straightXY 10 10)
  return . setToken token . withColor (transparent 0.3 white) . fromSegments $ segmentList

fuzzyCircle :: ( Space s
               , Random s
               , RandomGen g)
            => Point2 s
            -> s
            -> s
            -> Rand g (ShapeTree Int s)
fuzzyCircle range minRad maxRad =
  do  color <- getRandom
      token <- getRandomR(0,32768)
      radius<- getRandomR(minRad,maxRad)
      point <- getRandomR(makePoint 0 0, range)
      return . translateBy point . scaleBy radius . setToken token . withColor color . mask $ circle

fuzzyCircleGradient :: ( Space s
                       , Random s
                       , RandomGen g)
                    => Point2 s
                    -> s
                    -> s
                    -> Rand g (ShapeTree Int s)
fuzzyCircleGradient range minRad maxRad =
  do  color <- getRandom
      color2 <- getRandom
      token <- getRandomR(0,32768)
      radius<- getRandomR(minRad,maxRad)
      point <- getRandomR(makePoint 0 0, range)
      return . translateBy point .
               scaleBy radius .
               setToken token .
               withRadialGradient zeroPoint 0 color 1 color2 .
               mask $
               circle

fuzzyGlyph :: ( IsStyle style
              , HasSpace style
              , Random (SpaceOf style)
              , Num (TokenOf style)
              , Random (TokenOf style), RandomGen g)
           => V.Vector (CompoundLayout style)
           -> Point2 (SpaceOf style)
           -> SpaceOf style
           -> SpaceOf style
           -> Rand g (Layout style)
fuzzyGlyph glyphs range minRad maxRad =
  do  i <- getRandomR(0, V.length glyphs - 1)
      let g = (V.!) glyphs i
      color  <- getRandom
      token  <- getRandomR(0,32768)
      angle  <- getRandomR(0,360)
      radius <- getRandomR(minRad,maxRad)
      point  <- getRandomR(makePoint 0 0, range)
      return . translateBy point . scaleBy radius . rotateBy (angle @@ deg) . setToken token . withColor color $ g

fuzzySquare :: ( Space s
               , Random s
               , RandomGen g)
            => Point2 s
            -> s
            -> s
            -> Rand g (ShapeTree Int s)
fuzzySquare range minRad maxRad =
    do  color <- getRandom
        token <- getRandomR(0,32768)
        radius<- getRandomR(minRad,maxRad)
        point <- getRandomR(makePoint 0 0, range)
        return . translateBy point . scaleBy radius . setToken token . withColor color . mask $ rectangle (Point2 1 1)
