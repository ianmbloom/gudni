{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Gudni.Util.Fuzzy
  ( Fuzzy (..)
  , fuzzyBreak
  , fuzzyCurve
  , fuzzyRadial
  , fuzzyGray
  , fuzzyCircle
  , fuzzyGlyph
  , fuzzySquare
  )
where

import Graphics.Gudni.Figure
import Control.Monad.Random
import Graphics.Gudni.Util.Draw
import qualified Data.Vector as V

class Fuzzy a where
  fuzz :: RandomGen g => Int -> Rand g a

fuzzyBreak :: RandomGen g => Int -> Rand g (Int, Int)
fuzzyBreak size = do b <- getRandomR (0,size)
                     return (b, size - b)

instance Random Compound where
  random = runRand $ do r :: Int <- getRandomR(0,2)
                        return $ case r of
                                  0 -> CompoundContinue
                                  1 -> CompoundAdd
                                  2 -> CompoundSubtract
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

instance Fuzzy RawShape where
  fuzz size = do  a :: Int <- getRandomR (3,4)
                  case a of
                    0 -> do border <- getRandom
                            sizeBox <- getRandom
                            return $ RawBox border sizeBox
                    1 -> do sizeBox <- getRandom
                            return $ RawRectangle sizeBox
                    2 -> do start  <- getRandom
                            end    <- getRandom
                            stroke <- getRandom
                            return $ RawLine start end stroke
                    3 -> do codePoint <- getRandomR(CodePoint 0, CodePoint 200)
                            return $ RawBox 1.0 (Point2 10 10) -- undefined -- RawGlyph codePoint
                    4 -> do len <- getRandomR(3,100)
                            segmentList <- take len <$> getRandomRs(straight 0 0, straight 10 10)
                            return $ Raw $ segmentList

removeDoubleFalse (False:False:bs) = removeDoubleFalse (False:bs)
removeDoubleFalse (a:bs)           = a:removeDoubleFalse bs
removeDoubleFalse []               = []

instance (Num token, Random token, Fuzzy rep) => Fuzzy (SRep token substance rep) where
  fuzz size = do color <- Solid <$> getRandom
                 token <- getRandomR(0,32768)
                 child <- fuzz (size - 1)
                 return $ SRep token color child

instance Fuzzy (ShapeTree Int) where
  fuzz size = do if (size < 1)
                 then do child <- fuzz (size - 1)
                         return $ SLeaf child
                 else do a :: Int <- getRandomR (0,1)
                         case a of
                           0 -> do transformType :: Transformer SubSpace <- getRandom
                                   child <- fuzz (size - 1)
                                   return $ STransform transformType child
                           1 -> do (aboveSize, belowSize) <- fuzzyBreak (size - 1)
                                   above <- fuzz aboveSize
                                   below <- fuzz belowSize
                                   return $ SMeld () above below

instance Fuzzy CompoundTree where
    fuzz size = if size < 1
                then do child <- fuzz (size - 1)
                        return $ SLeaf child
                else do a :: Int <- getRandomR (0,3)
                        case a of
                          0 -> do (aboveSize, belowSize) <- fuzzyBreak (size - 1)
                                  above <- fuzz aboveSize
                                  below <- fuzz belowSize
                                  return $ cAdd above below
                          1 -> do (aboveSize, belowSize) <- fuzzyBreak (size - 1)
                                  above <- fuzz aboveSize
                                  below <- fuzz belowSize
                                  return $ cSubtract above below
                          2 -> do (aboveSize, belowSize) <- fuzzyBreak (size - 1)
                                  above <- fuzz aboveSize
                                  below <- fuzz belowSize
                                  return $ cContinue above below
                          3 -> do transformType <- getRandom
                                  child <- fuzz (size - 1)
                                  return $ STransform transformType child

--type RawTree rep = ShapeTree Align (SRep Int (PictureRef PictId) (CompoundTree rep))

fuzzyCurve :: (RandomGen g) => Point2 SubSpace -> Int -> Rand g (ShapeTree Int)
fuzzyCurve range len = do
  color <- Solid <$> getRandom
  token <- getRandomR(0,32768)
  segmentList <- take len <$> getRandomRs(straight 0 0, Straight range)
  return $ SLeaf $ SRep token color $ SLeaf $ Raw segmentList

makePairs :: [a] -> [(a,a)]
makePairs (a:b:cs) = (a,b):makePairs cs
makePairs (a:[]) = error "shoudn't happen"
makePairs [] = error "shoudn't happen"

fuzzyRadial :: (RandomGen g) => SubSpace -> SubSpace -> Int -> Rand g (ShapeTree Int)
fuzzyRadial minRad maxRad len = do
  color <- Solid <$> getRandom
  token <- getRandomR(0,32768)
  boolList <- removeDoubleFalse <$> getRandomRs(True, False)
  angleList <- getRandomRs(0,1)
  radiusList <- getRandomRs(minRad,maxRad)
  let pointPairList   = makePairs $ zipWith (\ a r -> Point2 (cosA (a @@ turn) * r) (sinA (a @@ turn) * r)) angleList radiusList
      makeSegment isCurved (onCurve, offCurve) = if isCurved then Straight onCurve else Curved onCurve offCurve
      segmentList = take len $ zipWith makeSegment boolList pointPairList
  return $ SLeaf $ SRep token color $ SLeaf $ Raw segmentList

fuzzyGray :: (RandomGen g) => Point2 SubSpace -> Int -> Rand g (ShapeTree Int)
fuzzyGray range len = do
  token <- getRandomR(0,32768)
  segmentList <- take len <$> getRandomRs(straight 0 0, straight 10 10)
  return $ SLeaf $ SRep token (Solid $ transparent 0.3 white) $ SLeaf $ Raw segmentList

fuzzyCircle :: (RandomGen g) => Point2 SubSpace -> SubSpace -> SubSpace -> Rand g (ShapeTree Int)
fuzzyCircle range  minRad maxRad = do
  color <- Solid <$> getRandom
  token <- getRandomR(0,32768)
  radius<- getRandomR(minRad,maxRad)
  point <- getRandomR(makePoint 0 0, range)
  return $ sTranslate point $ sScale radius $ SLeaf $ SRep token color circle

fuzzyGlyph :: (RandomGen g) => V.Vector (Glyph SubSpace) -> Point2 SubSpace -> SubSpace -> SubSpace -> Rand g (ShapeTree Int)
fuzzyGlyph glyphs range minRad maxRad =
  do  i <- getRandomR(0, V.length glyphs - 1)
      let g = (V.!) glyphs i
      color  <- Solid <$> getRandom
      token  <- getRandomR(0,32768)
      angle  <- getRandomR(0,360)
      radius <- getRandomR(minRad,maxRad)
      point  <- getRandomR(makePoint 0 0, range)
      return $ sTranslate point $ sScale radius $ sRotate (angle @@ deg) $ SLeaf $ SRep token color $ glyph g


fuzzySquare :: (RandomGen g) => Point2 SubSpace -> SubSpace -> SubSpace -> Rand g (ShapeTree Int)
fuzzySquare range  minRad maxRad = do
  color <- Solid <$> getRandom
  token <- getRandomR(0,32768)
  radius<- getRandomR(minRad,maxRad)
  point <- getRandomR(makePoint 0 0, range)
  return $ sTranslate point $ sScale radius $ SLeaf $ SRep token color unitSquare
