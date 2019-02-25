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
  , fuzzySquare
  )
where

import Graphics.Gudni.Figure
import Control.Monad.Random
import Graphics.Gudni.Util.Draw

class Fuzzy a where
  fuzz :: RandomGen g => Int -> Rand g a

fuzzyBreak :: RandomGen g => Int -> Rand g (Int, Int)
fuzzyBreak size = do b <- getRandomR (0,size)
                     return (b, size - b)

instance Random CombineType where
  random = runRand $ do r :: Int <- getRandomR(0,2)
                        return $ case r of
                                  0 -> CombineContinue
                                  1 -> CombineAdd
                                  2 -> CombineSubtract
  randomR _ = random

instance Random Color where
  random = runRand $ do hue       <- getRandomR(0,360)
                        sat       <- getRandomR(0.5,1)
                        lightness <- getRandomR(0.2,1)
                        alpha     <- getRandomR(0.2,0.4)
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
                            return $ undefined -- RawGlyph codePoint
                    4 -> do len <- getRandomR(3,100)
                            boolList <- removeDoubleFalse <$> getRandomRs(True, False)
                            pointList <- getRandomRs(makePoint 0 0, makePoint 10 10)
                            let vertexList = take len $ zipWith Vert (True:boolList) pointList
                            return $ Raw $ vertexList

removeDoubleFalse (False:False:bs) = removeDoubleFalse (False:bs)
removeDoubleFalse (a:bs)           = a:removeDoubleFalse bs
removeDoubleFalse []               = []

instance (Num token, Random token, Fuzzy rep) => Fuzzy (SRep token substance rep) where
  fuzz size = do color <- Solid <$> getRandom
                 token <- getRandomR(0,32768)
                 child <- fuzz (size - 1)
                 return $ SRep token color child

instance Fuzzy ShapeTree where
  fuzz size = do if (size < 1)
                 then do child <- fuzz (size - 1)
                         return $ SLeaf child
                 else do a :: Int <- getRandomR (0,1)
                         case a of
                           0 -> do transformType :: TransformType <- getRandom
                                   child <- fuzz (size - 1)
                                   return $ STransform transformType child
                           1 -> do (aboveSize, belowSize) <- fuzzyBreak (size - 1)
                                   above <- fuzz aboveSize
                                   below <- fuzz belowSize
                                   return $ SOverlap () above below

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

fuzzyCurve :: (RandomGen g) => Point2 DisplaySpace -> Int -> Rand g ShapeTree
fuzzyCurve range len = do
  color <- Solid <$> getRandom
  token <- getRandomR(0,32768)
  boolList <- removeDoubleFalse <$> getRandomRs(True, False)
  pointList <- getRandomRs(makePoint 0 0, range)
  let vertexList = take len $ zipWith Vert (True:boolList) pointList
  return $ SLeaf $ SRep token color $ SLeaf $ Raw vertexList

fuzzyRadial :: (RandomGen g) => DisplaySpace -> DisplaySpace -> Int -> Rand g ShapeTree
fuzzyRadial minRad maxRad len = do
  color <- Solid <$> getRandom
  token <- getRandomR(0,32768)
  boolList <- removeDoubleFalse <$> getRandomRs(True, False)
  angleList <- getRandomRs(0,1)
  radiusList <- getRandomRs(minRad,maxRad)
  let pointList = zipWith (\ a r -> Point2 (cosA (a @@ turn) * r) (sinA (a @@ turn) * r)) angleList radiusList
  let vertexList = take len $ zipWith Vert (True:boolList) pointList
  return $ SLeaf $ SRep token color $ SLeaf $ Raw vertexList


fuzzyGray :: (RandomGen g) => Point2 DisplaySpace -> Int -> Rand g ShapeTree
fuzzyGray range len = do
  token <- getRandomR(0,32768)
  boolList <- removeDoubleFalse <$> getRandomRs(True, False)
  pointList <- getRandomRs(makePoint 0 0, range)
  let vertexList = take len $ zipWith Vert (True:boolList) pointList
  return $ SLeaf $ SRep token (Solid $ transparent 0.3 white) $ SLeaf $ Raw vertexList

fuzzyCircle :: (RandomGen g) => Point2 DisplaySpace -> DisplaySpace -> DisplaySpace -> Rand g ShapeTree
fuzzyCircle range  minRad maxRad = do
  color <- Solid <$> getRandom
  token <- getRandomR(0,32768)
  radius<- getRandomR(minRad,maxRad)
  point <- getRandomR(makePoint 0 0, range)
  return $ tTranslate point $ tScale radius $ SLeaf $ SRep token color circle

fuzzySquare :: (RandomGen g) => Point2 DisplaySpace -> DisplaySpace -> DisplaySpace -> Rand g ShapeTree
fuzzySquare range  minRad maxRad = do
  color <- Solid <$> getRandom
  token <- getRandomR(0,32768)
  radius<- getRandomR(minRad,maxRad)
  point <- getRandomR(makePoint 0 0, range)
  return $ tTranslate point $ tScale radius $ SLeaf $ SRep token color unitSquare
