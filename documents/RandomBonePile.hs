{-
instance Random s => Random (Point2 s) where
  random = runRand $ do x <- getRandom; y <- getRandom; return (Point2 x y)
  randomR (Point2 x0 y0, Point2 x1 y1) = runRand $ do x <- getRandomR (x0, x1)
                                                      y <- getRandomR (y0, y1)
                                                      return (Point2 x y)
-}
{-
instance (Num token, Random token, Fuzzy rep) => Fuzzy (SRep token textureRep rep) where
  fuzz size = do color <- getRandom
                 token <- getRandomR(0,32768)
                 child <- fuzz (size - 1)
                 return . setToken token . withColor color $ child
-}
{-
instance Fuzzy (ShapeTree Int s) where
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
                                  return $ addOver above below
                          1 -> do (aboveSize, belowSize) <- fuzzyBreak (size - 1)
                                  above <- fuzz aboveSize
                                  below <- fuzz belowSize
                                  return $ subtractFrom above below
                          2 -> do (aboveSize, belowSize) <- fuzzyBreak (size - 1)
                                  above <- fuzz aboveSize
                                  below <- fuzz belowSize
                                  return $ cContinue above below
                          3 -> do transformType <- getRandom
                                  child <- fuzz (size - 1)
                                  return $ STransform transformType child
-}
--type RawTree rep = ShapeTree Align (SRep Int (PictureRef PictId) (CompoundTree rep))
