{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Util.Shuffle
 ( shuffleMutable
 , shuffleImmutable
 , pullFromBag
 )
where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Set as Z

import Control.Monad.ST.Trans
import Control.Monad.ST.Trans.Internal
import Control.Monad.State
import Control.Monad.Random

shuffleMutable :: forall s a
               .  MV.MVector s a
               -> STT s (Rand StdGen) ()
{-# INLINABLE shuffleMutable #-}
shuffleMutable mutable = go mutable (MV.length mutable - 1)
  where
    go :: MV.MVector s a -> Int -> STT s (Rand StdGen) ()
    {-# INLINE go #-}
    go _ 0   =  pure ()
    go v maxInd =
      do
        ind <- lift $ getRandomR (0, maxInd)
        liftST $ MV.swap v 0 ind
        go (MV.tail v) (maxInd - 1)

shuffleImmutable :: forall a
                 .  V.Vector a
                 -> Rand StdGen (V.Vector a)
shuffleImmutable vector
  | V.length vector <= 1 = pure vector
  | otherwise =
      runSTT $
      do mutable  <- liftST $ V.thaw vector
         shuffleMutable mutable
         liftST $ V.freeze mutable

pullFromBag :: Z.Set a -> Rand StdGen (Maybe a, Z.Set a)
pullFromBag bag =
  let size = Z.size bag
  in
  if size > 0
  then do pick <- getRandomR(0,size - 1) :: Rand StdGen Int
          return (Just . Z.elemAt pick $ bag, Z.deleteAt pick bag)
  else return (Nothing, bag)
