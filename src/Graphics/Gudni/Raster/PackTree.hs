{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Raster.PackTree
  ( mkPackTree
  , modifySpan
  , addSpan
  , subtractSpan
  , getMax
  )
where

import Graphics.Gudni.Figure

import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST

data PackTree s = PackTree
  { pTDepth  :: Int
  , pTVector :: VM.STVector s Int
  , pTLeft   :: SubSpace
  , pTRight  :: SubSpace
  }

mkPackTree :: Int -> SubSpace -> SubSpace -> ST s (PackTree s)
mkPackTree depth left right =
   do let size = 2 ^ (depth + 1) - 1
      vector <- VM.replicate size 0
      return $ PackTree
        { pTDepth = depth
        , pTVector = vector
        , pTLeft = left
        , pTRight = right
        }

modifySpan :: forall s
           .  (Int -> Int)
           -> SubSpace
           -> SubSpace
           -> PackTree s
           -> ST s (PackTree s)
modifySpan f left right pack =
   do mx <- go 0 0 (pTLeft pack) (pTRight pack)
      return pack
   where
   vector :: VM.STVector s Int
   vector = pTVector pack
   go :: Int -> Int -> SubSpace -> SubSpace -> ST s Int
   go depth i treeLeft treeRight =
       if depth < pTDepth pack
       then
         do let cut = (treeLeft + treeRight) / 2
            ml <- if left < cut
                  then go (depth + 1) (i * 2 + 1) treeLeft cut
                  else VM.read vector i
            mr <- if right > cut
                  then go (depth + 1) (i * 2 + 2) cut treeRight
                  else VM.read vector i
            let mx = max ml mr
            VM.write vector i mx
            return mx
       else
         do a <- f <$> VM.read vector i
            VM.write vector a i
            return a

addSpan :: Int
        -> SubSpace
        -> SubSpace
        -> PackTree s
        -> ST s (PackTree s)
addSpan count = modifySpan (+count)

subtractSpan :: Int
             -> SubSpace
             -> SubSpace
             -> PackTree s
             -> ST s (PackTree s)
subtractSpan count = modifySpan (subtract count)

getMax :: PackTree s -> ST s Int
getMax pack = VM.read (pTVector pack) 0
