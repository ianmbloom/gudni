{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Gudni.Base.Chain
  ( HasElements(..)
  , Chain(..)
  )
where

import Graphics.Gudni.Base.Reversible
import Data.Kind
import Data.Functor.Classes
import Control.Monad
import Control.Applicative
import qualified Data.Vector as V
import qualified Data.List as L

class HasElements t where
  type ElemOf t :: Type

instance HasElements (V.Vector a) where
  type ElemOf (V.Vector a) = a

instance HasElements [a] where
  type ElemOf [a] = a


class (Alternative t, Monad t, Functor t, Foldable t, Eq1 t) => Chain t where
  firstLink:: t a -> a
  rest     :: t a -> t a
  lastLink :: t a -> a
  notLast  :: t a -> t a
  reverseChain  :: t a -> t a
  halfSplit :: t a -> (t a, t a)
  segregate :: (a -> Bool) -> t a -> (t a, t a)
  zipWithChain :: (a -> a -> b) -> t a -> t a -> t b
  scanlChain :: (b -> a -> b) -> b -> t a -> t b
  overChainNeighbors :: (a -> a -> a) -> t a -> t a
  overChainNeighbors f chain = zipWithChain f chain (rest chain) <|> pure (lastLink chain)
  chainFromList :: [a] -> t a
  concatChains :: [t a] -> t a

instance Chain V.Vector where
  firstLink = V.head
  rest      = V.tail
  lastLink  = V.last
  notLast vector = V.take (V.length vector - 1) vector
  reverseChain = V.reverse
  halfSplit vector = if (V.null vector)
                     then (V.empty, V.empty)
                     else let len = V.length vector
                              half = len - (len `div` 2)
                          in (V.take half vector, V.drop half vector)
  segregate = V.partition
  zipWithChain = V.zipWith
  scanlChain = V.scanl
  chainFromList = V.fromList
  concatChains = V.concat

instance (Reversible t) => Reversible (V.Vector t) where
  reverseItem = reverseChain . fmap reverseItem


instance Chain [] where
  firstLink = L.head
  rest      = L.tail
  lastLink  = L.last
  notLast list = L.take (length list - 1) list
  reverseChain = L.reverse
  halfSplit list = if (null list)
                     then (empty, empty)
                     else let half = L.length list `div` 2
                          in (L.take half list, L.drop half list)
  segregate = L.partition
  zipWithChain = L.zipWith
  scanlChain = L.scanl
  chainFromList = id
  concatChains = L.concat

instance (Reversible t) => Reversible [t] where
  reverseItem = reverseChain . fmap reverseItem
