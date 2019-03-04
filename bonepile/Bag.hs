module Graphics.Gudni.Util.Bag
  ( Bag(..)
  , emptyBag
  , addToBag
  , getFromBag
  )
where

import qualified Data.IntMap.Strict as M
import Data.Maybe
import Control.DeepSeq

data Bag i t = Bag
  { bagKey       :: !i
  , bagContainer :: M.IntMap t
  } deriving (Show)

emptyBag :: (Num i) => Bag i t
emptyBag = Bag 0 M.empty

addToBag :: (Integral i, Num i) => Bag i t -> t -> (Bag i t, i)
addToBag (Bag i bag) item = (Bag (i+1) (M.insert (fromIntegral i) item bag), i)

getFromBag :: (Integral i) => Bag i t -> i -> t
getFromBag (Bag _ bag) i = fromJust $ M.lookup (fromIntegral i) bag

instance (NFData i, NFData t) => NFData (Bag i t) where
  rnf (Bag k c) = k `deepseq` c `deepseq` ()
