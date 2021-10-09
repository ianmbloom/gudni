{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Base.Ring
  ( Ring(..)
  )
where

import Graphics.Gudni.Base.Chain
import Control.Applicative
import qualified Data.Vector as V

overLoopNeighbors' :: Chain f => (a -> a -> b) -> f a -> f b
overLoopNeighbors' f vector =
  let rotated = rest vector <|> pure (firstLink vector)
  in  zipWithChain f vector rotated

class Chain t => Ring t where
    -- | Zip each element with its next neighbor or the first element.
    overLoopNeighbors :: (a -> a -> b) -> t a -> t b

instance Ring V.Vector where
   overLoopNeighbors = overLoopNeighbors'

instance Ring [] where
   overLoopNeighbors = overLoopNeighbors'
