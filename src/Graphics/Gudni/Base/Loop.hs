{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Base.Loop
  ( Loop(..)
  )
where

import Graphics.Gudni.Base.Chain
import Control.Applicative
import qualified Data.Vector as V

overLoopNeighbors' :: Chain f => (a -> a -> b) -> f a -> f b
overLoopNeighbors' f vector =
  let rotated = rest vector <|> pure (firstLink vector)
  in  zipWithChain f vector rotated

class Chain t => Loop t where
    -- | Zip each element with its next neighbor or the first element.
    overLoopNeighbors :: (a -> a -> b) -> t a -> t b

instance Loop V.Vector where
   overLoopNeighbors = overLoopNeighbors'

instance Loop [] where
   overLoopNeighbors = overLoopNeighbors'
