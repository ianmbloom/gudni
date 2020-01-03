
module Graphics.Gudni.Util.Chain
  ( Chain(..)
  )
where

import Control.Monad
import Control.Applicative
import qualified Data.Vector as V

class (Alternative t, Monad t, Functor t) => Chain t where
  first   :: t a -> a
  rest    :: t a -> t a
  last    :: t a -> a
  notLast :: t a -> t a
  reverse :: t a -> t a
  halfSplit :: t a -> (t a, t a)
  segregate :: (a -> Bool) -> t a -> (t a, t a)

instance Chain V.Vector where
  first   = V.head
  rest    = V.tail
  last    = V.last
  notLast vector = V.take (V.length vector - 1) vector
  reverse = V.reverse
  halfSplit vector = if (V.null vector)
                     then (V.empty, V.empty)
                     else let half = V.length vector `div` 2
                          in (V.take half vector, V.drop half vector)
  segregate = V.span

instance Chain [] where
  first   = head
  rest    = tail
  last    = Prelude.last
  notLast list = take (length list - 1) list
  reverse = Prelude.reverse
  halfSplit list = if (null list)
                     then (empty, empty)
                     else let half = length list `div` 2
                          in (take half list, drop half list)
  segregate = span
