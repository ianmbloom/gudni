
module Graphics.Gudni.Figure.Reversible
  ( Reversible(..)
  )
where

import qualified Data.Vector as V

class Reversible t where
      reverseItem :: t -> t
