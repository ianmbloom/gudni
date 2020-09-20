module Graphics.Gudni.Figure.Primitive.ArcLength
  ( HasArcLength(..)
  )
where

import Graphics.Gudni.Figure.Primitive.Space

class HasSpace t => HasArcLength t where
  arcLength :: t -> SpaceOf t
