module Graphics.Gudni.Figure.Principle.ArcLength
  ( HasArcLength(..)
  )
where

import Graphics.Gudni.Figure.Principle.Space

class HasSpace t => HasArcLength t where
  arcLength :: t -> SpaceOf t
