module Graphics.Gudni.Figure.ArcLength
  ( HasArcLength(..)
  )
where

import Graphics.Gudni.Figure.Space

class HasSpace t => HasArcLength t where
  arcLength :: t -> SpaceOf t
