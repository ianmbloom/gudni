module Graphics.Gudni.Figure.HasDefault
  ( HasDefault(..)
  )
where

class HasDefault t where
    defaultValue :: t
