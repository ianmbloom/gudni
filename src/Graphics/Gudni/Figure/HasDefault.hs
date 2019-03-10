module Graphics.Gudni.Figure.HasDefault
  ( HasDefault(..)
  )
where

class HasDefault t where
    -- | Class for types with a default value.
    defaultValue :: t
