module Graphics.Gudni.Base.HasDefault
  ( HasDefault(..)
  )
where

class HasDefault t where
    -- | Class for types with a default value.
    defaultValue :: t

instance HasDefault Int where
    defaultValue = 0

instance HasDefault (Maybe a) where
    defaultValue = Nothing
