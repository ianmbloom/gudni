module Indexable
  ( Indexable
  )
where

class Indexable t where
  indexRange :: t -> (Index, Index)
  indexOptic :: t -> Index -> Lens' t t
