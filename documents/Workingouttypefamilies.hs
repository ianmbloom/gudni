{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative

-- For starters
class TreeType i where
    type Leaf i :: *

class TagTreeType i where
    type Tag  i :: *
    type Item i :: *

data STree i where
     STree :: STree i -> STree i -> STree i
     SLeaf :: Leaf i -> STree i
deriving instance (Show (Leaf i)) => Show (STree i)

data SBranch i where
     STag  :: Tag i -> STree i -> SBranch i
     SItem :: Item i -> SBranch i
deriving instance (Show (Leaf i),Show (Tag i), Show (Item i)) => Show (SBranch i)

data MaybeTree_ tag

instance TreeType (MaybeTree_ tag) where
  type Leaf (MaybeTree_ tag) = SBranch (MaybeTree_ tag)

instance TagTreeType (MaybeTree_ tag) where
  type Tag  (MaybeTree_ tag) = tag
  type Item (MaybeTree_ tag) = Maybe Int

data FullTree_ tag

instance TreeType (FullTree_ tag) where
  type Leaf (FullTree_ tag) = SBranch (FullTree_ tag)

instance TagTreeType (FullTree_ tag) where
  type Tag  (FullTree_ tag) = tag
  type Item (FullTree_ tag) = Int

data IdItem a

instance TreeType a  => TreeType (IdItem a) where
  type Leaf (IdItem a) = Leaf a

instance TagTreeType a => TagTreeType (IdItem a) where
  type Tag  (IdItem a) = Tag a
  type Item (IdItem a) = Item a

data MaybeItem a

instance (TreeType a)  => TreeType (MaybeItem a) where
  type Leaf (MaybeItem a) = Leaf a

instance TagTreeType a => TagTreeType (MaybeItem a) where
  type Tag  (MaybeItem a) = Tag a
  type Item (MaybeItem a) = Maybe (Item a)

eitherMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
eitherMaybe f a b = f <$> a <*> b <|> a <|> b

fullSTree :: forall a .
            (TreeType a
            ,TagTreeType a
            ,Leaf (MaybeItem a)~Maybe (SBranch (MaybeItem a))
            ,Leaf (IdItem a)~SBranch (IdItem a)
            ) =>
             STree (MaybeItem a)
          -> Maybe (STree (IdItem a))
fullSTree =
    go
    where
    go :: STree (MaybeItem a)
       -> Maybe (STree (IdItem a))
    go tree =
       case tree of
           SLeaf (SItem Nothing) -> Nothing
           SLeaf (SItem (Just rep)) -> Just . SLeaf . SItem $ rep
           SLeaf (STag tag tree) ->
              case go tree of
                  Just tree' -> Just . SLeaf . STag tag $ tree'
                  Nothing -> Nothing
           STree above below ->
              let a = go above
                  b = go below
              in  eitherMaybe STree a b


myTestTree :: STree (MaybeTree_ String)
myTestTree = STree (SLeaf (SItem (Just 1))) (STree (SLeaf (STag "left" (SLeaf (SItem (Just 2))))) (SLeaf (STag "right" (SLeaf (SItem Nothing) ))))

main = show . fullSTree $ myTestTree

{-
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative

-- For starters
class TreeType i where
    type Leaf i :: *

data STree i where
     STree :: STree i -> STree i -> STree i
     SLeaf :: Leaf i -> STree i
deriving instance (Show (Leaf i)) => Show (STree i)

data MaybeIntTree_

instance TreeType (MaybeIntTree_) where
  type Leaf (MaybeIntTree_) = Maybe Int

data IntTree_

instance TreeType (IntTree_) where
  type Leaf (IntTree_) = Int

data IdLeaf a

instance TreeType a  => TreeType (IdLeaf a) where
  type Leaf (IdLeaf a) = Leaf a

data MaybeLeaf a

instance (TreeType a)  => TreeType (MaybeLeaf a) where
  type Leaf (MaybeLeaf a) = Maybe (Leaf a)

eitherMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
eitherMaybe f a b = f <$> a <*> b <|> a <|> b

fullSTree :: forall a .
            (TreeType a
            ) =>
             STree (MaybeLeaf a)
          -> Maybe (STree (IdLeaf a))
fullSTree =
    go
    where
    go :: STree (MaybeLeaf a)
       -> Maybe (STree (IdLeaf a))
    go tree =
       case tree of
           SLeaf Nothing -> Nothing
           SLeaf (Just rep) -> Just . SLeaf $ rep
           STree above below ->
              let a = go above
                  b = go below
              in  eitherMaybe STree a b


myTestTree :: STree (MaybeIntTree_)
myTestTree = STree (SLeaf (Just 1)) (STree (SLeaf (Just 2)) (SLeaf Nothing))

main = putStrLn . show . fullSTree $ myTestTree-}
