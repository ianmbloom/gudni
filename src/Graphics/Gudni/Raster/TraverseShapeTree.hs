{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.TraverseShapeTree
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for traversing a ShapeTree while accumulating information from the
-- transform and meld nodes along the way.

module Graphics.Gudni.Raster.TraverseShapeTree
  ( traverseSTree
  , traverseSBranch
  , traverseCompoundTree
  , traverseCompound
  , traverseShapeTree
  , traverseOverlap
  , mapSTree
  , mapMSTree
  , fullSTree
  , collapseTrans
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Cut
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Util

import Control.Lens
import qualified Data.Sequence as S
import Control.Monad.State
import Data.Foldable
import Control.Lens
import Data.Maybe


-- | Operate on a compound junction.
traverseCompound :: Compound -> Compound -> (Compound, Compound)
traverseCompound CompoundAdd      current = (current, current)
traverseCompound CompoundSubtract current = (invertCompound current, current)

-- | Operate on an overlay junction.
traverseOverlap :: Overlap -> Overlap -> (Overlap, Overlap)
traverseOverlap Overlap Overlap = (Overlap,Overlap)

-- | Traverse across an STree monadically collecting metadata from
traverseSTree :: forall i m value
              .  ( Monad m)
                 -- traverseMeld
              => (Meld i -> Meld i -> (Meld i, Meld i))
                 -- meldValues
              -> (Meld i -> value -> value -> value)
                 -- onLeaf
              -> ( (Meld i -> Tag i -> STree i -> m value)
                  -> Meld i -> Tag i -> Leaf i -> m value
                 )
                 -- parentMeld
              -> Meld i
                 -- tree
              -> Tag i
              -> STree i
              -> m value
traverseSTree traverseMeld
              meldValues
              onLeaf =
    go
    where
    go :: Meld i
       -> Tag i
       -> STree i
       -> m value
    go parentMeld parentTag tree =
       case tree of
           SMeld overlap above below ->
              do let (aMeld, bMeld) = traverseMeld overlap parentMeld
                 aR <- go aMeld parentTag above
                 bR <- go bMeld parentTag below
                 return $ meldValues overlap aR bR
           SLeaf rep -> onLeaf go parentMeld parentTag rep

traverseSBranch :: forall i m value .
                      ( Leaf i ~ SBranch i
                      , Monad m
                      )
                      -- postTagOp
                   => ( Tag i -> m value -> m value )
                      -- preTagOp
                   -> ( Tag i -> Tag i -> Tag i )
                      -- onItem
                   -> ( Meld i -> Tag i -> Item i -> m value )
                      -- onTree
                   -> ( Meld i -> Tag i -> STree i -> m value)
                      -- parentMeld
                   -> Meld i
                      -- parentTag
                   -> Tag i
                      -- rep
                   -> Leaf i
                   -> m value
traverseSBranch postTagOp
                preTagOp
                onItem
                onTree =
    go
    where
    go :: Meld i
       -> Tag i
       -> Leaf i
       -> m value
    go parentMeld parentTag rep =
       case rep of
            SBranch currentTag child ->
                let tag = preTagOp currentTag parentTag
                in  postTagOp currentTag $ onTree parentMeld tag child
            SItem item ->
                onItem parentMeld parentTag item

noPostTrans = const id

-- | Traverse a compound shape tree
                        -- meldValues
traverseCompoundTree :: ( Monad m
                        , Space s
                        , i~CompoundTree_ s)
                     => ( Meld i -> value -> value -> value)
                        -- onItem
                     -> ( Meld i -> Tag i -> Item i -> m value )
                     -> Meld i
                     -> STree i
                     -> m value
traverseCompoundTree meldValues onItem parentTrans tree =
  let trTree   = traverseSTree traverseCompound meldValues
      trBranch = traverseSBranch noPostTrans CombineTransform onItem
  in  trTree trBranch parentTrans defaultValue tree

-- | Traverse an overlap shape tree
traverseShapeTree :: forall m s i token textureLabel value .
                     ( Monad m
                     , Space s
                     , i~ShapeTree_ token textureLabel s)
                  => (Meld i -> value -> value -> value)
                  -> (Meld i -> Tag i -> Item i -> m value)
                  -> STree i
                  -> m value
traverseShapeTree meldValues onItem tree =
  let trTree   = traverseSTree traverseOverlap meldValues
      trBranch = traverseSBranch noPostTrans CombineTransform onItem
  in  trTree trBranch (defaultValue :: Meld i) IdentityTransform tree

keepMeld a b = (b, b)

mapSTree :: (Meld a ~ Meld b
            )
         => (Leaf a -> Leaf b)
         -> STree a
         -> STree b
mapSTree f tree = go  tree
    where
    go tree =
       case tree of
           SLeaf rep -> SLeaf (f rep)
           SMeld overlap above below ->
              let a = go above
                  b = go below
              in  SMeld overlap a b

mapSItem :: (Meld a ~ Meld b
            ,Tag a ~ Tag b
            ,Leaf a ~ SBranch a
            ,Leaf b ~ SBranch b
            )
         => (Item a -> Item b)
         -> STree a
         -> STree b
mapSItem f =
  go
  where
    go tree =
       case tree of
           SLeaf (SItem rep) -> SLeaf . SItem . f $ rep
           SLeaf (SBranch tag child) -> SLeaf . SBranch tag . go $ child
           SMeld overlap above below ->
              let a = go above
                  b = go below
              in  SMeld overlap a b

mapMSTree :: ( HasDefault (Meld a)
             , Meld a ~ Meld b
             , Monad m
             )
          => (Leaf a -> m (Leaf b))
          -> STree a
          -> m (STree b)
mapMSTree f =
    go
    where
    go tree =
       case tree of
           SLeaf rep -> SLeaf <$> f rep
           SMeld overlap above below ->
              do a <- go above
                 b <- go below
                 return $ SMeld overlap a b

data IdItem a

instance TreeType a  => TreeType (IdItem a) where
  type Meld (IdItem a) = Meld a
  type Leaf (IdItem a) = Leaf a

instance TagTreeType a => TagTreeType (IdItem a) where
  type Tag  (IdItem a) = Tag a
  type Item (IdItem a) = Item a

data MaybeItem a

instance (TreeType a)  => TreeType (MaybeItem a) where
  type Meld (MaybeItem a) = Meld a
  type Leaf (MaybeItem a) = Leaf a

instance TagTreeType a => TagTreeType (MaybeItem a) where
  type Tag  (MaybeItem a) = Tag a
  type Item (MaybeItem a) = Maybe (Item a)


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
           --SLeaf (SItem Nothing) -> Nothing
           SLeaf (SItem (Just rep)) -> Just . SLeaf . SItem $ rep
           --SLeaf (SBranch tag tree) ->
           --   case go tree of
           --       Just tree' -> Just . SLeaf . SBranch tag $ tree'
           --       Nothing -> Nothing
           --SMeld overlap above below ->
           --   let a = go above
           --       b = go below
           --   in  eitherMaybe (SMeld overlap) a b

{-
data FullCompoundTree_ s

instance (Space s) => TreeType (FullCompoundTree_ s) where
  type Meld (FullCompoundTree_ s) = Compound
  type Leaf (FullCompoundTree_ s) = SBranch (FullCompoundTree_ s)

instance (Space s) => TagTreeType (FullCompoundTree_ s) where
  type Tag  (FullCompoundTree_ s) = Transformer s
  type Item (FullCompoundTree_ s) = Shape s

type FullCompoundTree s = STree (FullCompoundTree_ s)

data FullShapeTree_ token textureLabel s

instance (Space s) => TreeType (FullShapeTree_ token textureLabel s) where
    type Meld (FullShapeTree_ token textureLabel s) = Overlap
    type Leaf (FullShapeTree_ token textureLabel s) = SBranch (FullShapeTree_ token textureLabel s)

instance (Space s) => TagTreeType (FullShapeTree_ token textureLabel s) where
    type Tag  (FullShapeTree_ token textureLabel s) = Transformer s
    type Item (FullShapeTree_ token textureLabel s) = SRep token textureLabel (FullCompoundTree s)

type FullShapeTree token textureLabel s = STree (FullShapeTree_ token textureLabel s)

fromSRep :: SRep a b (Maybe c) -> Maybe (SRep a b c)
fromSRep (SRep a b mC) = fmap (SRep a b) mC

fullCompoundTree :: CompoundTree s -> Maybe (FullCompoundTree s)
fullCompoundTree = fullSTree

fullSRep :: SRep a b (CompoundTree s) -> SRep a b (Maybe (FullCompoundTree s))
fullSRep (SRep a b tree) = SRep a b (fullCompoundTree tree)

mergeSRep :: Maybe (SRep a b (CompoundTree s)) -> Maybe (SRep a b (FullCompoundTree s))
mergeSRep = join . fmap (fromSRep . fullSRep)

fullMerge :: ( Leaf a~SBranch a
             ) => STree (MaybeItem a) -> STree a
fullMerge = mapSItem mergeSRep

fullShapeTree :: forall a b token tex s g
              .  ( Leaf a~SBranch a
                 , Leaf b~SBranch b
                 , Item a~Maybe (SRep token tex (CompoundTree s))
                 , Tag a~Tag b
                 , Meld a~Meld b
                 )
              => STree a
              -> Maybe (STree b)
fullShapeTree tree = fullSTree . fullMerge $ tree

-}
collapseTrans :: forall a b
              . ( CanProject (BezierSpace (SpaceOf (Item a))) (Item a)
                , Transformable (Item a)
                , Leaf a~SBranch b
                , Item a~Leaf b
                , Meld a~Meld b
                )
              => STree a
              -> STree b
collapseTrans tree =
  go IdentityTransform tree
  where
  -- go :: Transformer (SpaceOf (Item a)) -> STree a -> STree b
  go parentTrans tree =
    case tree of
      SMeld meld a b -> SMeld meld (go parentTrans a) (go parentTrans b)
      --SLeaf (SBranch trans tree) -> go (CombineTransform parentTrans trans) tree
      --SLeaf (SItem item) -> SLeaf $ (applyTransformer parentTrans item :: Leaf b)
