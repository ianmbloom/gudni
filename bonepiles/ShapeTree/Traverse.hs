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

module Graphics.Gudni.ShapeTree.Traverse
  ( traverseTree
  , traverseSTree
  , traverseSBranch
  , noPostTrans
  , keepMeld
  , traverseCompoundTree
  , traverseCompound
  , traverseShapeTree
  , mapSLeaf
  , mapMSLeaf
  , mapSItem
  , mapMSItem
  , mapBranchMeld
  , mapMeld
  , fullSTree
  , joinLeaves
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Lens
import qualified Data.Sequence as S
import Control.Monad.State
import Data.Foldable
import Control.Lens
import Control.Monad.Identity
import Data.Maybe


-- | Operate on a compound junction.
traverseCompound :: Compound -> Compound -> (Compound, Compound)
traverseCompound CompoundAdd      current = (current, current)
traverseCompound CompoundSubtract current = (invertCompound current, current)

traverseTree :: forall i m value
             .  (Monad m)
             => (Meld i -> Meld i -> (Meld i, Meld i))
             -> (Meld i -> value -> value -> value)
             -> (Meld i -> Leaf i -> m value)
             -> Meld i
             -> STree i
             -> m value
traverseTree traverseMeld
             meldValues
             onLeaf =
    go
    where
    go parentMeld tree =
      case tree of
           SMeld meld above below ->
              do let (aMeld, bMeld) = traverseMeld meld parentMeld
                 aR <- go aMeld above
                 bR <- go bMeld below
                 return $ meldValues meld aR bR
           SLeaf rep -> onLeaf parentMeld rep


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
                   => ( Tag i -> value -> value )
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
                in  postTagOp currentTag <$> onTree parentMeld tag child
            SItem item ->
                onItem parentMeld parentTag item

noPostTrans = const id
keepMeld _ b = (b, b)


-- | Traverse a compound shape tree
                        -- meldValues
traverseCompoundTree :: forall  m i value
                     .  ( Monad m
                        , Meld i ~ Compound
                        , Tag  i ~ Transformer (SpaceOf (Item i))
                        , Leaf i ~ SBranch i
                        )
                     => ( Meld i -> value -> value -> value)
                        -- onItem
                     -> ( Meld i -> Tag i -> Item i -> m value )
                     -> Tag i
                     -> STree i
                     -> m value
traverseCompoundTree meldValues onItem parentTrans tree =
  let trTree   = traverseSTree traverseCompound meldValues
      trBranch = traverseSBranch noPostTrans CombineTransform onItem
  in  trTree trBranch defaultValue parentTrans tree

-- | Traverse an overlap shape tree
traverseShapeTree :: forall  m i value
                  .  ( Monad m
                     , Meld i ~ Overlap
                     , Tag  i ~ Transformer (SpaceOf (Item i))
                     , Leaf i ~ SBranch i
                     )
                  => (Meld i -> value -> value -> value)
                  -> (Meld i -> Tag i -> Item i -> m value)
                  -> STree i
                  -> m value
traverseShapeTree meldValues onItem tree =
  let trTree   = traverseSTree keepMeld meldValues
      trBranch = traverseSBranch noPostTrans CombineTransform onItem
  in  trTree trBranch (defaultValue :: Meld i) (Simple IdentityTransform) tree

mapSLeaf :: (Meld a ~ Meld b
            )
         => (Leaf a -> Leaf b)
         -> STree a
         -> STree b
mapSLeaf f tree = go  tree
    where
    go tree =
       case tree of
           SLeaf rep -> SLeaf (f rep)
           SMeld overlap above below ->
              let a = go above
                  b = go below
              in  SMeld overlap a b

mapSItem :: ( Meld a ~ Meld b
            , Tag a ~ Tag b
            , Leaf a ~ SBranch a
            , Leaf b ~ SBranch b
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

mapMSItem :: ( Monad m
             , Meld a ~ Meld b
             , Tag a ~ Tag b
             , Leaf a ~ SBranch a
             , Leaf b ~ SBranch b
             )
         => (Item a -> m (Item b))
         -> STree a
         -> m (STree b)
mapMSItem f =
  go
  where
    go tree =
       case tree of
           SLeaf (SItem rep) -> SLeaf . SItem <$> f rep
           SLeaf (SBranch tag child) -> SLeaf . SBranch tag <$> go child
           SMeld overlap above below ->
              do a <- go above
                 b <- go below
                 return $ SMeld overlap a b


mapMSLeaf :: ( HasDefault (Meld a)
             , Meld a ~ Meld b
             , Monad m
             )
          => (Leaf a -> m (Leaf b))
          -> STree a
          -> m (STree b)
mapMSLeaf f =
    go
    where
    go tree =
       case tree of
           SLeaf rep -> SLeaf <$> f rep
           SMeld overlap above below ->
              do a <- go above
                 b <- go below
                 return $ SMeld overlap a b

mapBranchMeld :: (a -> b)
              -> BranchTree a tag item
              -> BranchTree b tag item
mapBranchMeld f =
  go
  where
  go tree =
    case tree of
        SLeaf (SBranch tag child) -> SLeaf . SBranch tag $ go child
        SLeaf (SItem item) -> SLeaf . SItem $ item
        SMeld meld a b -> SMeld (f meld) (go a) (go b)

mapMeld :: (a -> b) -> Tree a item -> Tree b item
mapMeld f =
  go
  where
  go tree =
    case tree of
        SLeaf leaf -> SLeaf leaf
        SMeld meld a b -> SMeld (f meld) (go a) (go b)


fullSTree :: forall meld item .
             TransTree meld (Maybe item)
          -> Maybe (TransTree meld item)
fullSTree =
    go
    where
    go tree =
       case tree of
           SLeaf (SItem Nothing) -> Nothing
           SLeaf (SItem (Just rep)) -> Just . SLeaf . SItem $ rep
           SLeaf (SBranch tag tree) ->
              case go tree of
                  Just tree' -> Just . SLeaf . SBranch tag $ tree'
                  Nothing -> Nothing
           SMeld overlap above below ->
              let a = go above
                  b = go below
              in  eitherMaybe (SMeld overlap) a b

joinLeaves :: (Tree meld (Tree meld a)) -> Tree meld a
joinLeaves (SMeld meld a b) = SMeld meld (joinLeaves a) (joinLeaves b)
joinLeaves (SLeaf a) = a
