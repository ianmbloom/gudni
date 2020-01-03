{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}

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
  ( traverseTree
  , traverseCompoundTree
  , traverseShapeTree
  , mapSTree
  , mapMSTree
  )
where

import Graphics.Gudni.Figure

import Control.Lens

-- | Operate on a compound junction.
traverseCompound :: Compound -> Compound -> (Compound, Compound)
traverseCompound CompoundAdd      current = (current, current)
traverseCompound CompoundSubtract current = (invertCompound current, current)

-- | Operate on an overlay junction.
traverseUnit :: () -> () -> ((), ())
traverseUnit () () = ((),())

-- | Traverse across an STree monadically collecting metadata from
traverseTree :: (Monad m, HasDefault o)
             => (o -> o -> (o, o))
             -> (  Transformer (SpaceOf rep)
                -> Transformer (SpaceOf rep)
                -> Transformer (SpaceOf rep)
                )
             -> o
             -> Transformer (SpaceOf rep)
             -> (o -> Transformer (SpaceOf rep) -> rep -> m ())
             -> STree o rep
             -> m ()
traverseTree combineOp transformOp c t f tree = go c t tree
    where go c t tree =
              case tree of
                  SLeaf rep -> f c t rep
                  SMeld overlap above below ->
                     do let (a, b) = combineOp overlap c
                        go a t above
                        go b t below
                  STransform tOp child -> go c (transformOp tOp t) child
                  SEmpty -> return ()

-- | Traverse a compound shape tree
traverseCompoundTree :: (Num (SpaceOf rep), Monad m)
                     => Compound
                     -> Transformer (SpaceOf rep)
                     -> (Compound -> Transformer (SpaceOf rep) -> rep -> m ())
                     -> STree Compound rep
                     -> m ()
traverseCompoundTree o t = traverseTree traverseCompound CombineTransform o t

-- | Traverse an overlap shape tree
traverseShapeTree :: (Num (SpaceOf rep), Monad m)
                  => (() -> Transformer (SpaceOf rep) -> rep -> m ())
                  -> STree () rep
                  -> m ()
traverseShapeTree = traverseTree traverseUnit CombineTransform () IdentityTransform

mapSTree :: (SpaceOf a ~ SpaceOf b)
         => (a -> b)
         -> STree o a
         -> STree o b
mapSTree f tree = go  tree
    where
    go tree =
       case tree of
           SLeaf rep -> SLeaf (f rep)
           SMeld overlap above below ->
              let a = go above
                  b = go below
              in  SMeld overlap a b
           STransform tOp child -> STransform tOp (go child)
           SEmpty -> SEmpty

mapMSTree :: (SpaceOf a ~ SpaceOf b, Monad m)
             => (a -> m b)
             -> STree o a
             -> m (STree o b)
mapMSTree f tree = go  tree
    where
    go tree =
       case tree of
           SLeaf rep -> SLeaf <$> f rep
           SMeld overlap above below ->
              do a <- go above
                 b <- go below
                 return $ SMeld overlap a b
           STransform tOp child -> STransform tOp <$> go child
           SEmpty -> return SEmpty
