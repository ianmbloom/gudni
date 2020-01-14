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
  , flattenShapeTree
  , flattenCompoundTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Debug

import Control.Lens
import qualified Data.Sequence as S
import Control.Monad.State
import Data.Foldable
import Control.Lens


-- | Operate on a compound junction.
traverseCompound :: Compound -> Compound -> (Compound, Compound)
traverseCompound CompoundAdd      current = (current, current)
traverseCompound CompoundSubtract current = (invertCompound current, current)

-- | Operate on an overlay junction.
traverseOverlap :: Overlap -> Overlap -> (Overlap, Overlap)
traverseOverlap Overlap Overlap = (Overlap,Overlap)

-- | Traverse across an STree monadically collecting metadata from
traverseTree :: (Monad m, HasDefault o, Show o, Show rep, Space (SpaceOf rep))
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
traverseCompoundTree :: (Space (SpaceOf rep), Show rep, Monad m)
                     => Compound
                     -> Transformer (SpaceOf rep)
                     -> (Compound -> Transformer (SpaceOf rep) -> rep -> m ())
                     -> STree Compound rep
                     -> m ()
traverseCompoundTree o t = traverseTree traverseCompound CombineTransform o t

-- | Traverse an overlap shape tree
traverseShapeTree :: (Space (SpaceOf rep), Show rep, Monad m)
                  => (Overlap -> Transformer (SpaceOf rep) -> rep -> m ())
                  -> STree Overlap rep
                  -> m ()
traverseShapeTree = traverseTree traverseOverlap CombineTransform Overlap IdentityTransform

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

flattenShapeTree :: (Space s, Show token) => ShapeTree token s
                 -> S.Seq (Shape s)
flattenShapeTree tree =
    execState (traverseShapeTree flattenSubstance tree) S.empty

flattenCompoundTree :: Space s
                    => Transformer s
                    -> STree Compound (Shape s)
                    -> State (S.Seq (Shape s)) ()
flattenCompoundTree transformer =
    traverseCompoundTree defaultValue transformer flattenShape

flattenSubstance :: Space s => Overlap -> Transformer s -> SRep token sub (STree Compound (Shape s)) -> State (S.Seq (Shape s))  ()
flattenSubstance Overlap transformer (SRep token substance subTree) =
    flattenCompoundTree transformer subTree

flattenShape :: (Space s, Show s) => Compound -> Transformer s -> Shape s -> State (S.Seq (Shape s)) ()
flattenShape combineType transformer shape =
    let shape' = Shape . map (applyTransformer transformer) $ view shapeOutlines shape
    in  modify (flip (S.|>) shape')
