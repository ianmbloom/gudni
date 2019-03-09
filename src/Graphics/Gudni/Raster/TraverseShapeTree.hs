{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Graphics.Gudni.Raster.TraverseShapeTree
  ( traverseTree
  , traverseCompoundTree
  , traverseShapeTree
  , transformOutlineTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Types

import Control.Lens

transformShapeTree :: forall t s o leaf . (Transformable t s)
                   => ((t s -> t s) -> leaf -> leaf)
                   -> STree o (TransformType s) leaf
                   -> STree o (TransformType s) leaf
transformShapeTree f tree =
  case tree of
      SLeaf rep -> SLeaf rep
      STransform t child ->
          fmap (f (applyTransformType t)) $ transformShapeTree f child
      SOverlap overlap above below ->
          let above' = transformShapeTree f above
              below' = transformShapeTree f below
          in  SOverlap overlap above' below'

transformOutlineTree :: (Floating s, Eq s)
                     => STree o (TransformType s) (SRep token substance (STree o1 (TransformType s) (RawShape_ s)))
                     -> STree o (TransformType s) (SRep token substance (STree o1 (TransformType s) (Outlines s)))
transformOutlineTree shapeTree =
    let --outlineTree :: STree () (TransformType DisplaySpace) (SRep SubstanceId PictId (STree CombineType (TransformType DisplaySpace) Outlines))
        outlineTree = fmap (over shapeCompoundTree (fmap (Group . rawShapeToOutlines))) shapeTree
        --transformedTree :: STree () (TransformType DisplaySpace) (SRep SubstanceId PictId (STree CombineType (TransformType DisplaySpace) Outlines))
        transformedTree = transformShapeTree (fmap . fmap . fmap) . (fmap . fmap $ transformShapeTree fmap) $ outlineTree
    in  transformedTree

traverseCombine :: CombineType -> CombineType -> (CombineType, CombineType)
traverseCombine CombineAdd      current = (current, current)
traverseCombine CombineSubtract current = (current, invertCombineType current)
traverseCombine CombineContinue current = (current, current)

traverseUnit :: () -> () -> ((), ())
traverseUnit () () = ((),())

traverseTree :: (Monad m, HasDefault o, HasDefault t)
             => (o -> o -> (o, o))
             -> (t -> t -> t)
             -> o
             -> t
             -> (o -> t -> rep -> m ())
             -> STree o t rep
             -> m ()
traverseTree combineOp transformOp c t f tree = go c t tree
    where go c t tree =
              case tree of
                  SLeaf rep -> f c t rep
                  SOverlap overlap above below ->
                     do let (a, b) = combineOp overlap c
                        go a t above
                        go b t below
                  STransform tOp child -> go c (transformOp tOp t) child

traverseCompoundTree :: (Num s, Monad m)
                     => CombineType
                     -> TransformType s
                     -> (CombineType -> TransformType s -> rep -> m ())
                     -> STree CombineType (TransformType s) rep
                     -> m ()
traverseCompoundTree o t = traverseTree traverseCombine CombineTransform o t

traverseShapeTree :: (Num s, Monad m)
                  => (() -> TransformType s -> rep -> m ())
                  -> STree () (TransformType s) rep
                  -> m ()
traverseShapeTree = traverseTree traverseUnit CombineTransform () identityTransform
