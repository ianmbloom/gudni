{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Scaffolding
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Top level functions for creating bounding box based layouts.
module Graphics.Gudni.Experimental.Scaffolding
  ( ScafName(..)
  , PointRef(..)
  , Scaf(..)
  , Scaffold(..)
  , buildScaffold
  , scaffoldToSTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.ShapeTree


import qualified Data.Map as M

type ScafName = String

data PointRef s where
  From      :: [ScafName] -> PointRef s
  Offset    :: Point2 s -> PointRef s

data Scaf m o t where
  Named     :: ScafName
            -> Scaffold m o t
            -> Scaf m o t
  Build     :: m (STree o t)
            -> Scaf m o t
  Build1    :: (Point2 (SpaceOf t) -> m (STree o t))
            -> PointRef (SpaceOf t)
            -> Scaf m o t
  Build2    :: (Point2 (SpaceOf t) -> Point2 (SpaceOf t) -> m (STree o t))
            -> PointRef (SpaceOf t)
            -> PointRef (SpaceOf t)
            -> Scaf m o t
  Build3    :: (Point2 (SpaceOf t) -> Point2 (SpaceOf t) -> Point2 (SpaceOf t) -> m (STree o t))
            -> PointRef (SpaceOf t)
            -> PointRef (SpaceOf t)
            -> PointRef (SpaceOf t)
            -> Scaf m o t

instance HasSpace t => HasSpace (Scaf m o t) where
  type SpaceOf (Scaf m o t) = SpaceOf t

type Scaffold m o t = STree o (Scaf m o t)

data ScafPoint s = ScafPoint (Point2 s) | Ambiguous

instance Space s => HasSpace (ScafPoint s) where
  type SpaceOf (ScafPoint s) = s

overScafPoint :: (Point2 s -> Point2 v) -> ScafPoint s -> ScafPoint v
overScafPoint f (ScafPoint p) = ScafPoint (f p)
overScafPoint f Ambiguous     = Ambiguous

instance Space s => SimpleTransformable (ScafPoint s) where
  type Simple (ScafPoint s) = ScafPoint s
  translateBy delta = overScafPoint (translateBy delta)
  scaleBy scale     = overScafPoint (scaleBy scale)
  stretchBy point   = overScafPoint (stretchBy point)

instance Space s => Transformable (ScafPoint s) where
  type Transformed (ScafPoint s) = ScafPoint s
  rotateBy rotate = overScafPoint (rotateBy rotate)

type NameMap s = M.Map [ScafName] (ScafPoint s)

scaffoldToSTree :: forall m o t
                .  (HasSpace t, Space (SpaceOf t), Monad m)
                => Scaffold m o t
                -> m (STree o t)
scaffoldToSTree scaf = buildScaffold mapping transformedScaf
  where
  (transformedScaf, mapping) = go scaf
  go :: Scaffold m o t -> (Scaffold m o t, NameMap (SpaceOf t))
  go scaffold =
    case scaffold of
       STransform transform tree ->
         let (innerTree, innerMap) = go tree
         in  (undefined {-applyTransformer transform innerTree-}, undefined {-M.map (applyTransformer transform) innerMap-})
       SMeld meld a b ->
         let (a', aMap) = go a
             (b', bMap) = go b
         in  (SMeld meld a' b', combineMaps aMap bMap)
       SLeaf scaf -> goScaf scaf
       SEmpty -> (SEmpty, M.empty)
  goScaf :: Scaf m o t -> (Scaffold m o t, NameMap (SpaceOf t))
  goScaf scaf =
      case scaf of
        Named  name child -> let (child', childMap) = go child
                             in  (child', M.insert [name] (ScafPoint zeroPoint) . M.mapKeys (name:) $ childMap)
        _      -> (SLeaf scaf, M.empty)


lookupPoint :: NameMap s
            -> PointRef s
            -> Point2 s
lookupPoint nameMap ref =
    case ref of
        From   name   -> let p = (M.!) nameMap name
                         in case p of
                               ScafPoint p -> p
                               Ambiguous -> error $ "ambiguous name lookup in scaffolding " ++ show name
        Offset p      -> p

buildScaf :: Monad m
          => NameMap (SpaceOf t)
          -> Scaf m o t
          -> m (STree o t)
buildScaf mapping scaf =
    case scaf of
      Build   f       -> f
      Build1  f x     -> let x' = lookupPoint mapping x
                         in  f x'
      Build2  f x y   -> let x' = lookupPoint mapping x
                             y' = lookupPoint mapping y
                         in  f x' y'
      Build3  f x y z -> let x' = lookupPoint mapping x
                             y' = lookupPoint mapping y
                             z' = lookupPoint mapping z
                         in  f x' y' z'
      Named   named scaf -> error "Named areas should be removed before buildScaf."

buildScaffold :: forall m o t . Monad m => NameMap (SpaceOf t) -> Scaffold m o t -> m (STree o t)
buildScaffold nameMap tree = go tree
  where
  go :: Scaffold m o t -> m (STree o t)
  go scaffold =
     case scaffold of
        STransform transform tree -> error "transforms should not be in traversed scaffold."
        SMeld meld a b ->
          do  a' <- go a
              b' <- go b
              return $ SMeld meld a' b'
        SLeaf scaf -> buildScaf nameMap scaf
        SEmpty -> return SEmpty

combineMaps :: NameMap s -> NameMap s -> NameMap s
combineMaps = M.unionWith (const (const Ambiguous))

mapPointRef :: (Point2 s -> Point2 v) -> PointRef s -> PointRef v
mapPointRef f ref =
    case ref of
        From name       -> From name
        Offset point    -> Offset (f point)

mapScaffoldPoints :: (Point2 (SpaceOf t) -> Point2 (SpaceOf t)) -> Scaffold m o t -> Scaffold m o t
mapScaffoldPoints f scaffold =
     case scaffold of
        STransform transform tree -> STransform transform (mapScaffoldPoints f tree)
        SMeld meld a b -> SMeld meld (mapScaffoldPoints f a) (mapScaffoldPoints f b)
        SLeaf scaf -> SLeaf $ mapScafPoints f scaf
        SEmpty -> SEmpty

mapScafPoints :: (Point2 (SpaceOf t) -> Point2 (SpaceOf t)) -> Scaf m o t -> Scaf m o t
mapScafPoints f scaf =
    case scaf of
        Named name child -> Named name $ mapScaffoldPoints f child
        Build  b         -> Build  b
        Build1 b x       -> Build1 b (mapPointRef f x)
        Build2 b x y     -> Build2 b (mapPointRef f x) (mapPointRef f y)

instance (HasSpace t) => SimpleTransformable (Scaf m o t) where
  translateBy delta   = mapScafPoints (translateBy delta  )
  scaleBy     scale   = mapScafPoints (scaleBy     scale  )
  stretchBy   boxSize = mapScafPoints (stretchBy   boxSize)

instance (Space (SpaceOf (Scaf m o t)), SimpleTransformable (Scaf m o t)) => Transformable (Scaf m o t) where
  rotateBy  angle = mapScafPoints (rotateBy angle)
