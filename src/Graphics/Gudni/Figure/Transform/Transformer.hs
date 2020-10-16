{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Transformer
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions and classes for linear transformation of objects.

module Graphics.Gudni.Figure.Transform.Transformer
 ( Transformer (..)
 , SimpleTransformer(..)
 , CanInvert(..)
 , CanApplySimpleTransformer(..)
 , CanApplyTransformer (..)
 , applyTranslation
 , applyStretch
 , applyScale
 , applyRotation
 , translateBox
 , stretchBox
)
where

import Graphics.Gudni.Base.HasDefault
import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Transform.Projection
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Figure.Fit.Bezier

import Graphics.Gudni.Base.Chain
import Graphics.Gudni.Util.Debug

import Control.Monad.Random
import Control.Applicative
import Control.Lens

import Data.Traversable
import Data.Hashable
import Data.Either

import System.Random

instance HasDefault (SimpleTransformer s) where
    defaultValue = IdentityTransform
instance HasDefault (Transformer s) where
    defaultValue = Simple defaultValue

class CanInvert a where
  invert :: a -> a

-- | Transformations that can be applied to a bounding box as well.
data SimpleTransformer s where
  IdentityTransform :: SimpleTransformer s
  Translate :: Point2 s -> SimpleTransformer s
  Stretch   :: Point2 s -> SimpleTransformer s
  CombineSimple :: SimpleTransformer s -> SimpleTransformer s -> SimpleTransformer s
  deriving (Show)

invertScale :: Space s => s -> s
invertScale s = if s == 0 then 0 else 1/s

instance Space s => CanInvert (SimpleTransformer s) where
  invert t =
    case t of
      IdentityTransform -> IdentityTransform
      Translate p -> Translate (negate p)
      Stretch s -> Stretch (fmap invertScale s)
      CombineSimple a b -> CombineSimple (invert b) (invert a)

-- | Transformations that break bounding boxes.
data Transformer s where
  Simple  :: SimpleTransformer s -> Transformer s
  Rotate  :: Angle s  -> Transformer s
  Project :: OpenCurve s -> Transformer s
  CombineTransform :: Transformer s -> Transformer s -> Transformer s
  deriving (Show)

instance (Num (Angle s), Space s) => CanInvert (Transformer s) where
  invert t =
    case t of
      Simple st -> Simple (invert st)
      Rotate a -> Rotate (negate a)
      Project curve -> error "invert projection not implemented."
      CombineTransform a b -> CombineTransform (invert b) (invert a)

instance (Space s) => HasSpace (Transformer s) where
  type SpaceOf (Transformer s) = s

class CanApplyTransformer a where
  applyTransformer :: Transformer (SpaceOf a) -> a -> a

class CanApplySimpleTransformer a where
  applySimpleTransformer :: SimpleTransformer (SpaceOf a) -> a -> a

instance (Space s, Chain f) => CanApplySimpleTransformer (Shape_ f s) where
  applySimpleTransformer f = over shapeOutlines (fmap (execSimpleTransformer f))

instance (Space s) => CanApplySimpleTransformer (Box s) where
  applySimpleTransformer trans =
    case trans of
       IdentityTransform -> id
       Translate p -> translateBox p
       Stretch s -> stretchBox s
       CombineSimple a b -> applySimpleTransformer b . applySimpleTransformer a

instance (Space s, Chain f) => CanApplyTransformer (Shape_ f s) where
  applyTransformer trans =
    case trans of
        Simple simple -> execSimpleTransformer simple
        Rotate a      -> applyRotation a
        Project path  -> projectDefault False . makeBezierSpace arcLength $ path
        CombineTransform a b -> applyTransformer b . applyTransformer a


execSimpleTransformer :: (PointContainer t) => SimpleTransformer (SpaceOf t) -> t -> t
execSimpleTransformer simpleTransformer =
    case simpleTransformer of
        IdentityTransform -> id
        Translate delta   -> applyTranslation delta
        Stretch size      -> applyStretch size
        CombineSimple a b -> execSimpleTransformer b . execSimpleTransformer a

translate_ :: Space s => Point2 s -> Point2 s -> Point2 s
translate_ = (^+^)

stretch_ :: Space s => Point2 s -> Point2 s -> Point2 s
stretch_ = liftA2 (*)

rotate_ :: Space s => Angle s -> Point2 s -> Point2 s
rotate_ = rotate

translateBox p = mapBox (translate_ p)
stretchBox s = mapBox (stretch_ s)

applyTranslation :: PointContainer t => Point2 (SpaceOf t) -> t -> t
applyTranslation p = mapOverPoints (translate_ p)

applyStretch :: PointContainer t => Point2 (SpaceOf t) -> t -> t
applyStretch p = mapOverPoints (stretch_ p)

applyScale :: PointContainer t => SpaceOf t -> t -> t
applyScale s = mapOverPoints (stretch_ . pure $ s)

applyRotation :: PointContainer t => Angle (SpaceOf t) -> t -> t
applyRotation angle = mapOverPoints (rotate_ angle)

instance (Chain f, Space s)
         => CanApplyProjection (Shape_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path shape =
        joinOverBeziers (projectBezierWithStepsAccuracy debug max_steps m_accuracy path) shape

instance ( Space s
         , Chain f
         ) => CanApplyProjection (OpenCurve_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path =
        joinOverBeziers (projectBezierWithStepsAccuracy debug max_steps m_accuracy path)

instance ( Space s
         , Chain f
         )
         => CanApplyProjection (Outline_ f s) where
    projectionWithStepsAccuracy debug max_steps m_accuracy path =
         joinOverBeziers (projectBezierWithStepsAccuracy debug max_steps m_accuracy path)


-- * Instances

instance (Floating s, Num s, Random s) => Random (Transformer s) where
  random = runRand $
    do r :: Int <- getRandomR (0,1)
       case r of
         0 -> do delta :: Point2 s <- getRandom
                 return . Simple . Translate $ delta
         1 -> do scale :: s <- getRandomR(0,100)
                 return . Simple . Stretch $ pure scale
         2 -> do size :: Point2 s <- getRandom
                 return . Simple . Stretch $ size
         3 -> do angle :: s <- getRandomR(0,1)
                 return $ Rotate (angle @@ turn)
  randomR _ = random

instance Hashable s => Hashable (SimpleTransformer s) where
    hashWithSalt s (Translate a) = s `hashWithSalt` (0 :: Int) `hashWithSalt` a
    hashWithSalt s (Stretch a)   = s `hashWithSalt` (2 :: Int) `hashWithSalt` a
    hashWithSalt s (CombineSimple a b) = s `hashWithSalt` (3 :: Int) `hashWithSalt` a `hashWithSalt` b

instance Hashable s => Hashable (Transformer s) where
    hashWithSalt s (Simple a)    = s `hashWithSalt` (0 :: Int) `hashWithSalt` a
    hashWithSalt s (Rotate a)    = s `hashWithSalt` (1 :: Int) `hashWithSalt` a
    hashWithSalt s (Project a)   = s `hashWithSalt` (2 :: Int) `hashWithSalt` a
    hashWithSalt s (CombineTransform a b) = s `hashWithSalt` (3 :: Int) `hashWithSalt` a `hashWithSalt` b
