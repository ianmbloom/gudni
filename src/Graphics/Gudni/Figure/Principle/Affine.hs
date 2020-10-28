{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE Rank2Types            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Principle.Affine
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Affine Transformations encoded in matrices.

module Graphics.Gudni.Figure.Principle.Affine
  ( Affine(..)
  , affineIdentity
  , affineTranslate
  , affineRotate
  , affineStretch
  , affineScale
  , affineShear
  , applyAffine
  , composeAffine
  ) where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle.Space
import Graphics.Gudni.Figure.Principle.Axis
import Graphics.Gudni.Figure.Principle.Angle
import Graphics.Gudni.Figure.Principle.Point

import Control.Lens
import Control.Applicative

import Linear.V1
import Linear.V3
import Linear.Matrix

newtype Affine s = Affine {unAffine :: V3 (V3 s) } deriving (Eq, Show, Generic)

affineIdentity :: Space s => Affine s
affineIdentity =
   Affine $
   V3 (V3 1 0 0)
      (V3 0 1 0)
      (V3 0 0 1)

affineTranslate :: Space s => Point2 s -> Affine s
affineTranslate (Point2 x y) =
    Affine $
    V3 (V3 1 0 x)
       (V3 0 1 y)
       (V3 0 0 1)

affineRotate :: Space s => Angle s -> Affine s
affineRotate a =
  let c = cosA a
      s = sinA a
  in  Affine $
      V3 (V3 c (-s) 0)
         (V3 s   c  0)
         (V3 0   0  1)

affineStretch :: Space s => Point2 s -> Affine s
affineStretch (Point2 x y) =
    Affine $
    V3 (V3 x 0 0)
       (V3 0 y 0)
       (V3 0 0 1)

affineScale :: Space s => s -> Affine s
affineScale s = affineStretch (pure s)

affineShear :: Space s => Point2 s -> Affine s
affineShear (Point2 x y) =
    Affine $
    V3 (V3 1 y 0)
       (V3 x 1 0)
       (V3 0 0 1)

composeAffine :: Space s => Affine s -> Affine s -> Affine s
composeAffine (Affine a) (Affine b) = Affine $ b !*! a

applyAffine :: Space s => Affine s -> Point2 s -> Point2 s
applyAffine (Affine a) (Point2 x y) = let (V3 (V1 x') (V1 y') _) = a !*! (fmap V1 $ V3 x y 1) in Point2 x' y'

outV3H (V3 a b c) = a <+> b <+> c
outV3V (V3 a b c) = a $$  b $$  c

instance (Out s) => Out (Affine s) where
    doc (Affine matrix) = outV3V . fmap outV3H . fmap (fmap doc) $ matrix
    docPrec _ = doc
