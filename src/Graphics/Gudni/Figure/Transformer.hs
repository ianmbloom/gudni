{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

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

module Graphics.Gudni.Figure.Transformer
 ( Transformer (..)
 , applyTransformer
)
where

import Graphics.Gudni.Figure.HasDefault
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.Transformable

import Graphics.Gudni.Util.Debug

import Control.Monad.Random
import Control.Applicative
import Control.Lens
import Control.DeepSeq

import Data.Traversable
import Data.Hashable
import Data.Either

import System.Random

instance Num s => HasDefault (Transformer s) where
    defaultValue = Translate (Point2 0 0)

data Transformer s where
  IdentityTransform :: Transformer s
  Translate :: Point2 s -> Transformer s
  Scale     :: s        -> Transformer s
  Stretch   :: Point2 s -> Transformer s
  Rotate    :: Angle s  -> Transformer s
  Project   :: Bool -> BezierSpace s -> Transformer s
  CombineTransform :: Transformer s -> Transformer s -> Transformer s
  deriving (Show)

applyTransformer :: (CanProject (BezierSpace s) t, Transformable t) => Transformer (SpaceOf t) -> t -> t
applyTransformer IdentityTransform = id
applyTransformer (Translate delta) = translateBy delta
applyTransformer (Scale scale)     = scaleBy scale
applyTransformer (Stretch size)    = stretchBy size
applyTransformer (Rotate angle)    = rotateBy angle
applyTransformer (Project debug curve)   = projectOnto debug curve
applyTransformer (CombineTransform a b) = applyTransformer b . applyTransformer a

-- * Instances

instance NFData s => NFData (Transformer s) where
  rnf (Translate a) = a `deepseq` ()
  rnf (Scale     a) = a `deepseq` ()
  rnf (Stretch   a) = a `deepseq` ()
  rnf (Rotate    a) = a `deepseq` ()

instance (Floating s, Num s, Random s) => Random (Transformer s) where
  random = runRand $
    do r :: Int <- getRandomR (0,1)
       case r of
         0 -> do delta :: Point2 s <- getRandom
                 return $ Translate delta
         1 -> do scale :: s <- getRandomR(0,100)
                 return $ Scale scale
         2 -> do size :: Point2 s <- getRandom
                 return $ Stretch size
         3 -> do angle :: s <- getRandomR(0,1)
                 return $ Rotate (angle @@ turn)
  randomR _ = random

instance Hashable s => Hashable (Transformer s) where
    hashWithSalt s (Translate a) = s `hashWithSalt` (0 :: Int) `hashWithSalt` a
    hashWithSalt s (Scale a)     = s `hashWithSalt` (1 :: Int) `hashWithSalt` a
    hashWithSalt s (Stretch a)   = s `hashWithSalt` (2 :: Int) `hashWithSalt` a
    hashWithSalt s (Rotate a)    = s `hashWithSalt` (3 :: Int) `hashWithSalt` a
