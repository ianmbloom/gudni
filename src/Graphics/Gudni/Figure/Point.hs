{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Graphics.Gudni.Figure.Point
  ( Point2 (..)
  , pattern Point2
  , pattern P
  , zeroPoint
  , pX
  , pY
  , makePoint
  , (^+^)
  , (^-^)
  , (^*)
  , (^/)
  , lerp
  , negated
  , normalize
  , norm
  , pointArea
)
where

import Graphics.Gudni.Figure.Space

import Data.Hashable

import Linear
import Linear.Affine

import System.Random

import Foreign.Storable
import Foreign.Ptr

import Control.Monad.Random
import Control.DeepSeq
import Control.Lens

type Point2 = Point V2
pattern Point2 x y = P (V2 x y)

zeroPoint :: Num s => Point2 s
zeroPoint = Point2 0 0
-- | Lens for the x element of a point.
pX :: Lens' (Point2 s) (Ortho XDimension s)
pX elt_fn (Point2 x y) = (\x' -> Point2 (unOrtho x') y) <$> (elt_fn . Ortho $ x)

-- | Lens for the y element of a point.
pY :: Lens' (Point2 s) (Ortho YDimension s)
pY elt_fn (Point2 x y) = (\y' -> Point2 x (unOrtho y')) <$> (elt_fn . Ortho $ y)

{-# INLINE makePoint #-}
-- | Make a point from two orthogonal dimensions.
makePoint :: Ortho XDimension s -> Ortho YDimension s -> Point2 s
makePoint (Ortho x) (Ortho y) = P (V2 x y)

pointArea :: Num s => Point2 s -> s
pointArea (Point2 x y) = x * y

instance (Functor f, Convertable a b) => Convertable (Point f a) (Point f b) where
  convert = fmap convert

instance Random s => Random (Point2 s) where
  random = runRand $ do x <- getRandom; y <- getRandom; return (Point2 x y)
  randomR (Point2 x0 y0, Point2 x1 y1) = runRand $ do x <- getRandomR (x0, x1)
                                                      y <- getRandomR (y0, y1)
                                                      return (Point2 x y)
