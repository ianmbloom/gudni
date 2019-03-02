{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Transformer
 ( TransformType (..)
 , applyTransformType
 , SimpleTransformable(..)
 , Transformable(..)
 , tTranslateXY
)
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Point
--import Graphics.Gudni.Figure.Box
--import Graphics.Gudni.Figure.Outline

import Graphics.Gudni.Util.Debug

import Control.Monad.Random
import Control.Applicative
import Control.Lens
import Control.DeepSeq

import Data.Traversable
import Data.Hashable
import Data.Either

import System.Random

class (Num s) => SimpleTransformable t s where
  tTranslate :: Point2 s -> t s -> t s
  tScale     :: s -> t s -> t s

class SimpleTransformable t s => Transformable t s where
  tRotate    :: Angle s -> t s -> t s

tTranslateXY :: SimpleTransformable t s => s -> s -> t s -> t s
tTranslateXY x y = tTranslate $ Point2 x y

instance Num s => SimpleTransformable Point2 s where
    tTranslate = (^+^)
    tScale     = flip (^*)
instance (Floating s, Num s) => Transformable Point2 s where
    tRotate    = rotate

data TransformType s where
  Translate :: Point2 s -> TransformType s
  Scale     :: s        -> TransformType s
  Rotate    :: Angle s  -> TransformType s

applyTransformType :: Transformable t s => TransformType s -> t s -> t s
applyTransformType (Translate delta) = tTranslate delta
applyTransformType (Scale scale)     = tScale scale
applyTransformType (Rotate angle)    = tRotate angle

-- * Instances

instance NFData s => NFData (TransformType s) where
  rnf (Translate a) = a `deepseq` ()
  rnf (Scale     a) = a `deepseq` ()
  rnf (Rotate    a) = a `deepseq` ()

instance (Floating s, Num s, Random s) => Random (TransformType s) where
  random = runRand $
    do r :: Int <- getRandomR (0,1)
       case r of
         0 -> do delta :: Point2 s <- getRandom
                 return $ Translate delta
         1 -> do scale :: s <- getRandomR(0,100)
                 return $ Scale scale
         2 -> do angle :: s <- getRandomR(0,1)
                 return $ Rotate (angle @@ turn)
  randomR _ = random

instance Hashable s => Hashable (TransformType s) where
    hashWithSalt s (Translate a) = s `hashWithSalt` (0 :: Int) `hashWithSalt` a
    hashWithSalt s (Scale a)     = s `hashWithSalt` (1 :: Int) `hashWithSalt` a
    hashWithSalt s (Rotate a)    = s `hashWithSalt` (2 :: Int) `hashWithSalt` a
