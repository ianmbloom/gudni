{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}

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

data TransformType where
  Translate :: Point2 DisplaySpace -> TransformType
  Scale     :: DisplaySpace        -> TransformType
  Rotate    :: Angle DisplaySpace  -> TransformType
  deriving (Show)

instance NFData TransformType where
  rnf (Translate a) = a `deepseq` ()
  rnf (Scale a) = a `deepseq` ()
  rnf (Rotate a) = a `deepseq` ()

instance Random TransformType where
  random = runRand $
    do r :: Int <- getRandomR (0,1)
       case r of
         0 -> do delta :: Point2 DisplaySpace <- getRandom
                 return $ Translate delta
         1 -> do scale :: DisplaySpace <- getRandomR(0,100)
                 return $ Scale scale
         2 -> do angle :: DisplaySpace <- getRandomR(0,1)
                 return $ Rotate (angle @@ turn)
  randomR _ = random

instance Hashable TransformType where
    hashWithSalt s (Translate a) = s `hashWithSalt` (0 :: Int) `hashWithSalt` a
    hashWithSalt s (Scale a)     = s `hashWithSalt` (1 :: Int) `hashWithSalt` a
    hashWithSalt s (Rotate a)    = s `hashWithSalt` (2 :: Int) `hashWithSalt` a

applyTransformType :: Transformable t => TransformType -> t -> t
applyTransformType (Translate delta) = tTranslate delta
applyTransformType (Scale scale)     = tScale scale
applyTransformType (Rotate angle)    = tRotate angle

class SimpleTransformable a where
  tTranslate :: Point2 DisplaySpace -> a -> a
  tScale     :: DisplaySpace -> a -> a

class SimpleTransformable a => Transformable a where
  tRotate    :: Angle DisplaySpace -> a -> a

tTranslateXY :: SimpleTransformable a => DisplaySpace -> DisplaySpace -> a -> a
tTranslateXY x y = tTranslate $ Point2 x y

instance SimpleTransformable (Point2 DisplaySpace) where
    tTranslate = (^+^)
    tScale     = flip (^*)
instance Transformable (Point2 DisplaySpace) where
    tRotate    = rotate

instance SimpleTransformable f => SimpleTransformable [f] where
    tTranslate p = map (tTranslate p)
    tScale     s = map (tScale s)
instance Transformable f => Transformable [f] where
    tRotate    a = map (tRotate a)
