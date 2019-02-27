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
  , pX
  , pY
  , makePoint
  , zeroPoint
  , Vert2 (..)
  , pattern Vert2
  , Vertex (..)
  , isOnCurve
  , stripVertex
  , vX
  , vY
  , isControl

  , (^+^)
  , (^-^)
  , (^*)
  , (^/)
  , lerp
  , negated
  , normalize
  , norm
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

pX :: Lens' (Point2 s) (Ortho XDimension s)
pX elt_fn (Point2 x y) = (\x' -> Point2 (unOrtho x') y) <$> (elt_fn . Ortho $ x)

pY :: Lens' (Point2 s) (Ortho YDimension s)
pY elt_fn (Point2 x y) = (\y' -> Point2 x (unOrtho y')) <$> (elt_fn . Ortho $ y)

{-# INLINE makePoint #-}
makePoint :: Ortho XDimension s -> Ortho YDimension s -> Point2 s
makePoint (Ortho x) (Ortho y) = P (V2 x y)
zeroPoint :: Num s => Point2 s
zeroPoint = Point2 0 0

instance (Functor f, Convertable a b) => Convertable (Point f a) (Point f b) where
  convert = fmap convert

instance Random s => Random (Point2 s) where
  random = runRand $ do x <- getRandom; y <- getRandom; return (Point2 x y)
  randomR (Point2 x0 y0, Point2 x1 y1) = runRand $ do x <- getRandomR (x0, x1)
                                                      y <- getRandomR (y0, y1)
                                                      return (Point2 x y)

data Vertex s = Vert
  { _isOnCurve :: Bool
  , _stripVertex :: Point2 s }
  deriving (Eq, Ord)
makeLenses ''Vertex

isControl :: Lens' (Vertex s) Bool
isControl elt_fn (Vert isOnCurve p) = (\isControl -> Vert (not isControl) p) <$> elt_fn (not isOnCurve)
type Vert2 s = Vertex s
pattern Vert2 o x y = Vert o (Point2 x y)

vX :: Lens' (Vertex s) (Ortho XDimension s)
vX = stripVertex . pX

vY :: Lens' (Vertex s) (Ortho YDimension s)
vY = stripVertex . pY

instance Convertable a b => Convertable (Vertex a) (Vertex b) where
  convert = over stripVertex convert

instance Random s => Random (Vertex s) where
  random = runRand $ do o <- getRandom; v <- getRandom; return $ Vert o v
  randomR (Vert o0 v0, Vert o1 v1) = runRand $ do o <- getRandom
                                                  v <- getRandomR (v0, v1)
                                                  return $ Vert ((o0 /= o1) && o) v

instance NFData s => NFData (Vertex s) where
  rnf (Vert o c) = o `deepseq` c `deepseq` ()
instance Hashable s => Hashable (Vertex s) where
  hashWithSalt s (Vert o c) = s `hashWithSalt` o `hashWithSalt` c
instance Show s => Show (Vertex s) where
  show (Vert o c) = "V-" ++ (if o then "On:" else "Off:") ++ show c
