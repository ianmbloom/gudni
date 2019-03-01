{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Graphics.Gudni.Figure.Outline
  ( CurvePair
  , pattern CurvePair
  , onCurve
  , offCurve
  , pairPoints
  , Outline(..)
  , mapOutline
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Util.Util
import Control.Lens
import Linear.V2

import Data.Hashable
import Control.DeepSeq

newtype CurvePair p = Cp {_unCp :: V2 p} deriving (Eq, Ord, Show, Num, Functor, Applicative, Foldable)
makeLenses ''CurvePair
pattern CurvePair a b = Cp (V2 a b)

onCurve :: Lens' (CurvePair a) a
onCurve = unCp . _x
offCurve :: Lens' (CurvePair a) a
offCurve = unCp . _y

pairPoints :: [Point2 s] -> [CurvePair (Point2 s)]
pairPoints (v0:v1:rest) = (CurvePair v0 v1):pairPoints rest
pairPoints [] = []
pairPoints [v0] = []

data Outline s = Outline [CurvePair (Point2 s)]
               deriving (Eq, Ord, Show)

mapOutline :: (Point2 a -> Point2 b) -> Outline a -> Outline b
mapOutline f (Outline ps) = Outline (map (fmap f) ps)

instance Boxable (CurvePair (Point2 DisplaySpace)) where
  getBoundingBox (CurvePair a b) =
      let top    = min (a ^. pY) (b ^. pY)
          bottom = max (a ^. pY) (b ^. pY)
          left   = min (a ^. pX) (b ^. pX)
          right  = max (a ^. pX) (b ^. pX)
      in makeBox left top right bottom

instance Boxable (Outline DisplaySpace) where
  getBoundingBox (Outline vs) = getBoundingBox $ map getBoundingBox $ vs

instance SimpleTransformable (Point2 s) => SimpleTransformable (Outline s) where
  tTranslate p = mapOutline (tTranslate p)
  tScale     s = mapOutline (tScale s)
instance Transformable (Point2 s) => Transformable (Outline s) where
  tRotate    a = mapOutline (tRotate a)

instance NFData p => NFData (CurvePair p) where
  rnf (Cp v2) = v2 `deepseq` ()

instance NFData s => NFData (Outline s) where
  rnf (Outline ps) = ps `deepseq` ()

instance Hashable p => Hashable (CurvePair p) where
  hashWithSalt s (Cp v2) = s `hashWithSalt` v2
instance Hashable s => Hashable (Outline s) where
  hashWithSalt s (Outline ps) = s `hashWithSalt` ps
