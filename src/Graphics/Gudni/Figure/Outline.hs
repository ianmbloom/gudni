{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Graphics.Gudni.Figure.Outline
  ( CurvePair
  , pattern CurvePair
  , onCurve
  , offCurve
  , Outline(..)
  , mapOutline
  , outlineBox
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Util.Util
import Control.Lens
import Linear.V2

newtype CurvePair a = Cp {_unCp :: V2 a} deriving (Eq, Ord, Show, Num, Functor, Applicative, Foldable)
makeLenses ''CurvePair
pattern CurvePair a b = Cp (V2 a b)

onCurve :: Lens' (CurvePair a) a
onCurve = unCp . _x
offCurve :: Lens' (CurvePair a) a
offCurve = unCp . _y

data Outline s = Outline [CurvePair (Point2 s)]
               deriving (Eq, Ord, Show)

mapOutline :: (Point2 a -> Point2 b) -> Outline a -> Outline b
mapOutline f (Outline ps) = Outline (map (fmap f) ps)

expandPairs :: CurvePair a -> [a]
expandPairs (CurvePair a b) = [a,b]

outlineBox :: Outline DisplaySpace -> Box DisplaySpace
outlineBox curve@(Outline vs) = boxPointList $ concatMap expandPairs $ vs
