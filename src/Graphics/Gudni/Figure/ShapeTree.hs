{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Graphics.Gudni.Figure.ShapeTree
  ( STree(..)
  , STreeRoot(..)
  , SRep(..)
  , mapShapeRep
  , shapeSubstance
  , shapeToken
  , shapeCompoundTree
  , Substance (..)
  , CombineType (..)
  , ShapeTreeRoot(..)
  , ShapeTree(..)
  , CompoundTree(..)
  , DefaultOverlap(..)
  , Combinable(..)
  , invertCombineType
  , cAdd
  , cSubtract
  , cContinue
  )
where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Glyph
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Picture
import Graphics.Gudni.Figure.RawShape

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util

import Control.Monad.State
import Control.Monad.Random
import Control.Applicative
import Control.Lens
import Control.DeepSeq

import Data.List
import Data.Hashable
import Data.Traversable
import qualified Data.Map as M

import Foreign.C.Types (CInt, CFloat, CUInt)
import Foreign.Storable

data STree overlap rep where
  SLeaf      :: rep -> STree overlap rep
  STransform :: TransformType -> STree overlap rep -> STree overlap rep
  SOverlap   :: overlap -> STree overlap rep -> STree overlap rep -> STree overlap rep
  deriving (Show)

data STreeRoot token substance rep = ShapeRoot
  { backgroundColor :: Color
  , shapeTreeTree   :: STree () (SRep token substance rep)
  }

data CombineType = CombineContinue | CombineAdd | CombineSubtract deriving (Ord, Eq, Show)

data Substance r = Solid Color | Texture r deriving (Show)

data SRep token substance rep = SRep
  { _shapeToken        :: token
  , _shapeSubstance    :: Substance substance
  , _shapeCompoundTree :: rep
  } deriving (Show)
makeLenses ''SRep

mapShapeRep :: (a -> b) -> SRep token substance a -> SRep token substance b
mapShapeRep f (SRep token sub compoundTree) = SRep token sub (f compoundTree)

instance NFData r => NFData (Substance r) where
  rnf (Solid color) = color `deepseq` ()
  rnf (Texture pict) = pict `deepseq` ()

type CompoundTree = STree CombineType RawShape
type ShapeTree = STree () (SRep Int (PictureRef PictId) CompoundTree)
type ShapeTreeRoot = STreeRoot Int (PictureRef PictId) CompoundTree

instance NFData CombineType where
  rnf _ = ()

class DefaultOverlap o where
  defaultOverlap :: o

instance DefaultOverlap () where
  defaultOverlap = ()

instance DefaultOverlap CombineType where
  defaultOverlap = CombineAdd

class Combinable o t where
  combine :: o -> t -> t -> t

invertCombineType :: CombineType -> CombineType
invertCombineType combineType =
    case combineType of
        CombineAdd      -> CombineSubtract
        CombineSubtract -> CombineAdd
        CombineContinue -> CombineContinue

cAdd      = SOverlap CombineAdd
cSubtract = SOverlap CombineSubtract
cContinue = SOverlap CombineContinue

instance SimpleTransformable (STree o rep) where
  tTranslate p = STransform (Translate p)
  tScale     s = STransform (Scale s)
instance Transformable (STree o rep) where
  tRotate    r = STransform (Rotate r)

instance SimpleTransformable rep => SimpleTransformable (SRep token substance rep) where
  tTranslate p = mapShapeRep (tTranslate p)
  tScale     s = mapShapeRep (tScale     s)
instance Transformable rep => Transformable (SRep token substance rep) where
  tRotate    r = mapShapeRep (tRotate    r)

---------------------------- Instances -------------------------------------

instance Functor (STree overlap) where
  fmap f (SLeaf child)                 = SLeaf $ f child
  fmap f (STransform t child)           = STransform t  $ fmap f child
  fmap f (SOverlap overlap above below) = SOverlap overlap (fmap f above) (fmap f below)

instance Foldable (STree overlap) where
  foldr f item (SLeaf child)  = f child item
  foldr f item (STransform t child)   = foldr f item child
  foldr f item (SOverlap overlap above below) = foldr f (foldr f item above) below
  foldMap f (SLeaf child)  = f child
  foldMap f (STransform t child)   = foldMap f child
  foldMap f (SOverlap overlap above below) = foldMap f above `mappend` foldMap f below

instance Traversable (STree overlap) where
  traverse f (SLeaf child)  = fmap SLeaf (f child)
  traverse f (STransform t child)   = fmap (STransform t) (traverse f child)
  traverse f (SOverlap overlap above below) = liftA2 (SOverlap overlap) (traverse f above) (traverse f below)
  sequenceA (SLeaf child)  = fmap SLeaf child
  sequenceA (STransform t child)   = fmap (STransform t ) (sequenceA child)
  sequenceA (SOverlap overlap above below) = liftA2 (SOverlap overlap) (sequenceA above) (sequenceA below)
