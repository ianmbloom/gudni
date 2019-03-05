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
  , sTranslate
  , sTranslateXY
  , sScale
  , sRotate
  , mapShapeRep
  , shapeSubstanceType
  , shapeToken
  , shapeCompoundTree
  , Substance (..)
  , CombineType (..)
  , ShapeTreeRoot(..)
  , ShapeTree(..)
  , CompoundTree(..)
  , DefaultOverlap(..)
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

data STree overlap trans leaf where
  SLeaf      :: leaf -> STree overlap trans leaf
  STransform :: trans -> STree overlap trans leaf -> STree overlap trans leaf
  SOverlap   :: overlap -> STree overlap trans leaf -> STree overlap trans leaf -> STree overlap trans leaf

data STreeRoot tree = ShapeRoot
  { backgroundColor :: Color
  , shapeTreeTree   :: tree
  }

data CombineType = CombineContinue | CombineAdd | CombineSubtract deriving (Ord, Eq, Show)

data Substance r = Solid Color | Texture r deriving (Show)

data SRep token substance rep = SRep
  { _shapeToken        :: token
  , _shapeSubstanceType    :: Substance substance
  , _shapeCompoundTree :: rep
  } deriving (Show)
makeLenses ''SRep

-- | Add a translate node to a tree.
sTranslate :: Point2 s -> STree o (TransformType s) leaf -> STree o (TransformType s) leaf
sTranslate delta = STransform (Translate delta)

-- | Convenience function to make a translation node from the component dimensions.
sTranslateXY :: Ortho XDimension s -> Ortho YDimension s -> STree o (TransformType s) leaf -> STree o (TransformType s) leaf
sTranslateXY x y = sTranslate (makePoint x y)

-- | Add a scale node to a tree
sScale :: s -> STree o (TransformType s) leaf -> STree o (TransformType s) leaf
sScale     scale = STransform (Scale scale)

-- | Add a rotate node to a tree
sRotate :: Angle s -> STree o (TransformType s) leaf -> STree o (TransformType s) leaf
sRotate    angle = STransform (Rotate angle)

instance Functor (SRep token substance) where
  fmap f (SRep token substance rep) = SRep token substance (f rep)

mapShapeRep :: (a -> b) -> SRep token substance a -> SRep token substance b
mapShapeRep f (SRep token sub compoundTree) = SRep token sub (f compoundTree)

instance NFData r => NFData (Substance r) where
  rnf (Solid color) = color `deepseq` ()
  rnf (Texture pict) = pict `deepseq` ()

type CompoundTree  = STree CombineType (TransformType DisplaySpace) RawShape
type ShapeTree     = STree () (TransformType DisplaySpace) (SRep Int (PictureRef PictId) CompoundTree)
type ShapeTreeRoot = STreeRoot ShapeTree

instance NFData CombineType where
  rnf _ = ()

class DefaultOverlap o where
  defaultOverlap :: o

instance DefaultOverlap () where
  defaultOverlap = ()

instance DefaultOverlap CombineType where
  defaultOverlap = CombineAdd

invertCombineType :: CombineType -> CombineType
invertCombineType combineType =
    case combineType of
        CombineAdd      -> CombineSubtract
        CombineSubtract -> CombineAdd
        CombineContinue -> CombineContinue

cAdd      :: STree CombineType o t -> STree CombineType o t -> STree CombineType o t
cSubtract :: STree CombineType o t -> STree CombineType o t -> STree CombineType o t
cContinue :: STree CombineType o t -> STree CombineType o t -> STree CombineType o t
cAdd      = SOverlap CombineAdd
cSubtract = SOverlap CombineSubtract
cContinue = SOverlap CombineContinue

---------------------------- Instances -------------------------------------

instance Functor (STree overlap trans) where
  fmap f (SLeaf child)                 = SLeaf $ f child
  fmap f (STransform t child)           = STransform t  $ fmap f child
  fmap f (SOverlap overlap above below) = SOverlap overlap (fmap f above) (fmap f below)

instance Foldable (STree overlap trans) where
  foldr f item (SLeaf child)  = f child item
  foldr f item (STransform t child)   = foldr f item child
  foldr f item (SOverlap overlap above below) = foldr f (foldr f item above) below
  foldMap f (SLeaf child)  = f child
  foldMap f (STransform t child)   = foldMap f child
  foldMap f (SOverlap overlap above below) = foldMap f above `mappend` foldMap f below

instance Traversable (STree overlap trans) where
  traverse f (SLeaf child)  = fmap SLeaf (f child)
  traverse f (STransform t child)   = fmap (STransform t) (traverse f child)
  traverse f (SOverlap overlap above below) = liftA2 (SOverlap overlap) (traverse f above) (traverse f below)
  sequenceA (SLeaf child)  = fmap SLeaf child
  sequenceA (STransform t child)   = fmap (STransform t ) (sequenceA child)
  sequenceA (SOverlap overlap above below) = liftA2 (SOverlap overlap) (sequenceA above) (sequenceA below)
