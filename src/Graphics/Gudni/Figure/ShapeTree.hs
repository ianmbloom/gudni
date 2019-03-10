{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.ShapeTree
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A ShapeTree is the main input data structure for the Gudni Rasterizer. A client program
-- generates a Scene which contains a ShapeTree for each frame that they wish to render.

module Graphics.Gudni.Figure.ShapeTree
  ( STree(..)
  , SRep(..)
  , sTranslate
  , sTranslateXY
  , sScale
  , sRotate
  , shapeSubstanceType
  , shapeToken
  , shapeCompoundTree
  , Substance (..)
  , Compound (..)
  , ShapeTree(..)
  , CompoundTree(..)
  , Scene(..)
  , sceneBackgroundColor
  , sceneShapeTree
  , HasDefault(..)
  , invertCompound
  , cAdd
  , cSubtract
  , cContinue
  )
where

import Graphics.Gudni.Figure.HasDefault
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

-- | Polymorphic data structure for trees of shapes. The meld type is the operations for combining two subtrees,
-- the trans type defines transformations that can be applied across to subtree and the leaf type the component elements of
-- a tree. The ShapeTree type is an STree that melds with Overlap whose component type is an SRep which itself contains
-- an STree that melds with Compound, and whose component type is RawShape.
data STree meld trans leaf where
  STransform :: trans -> STree meld trans leaf -> STree meld trans leaf
  SMeld      :: meld -> STree meld trans leaf -> STree meld trans leaf -> STree meld trans leaf
  SLeaf      :: leaf -> STree meld trans leaf
  deriving (Show)

-- | Type of melding of compound shapes.
data Compound
        -- | Neutral combination of outlines.
        = CompoundContinue
        -- | Addition of shapes.
        | CompoundAdd
        -- | Substraction of the first shape from the second.
        | CompoundSubtract
        deriving (Ord, Eq, Show)

-- | Type of overlapping two seperate shapes.
type Overlap = ()

instance HasDefault Compound where
  defaultValue = CompoundAdd

instance HasDefault Overlap where
  defaultValue = ()

-- | Type of filling for overlapping shapes.
data Substance r = Solid Color | Texture r deriving (Show)

-- | An SRep defines an individual shape and it's metadata.
data SRep token substance rep = SRep
  { _shapeToken         :: token
  , _shapeSubstanceType :: Substance substance
  , _shapeCompoundTree  :: rep
  } deriving (Show)
makeLenses ''SRep

-- | Add a translate node to a tree.
sTranslate :: Point2 s -> STree o (Transformer s) leaf -> STree o (Transformer s) leaf
sTranslate delta = STransform (Translate delta)

-- | Convenience function to make a translation node from the component dimensions.
sTranslateXY :: Ortho XDimension s -> Ortho YDimension s -> STree o (Transformer s) leaf -> STree o (Transformer s) leaf
sTranslateXY x y = sTranslate (makePoint x y)

-- | Add a scale node to a tree
sScale :: s -> STree o (Transformer s) leaf -> STree o (Transformer s) leaf
sScale     scale = STransform (Scale scale)

-- | Add a rotate node to a tree
sRotate :: Angle s -> STree o (Transformer s) leaf -> STree o (Transformer s) leaf
sRotate    angle = STransform (Rotate angle)

instance Functor (SRep token substance) where
  fmap f (SRep token substance rep) = SRep token substance (f rep)

instance NFData r => NFData (Substance r) where
  rnf (Solid color) = color `deepseq` ()
  rnf (Texture pict) = pict `deepseq` ()

type CompoundTree  = STree Compound (Transformer SubSpace) RawShape
type ShapeTree token = STree () (Transformer SubSpace) (SRep token (PictureUsage PictId) CompoundTree)

-- | A container for a ShapeTree that indicates the background color.
data Scene token = Scene
  { _SceneBackgroundColor :: Color
  , _SceneShapeTree       :: ShapeTree token
  } deriving (Show)
makeLenses ''Scene

instance NFData Compound where
  rnf _ = ()

-- Invert a compound type
invertCompound :: Compound -> Compound
invertCompound combineType =
    case combineType of
        CompoundAdd      -> CompoundSubtract
        CompoundSubtract -> CompoundAdd
        CompoundContinue -> CompoundContinue

-- | Combine two subtrees by adding them.
cAdd      :: STree Compound o t -> STree Compound o t -> STree Compound o t
cAdd      = SMeld CompoundAdd

-- | Combine two subtrees by substracting the first from the second.
cSubtract :: STree Compound o t -> STree Compound o t -> STree Compound o t
cSubtract = SMeld CompoundSubtract

-- | Combine two subtrees by nuetrally concatenating their component outlines.
cContinue :: STree Compound o t -> STree Compound o t -> STree Compound o t
cContinue = SMeld CompoundContinue

---------------------------- Instances -------------------------------------
instance Functor (STree overlap trans) where
  fmap f (SLeaf child)                 = SLeaf $ f child
  fmap f (STransform t child)           = STransform t  $ fmap f child
  fmap f (SMeld overlap above below) = SMeld overlap (fmap f above) (fmap f below)

instance Foldable (STree overlap trans) where
  foldr f item (SLeaf child)  = f child item
  foldr f item (STransform t child)   = foldr f item child
  foldr f item (SMeld overlap above below) = foldr f (foldr f item above) below
  foldMap f (SLeaf child)  = f child
  foldMap f (STransform t child)   = foldMap f child
  foldMap f (SMeld overlap above below) = foldMap f above `mappend` foldMap f below

instance Traversable (STree overlap trans) where
  traverse f (SLeaf child)  = fmap SLeaf (f child)
  traverse f (STransform t child)   = fmap (STransform t) (traverse f child)
  traverse f (SMeld overlap above below) = liftA2 (SMeld overlap) (traverse f above) (traverse f below)
  sequenceA (SLeaf child)  = fmap SLeaf child
  sequenceA (STransform t child)   = fmap (STransform t ) (sequenceA child)
  sequenceA (SMeld overlap above below) = liftA2 (SMeld overlap) (sequenceA above) (sequenceA below)
