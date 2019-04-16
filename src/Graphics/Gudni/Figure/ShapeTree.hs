{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}

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
  , tTranslate
  , tTranslateXY
  , tScale
  , tRotate
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
  )
where

import Graphics.Gudni.Figure.HasDefault
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Box
--import Graphics.Gudni.Figure.Glyph
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Picture
import Graphics.Gudni.Figure.Outline

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
data STree meld leaf where
  STransform :: Transformer (SpaceOf leaf) -> STree meld leaf -> STree meld leaf
  SMeld      :: meld -> STree meld leaf -> STree meld leaf -> STree meld leaf
  SLeaf      :: leaf -> STree meld leaf

deriving instance (Show meld, Show leaf, Show (SpaceOf leaf)) => Show (STree meld leaf)

instance HasSpace leaf => HasSpace (STree meld leaf) where
  type SpaceOf (STree meld leaf) = SpaceOf leaf
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

instance HasSpace rep => HasSpace (SRep token substance rep) where
  type SpaceOf (SRep token substance rep) = SpaceOf rep

instance (HasSpace leaf) => SimpleTransformable (STree o leaf) where
  tTranslate delta = STransform (Translate delta)
  tScale factor    = STransform (Scale factor)

instance (HasSpace leaf) => Transformable (STree o leaf) where
  tRotate angle = STransform (Rotate angle)

instance Functor (SRep token substance) where
  fmap f (SRep token substance rep) = SRep token substance (f rep)

instance NFData r => NFData (Substance r) where
  rnf (Solid color) = color `deepseq` ()
  rnf (Texture pict) = pict `deepseq` ()

type CompoundTree s = STree Compound [Outline s]
type ShapeTree token s = STree () (SRep token (PictureUsage PictId) (CompoundTree s))

-- | A container for a ShapeTree that indicates the background color.
data Scene token = Scene
  { _sceneBackgroundColor :: Color
  , _sceneShapeTree       :: ShapeTree token SubSpace
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

{-
---------------------------- Instances -------------------------------------
instance (SpaceOf a Functor (STree overlap) where
  fmap f (SLeaf child)                 = SLeaf $ f child
  fmap f (STransform t child)           = STransform t  $ fmap f child
  fmap f (SMeld overlap above below) = SMeld overlap (fmap f above) (fmap f below)

instance Foldable (STree overlap) where
  foldr f item (SLeaf child)  = f child item
  foldr f item (STransform t child)   = foldr f item child
  foldr f item (SMeld overlap above below) = foldr f (foldr f item above) below
  foldMap f (SLeaf child)  = f child
  foldMap f (STransform t child)   = foldMap f child
  foldMap f (SMeld overlap above below) = foldMap f above `mappend` foldMap f below

instance Traversable (STree overlap) where
  traverse f (SLeaf child)  = fmap SLeaf (f child)
  traverse f (STransform t child)   = fmap (STransform t) (traverse f child)
  traverse f (SMeld overlap above below) = liftA2 (SMeld overlap) (traverse f above) (traverse f below)
  sequenceA (SLeaf child)  = fmap SLeaf child
  sequenceA (STransform t child)   = fmap (STransform t ) (sequenceA child)
  sequenceA (SMeld overlap above below) = liftA2 (SMeld overlap) (sequenceA above) (sequenceA below)
-}
