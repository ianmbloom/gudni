{-# LANGUAGE UndecidableInstances #-} -- Show (SpaceOf leaf)
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
  , sRepToken
  , sRepSubstance
  , sRep
  , Compound (..)
  , ShapeTree(..)
  , Shape(..)
  , shapeOutlines
  , CompoundTree(..)
  , Scene(..)
  , sceneBackgroundColor
  , sceneShapeTree
  , HasDefault(..)
  , invertCompound
  , PictureName(..)
  , Picture(..)
  , NamedTexture(..)
  )
where

import Graphics.Gudni.Figure.HasDefault
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.OpenCurve
--import Graphics.Gudni.Figure.Glyph
import Graphics.Gudni.Figure.Substance
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.Transformable
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

newtype Shape s = Shape {_shapeOutlines :: [Outline s]} deriving (Show)
makeLenses ''Shape

instance Space s => HasSpace (Shape s) where
  type SpaceOf (Shape s) = s


-- | Polymorphic data structure for trees of shapes. The meld type is the operations for combining two subtrees,
-- the trans type defines transformations that can be applied across to subtree and the leaf type the component elements of
-- a tree. The ShapeTree type is an STree that melds with Overlap whose component type is an SRep which itself contains
-- an STree that melds with Compound, and whose component type is RawShape.
data STree meld leaf where
  STransform :: Transformer (SpaceOf leaf) -> STree meld leaf -> STree meld leaf
  SMeld      :: meld -> STree meld leaf -> STree meld leaf -> STree meld leaf
  SLeaf      :: leaf -> STree meld leaf
  SEmpty     :: STree meld leaf

deriving instance (Show meld, Show leaf, Show (SpaceOf leaf), Space (SpaceOf leaf)) => Show (STree meld leaf)

instance HasSpace leaf => HasSpace (STree meld leaf) where
  type SpaceOf (STree meld leaf) = SpaceOf leaf

instance (HasSpace leaf) => HasSpace (Maybe (STree meld leaf)) where
    type SpaceOf (Maybe (STree meld leaf)) = SpaceOf leaf
-- | Type of melding of compound shapes.
data Compound
    -- | Addition of shapes.
    = CompoundAdd
    -- | Substraction of the first shape from the second.
    | CompoundSubtract
    deriving (Ord, Eq, Show)

-- | Type of overlapping two seperate shapes.
type Overlap = ()

instance HasDefault Compound where
  defaultValue = CompoundAdd

instance HasDefault Overlap where
  defaultValue = ()

-- | An SRep defines an individual shape and it's metadata.
data SRep token textureLabel rep = SRep
  { _sRepToken     :: token
  , _sRepSubstance :: Substance textureLabel (SpaceOf rep)
  , _sRep          :: rep
  }
makeLenses ''SRep

deriving instance (Space (SpaceOf rep), Show token, Show rep, Show textureLabel) => Show (SRep token textureLabel rep)

instance HasSpace rep => HasSpace (SRep token textureLabel rep) where
  type SpaceOf (SRep token textureLabel rep) = SpaceOf rep

instance (HasSpace leaf) => SimpleTransformable (STree o leaf) where
  translateBy delta tree = if delta == zeroPoint then tree else STransform (Translate delta) tree
  scaleBy factor tree = if factor == 1 then tree else STransform (Scale factor) tree
  stretchBy size tree = if size == Point2 1 1 then tree else STransform (Stretch size) tree

instance (HasSpace leaf) => Transformable (STree o leaf) where
  rotateBy angle tree = if angle == (0 @@ rad) then tree else STransform (Rotate angle) tree

instance forall o s leaf .(Space s, s ~ (SpaceOf leaf), CanProject (BezierSpace s) leaf) => CanProject (BezierSpace s) (STree o leaf) where
  projectionWithStepsAccuracy max_steps m_accuracy path tree = STransform (Project path) $ tree

instance (Space s) => CanProject (BezierSpace s) (Shape s) where
  projectionWithStepsAccuracy max_steps m_accuracy path shapes = over shapeOutlines (fmap (projectionWithStepsAccuracy max_steps m_accuracy path)) shapes

instance (Space s) => CanProject (BezierSpace s) (SRep token NamedTexture (CompoundTree s)) where
  projectionWithStepsAccuracy max_steps m_accuracy path (SRep token substance rep) =
      SRep token substance $ projectionWithStepsAccuracy max_steps m_accuracy path rep

-- instance Functor (SRep token) where
--   fmap f (SRep token substance rep) = SRep token substance (f rep)

type CompoundTree s = STree Compound (Shape s)
type ShapeTree token s = STree () (SRep token NamedTexture (CompoundTree s))

-- | A container for a ShapeTree that indicates the background color.
data Scene tree = Scene
  { _sceneBackgroundColor :: Color
  , _sceneShapeTree       :: tree
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
