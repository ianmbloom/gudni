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
  ( TreeType(..)
  , TagTreeType(..)
  , STree(..)
  , SBranch(..)
  , SRep(..)
  , sRepToken
  , sRepSubstance
  , sRep
  , Compound (..)
  , ShapeTree_(..)
  , ShapeTree(..)
  , CompoundTree_(..)
  , CompoundTree(..)
  , Scene(..)
  , sceneBackgroundColor
  , sceneShapeTree
  , HasDefault(..)
  , invertCompound
  , Overlap(..)
  )
where

import Graphics.Gudni.Figure.HasDefault
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Shape
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Angle
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Substance
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.BezierSpace

import Graphics.Gudni.Util.Chain

import Control.Lens
import Control.DeepSeq

import Data.Vector as V

class HasSpace (Leaf i) => TreeType i where
    type Meld i :: *
    type Leaf i :: *

class (HasSpace (Item i), TreeType i) => TagTreeType i where
    type Tag  i :: *
    type Item i :: *

-- | Polymorphic data structure for trees of shapes. The meld type is the operations for combining two subtrees,
-- the trans type defines transformations that can be applied across to subtree and the leaf type the component elements of
-- a tree. The ShapeTree type is an STree that melds with Overlap whose component type is an SRep which itself contains
-- an STree that melds with Compound, and whose component type is RawShape.
data STree i where
     SMeld :: Meld i -> STree i -> STree i -> STree i
     SLeaf :: Leaf i -> STree i
deriving instance (Show (Meld i), Show (Tag i), Show (Leaf i)) => Show (STree i)

instance HasSpace (Leaf i) => HasSpace (STree i) where
  type SpaceOf (STree i) = SpaceOf (Leaf i)

data SBranch i where
     SBranch :: Tag i -> STree i -> SBranch i
     SItem   :: Item i -> SBranch i

instance HasSpace (Item i) => HasSpace (SBranch i) where
  type SpaceOf (SBranch i) = SpaceOf (Item i)

data CompoundTree_ s

instance (Space s) => TreeType (CompoundTree_ s) where
  type Meld (CompoundTree_ s) = Compound
  type Leaf (CompoundTree_ s) = SBranch (CompoundTree_ s)

instance (Space s) => TagTreeType (CompoundTree_ s) where
  type Tag  (CompoundTree_ s) = Transformer s
  type Item (CompoundTree_ s) = Maybe (Shape s)

type CompoundTree s = STree (CompoundTree_ s)

data ShapeTree_ token textureLabel s

instance (Space s) => TreeType (ShapeTree_ token textureLabel s) where
    type Meld (ShapeTree_ token textureLabel s) = Overlap
    type Leaf (ShapeTree_ token textureLabel s) = SBranch (ShapeTree_ token textureLabel s)

instance (Space s) => TagTreeType (ShapeTree_ token textureLabel s) where
    type Tag  (ShapeTree_ token textureLabel s) = Transformer s
    type Item (ShapeTree_ token textureLabel s) = Maybe (SRep token textureLabel (CompoundTree s))

type ShapeTree token textureLabel s = STree (ShapeTree_ token textureLabel s)

addBranch :: (Leaf i~SBranch i) => Tag i -> STree i -> STree i
addBranch tag = SLeaf . SBranch tag

instance (HasSpace (Leaf i), Tag i~Transformer (SpaceOf (Leaf i)), Leaf i~SBranch i)
         => SimpleTransformable (STree i) where
  translateBy delta tree = if delta == zeroPoint then tree else addBranch (Translate delta) tree
  scaleBy factor tree    = if factor == 1 then tree else        addBranch (Scale factor) tree
  stretchBy size tree    = if size == Point2 1 1 then tree else addBranch (Stretch size) tree

instance (HasSpace (Leaf i), Tag i~Transformer (SpaceOf (Leaf i)), Leaf i~SBranch i)
         => Transformable (STree i) where
  rotateBy angle tree = if angle == (0 @@ rad) then tree else addBranch (Rotate angle) tree

instance (HasSpace (Leaf i), Tag i~Transformer (SpaceOf (Leaf i)), Leaf i~SBranch i, s~SpaceOf(Leaf i))
         => CanProject (BezierSpace s) (STree i) where
  projectionWithStepsAccuracy debug max_steps m_accuracy path = addBranch (Project debug path)

-- | Type of melding of compound shapes.
data Compound
    -- | Addition of shapes.
    = CompoundAdd
    -- | Substraction of the first shape from the second.
    | CompoundSubtract
    deriving (Ord, Eq, Show)

-- Invert a compound type
invertCompound :: Compound -> Compound
invertCompound combineType =
    case combineType of
        CompoundAdd      -> CompoundSubtract
        CompoundSubtract -> CompoundAdd

instance NFData Compound where
  rnf _ = ()

-- | Type of overlapping two seperate shapes.
data Overlap = Overlap deriving (Show, Eq)

instance HasDefault Compound where
  defaultValue = CompoundAdd

instance HasDefault Overlap where
  defaultValue = Overlap

-- | An SRep defines an individual shape and it's metadata.
data SRep token textureLabel rep = SRep
  { _sRepToken     :: Maybe token
  , _sRepSubstance :: Substance textureLabel (SpaceOf rep)
  , _sRep          :: rep
  }
makeLenses ''SRep

instance HasBox rep => HasBox (SRep token tex rep) where
  boxOf = boxOf . view sRep

deriving instance (Space (SpaceOf rep), Show token, Show rep, Show textureLabel) => Show (SRep token textureLabel rep)

instance HasSpace rep => HasSpace (SRep token textureLabel rep) where
  type SpaceOf (SRep token textureLabel rep) = SpaceOf rep

instance (SimpleTransformable rep) => SimpleTransformable (SRep token texture rep) where
    translateBy p = over sRep (translateBy p)
    scaleBy     s = over sRep (scaleBy s)
    stretchBy   p = over sRep (stretchBy p)
instance (Transformable rep) => Transformable (SRep token texture rep) where
    rotateBy    a = over sRep (rotateBy a)

-- | A container for a ShapeTree that indicates the background color.
data Scene i = Scene
  { _sceneBackgroundColor :: Color
  , _sceneShapeTree       :: Maybe (STree i)
  }
makeLenses ''Scene
deriving instance Show (STree i) => Show (Scene i)
