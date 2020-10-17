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
-- Module      :  Graphics.Gudni.Raster.ShapeTree
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A ShapeTree is the main input data structure for the Gudni Rasterizer. A client program
-- generates a Scene which contains a ShapeTree for each frame that they wish to render.

module Graphics.Gudni.ShapeTree.STree
  ( HasMeld(..)
  , TreeType(..)
  , TagTreeType(..)
  , STree(..)
  , SBranch(..)
  , SMask(..)
  , sMaskToken
  , sMaskSubstance
  , sMask
  , overSRep
  , addBranch
  , liftShapeTree
  , liftCompoundTree
  , Compound (..)
  , ShapeTree_(..)
  , ShapeTree(..)
  , overShapeTree
  , FullShapeTree(..)
  , CompoundTree_(..)
  , CompoundTree(..)
  , overCompoundTree
  , FullCompoundTree(..)
  , BranchTree_(..)
  , BranchTree(..)
  , TransTree_(..)
  , TransTree(..)
  , Tree_(..)
  , Tree(..)
  , Scene(..)
  , sceneBackgroundColor
  , sceneShapeTree
  , HasDefault(..)
  , invertCompound
  , Overlap(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Shape
import Graphics.Gudni.Figure.Substance
import Graphics.Gudni.Figure.Transform

import Graphics.Gudni.Base.Chain

import Control.Lens

import Data.Vector as V

class HasMeld i where
    type Meld i :: *

class HasSpace (Leaf i) => TreeType i where
    type Leaf i :: *

class (HasSpace (Item i), TreeType i) => TagTreeType i where
    type Tag  i :: *
    type Item i :: *

-- | Polymorphic data structure for trees of shapes. The meld type is the operations for combining two subtrees,
-- the trans type defines transformations that can be applied across to subtree and the leaf type the component elements of
-- a tree. The ShapeTree type is an STree that melds with Overlap whose component type is an SMask which itself contains
-- an STree that melds with Compound, and whose component type is RawShape.
data STree i where
     SMeld :: Meld i -> STree i -> STree i -> STree i
     SLeaf :: Leaf i -> STree i
deriving instance (Show (Meld i), Show (Leaf i)) => Show (STree i)

instance HasSpace (Leaf i) => HasSpace (STree i) where
  type SpaceOf (STree i) = SpaceOf (Leaf i)

data SBranch i where
     SBranch :: Tag i -> STree i -> SBranch i
     SItem   :: Item i -> SBranch i
deriving instance (Show (Meld i), Show (Leaf i), Show (Tag i), Show (Item i)) => Show (SBranch i)

instance HasSpace (Item i) => HasSpace (SBranch i) where
  type SpaceOf (SBranch i) = SpaceOf (Item i)

data BranchTree_ meld tag item

instance HasMeld (BranchTree_ meld tag item) where
  type Meld (BranchTree_ meld tag item) = meld

instance (HasSpace item) => TreeType (BranchTree_ meld tag item) where
  type Leaf (BranchTree_ meld tag item) = SBranch (BranchTree_ meld tag item)

instance (HasSpace item) => TagTreeType (BranchTree_ meld tag item) where
  type Tag  (BranchTree_ meld tag item) = tag
  type Item (BranchTree_ meld tag item) = item

type TransTree_ meld item = BranchTree_ meld (Transformer (SpaceOf item)) item

type CompoundTree_ s = TransTree_ Compound (Maybe (Shape s))
type ShapeTree_ token tex s = TransTree_ Overlap (Maybe (SMask token tex (CompoundTree s)))

newtype CompoundTree s = CompoundTree (STree (CompoundTree_ s))
overCompoundTree f (CompoundTree x) = CompoundTree (f x)

newtype ShapeTree token s = ShapeTree (STree (ShapeTree_ token NamedTexture s))
overShapeTree f (ShapeTree x) = ShapeTree (f x)

type TransTree meld item = STree (TransTree_ meld item)

type BranchTree meld tag item = STree (BranchTree_ meld tag item)

type FullCompoundTree s = TransTree Compound (Shape s)
type FullShapeTree token tex s = TransTree Overlap (SMask token tex (FullCompoundTree s))


data Tree_ meld leaf

type Tree meld leaf = STree (Tree_ meld leaf)

instance HasMeld (Tree_ meld item) where
  type Meld (Tree_ meld item) = meld

instance (HasSpace leaf) => TreeType (Tree_ meld leaf) where
  type Leaf (Tree_ meld leaf) = leaf

instance Space s => HasSpace (ShapeTree token s) where
  type SpaceOf (ShapeTree token s) = s

instance Space s => HasSpace (CompoundTree s) where
  type SpaceOf (CompoundTree s) = s

overSRep f (SMask token tex rep) = SMask token tex (f rep)

addBranch :: (Leaf i~SBranch i) => Tag i -> STree i -> STree i
addBranch tag = SLeaf . SBranch tag

instance (HasSpace leaf, tag~Transformer(SpaceOf leaf)) => SimpleTransformable (STree (BranchTree_ meld tag leaf)) where
  translateBy delta tree = if delta == zeroPoint then tree else addBranch (Simple $ Translate delta) tree
  stretchBy size tree    = if size == Point2 1 1 then tree else addBranch (Simple $ Stretch size) tree
  simpleTransformWith t = addBranch (Simple t)

instance (HasSpace leaf,tag~Transformer(SpaceOf leaf)) => Transformable (STree (BranchTree_ meld tag leaf)) where
  rotateBy angle tree = if angle == (0 @@ rad) then tree else addBranch (Rotate angle) tree
  transformWith t = addBranch t

instance (HasSpace leaf, tag~Transformer(SpaceOf leaf)) => Projectable (STree (BranchTree_ meld tag leaf)) where
  projectOnto path = addBranch (Project path)

instance (Space s) => SimpleTransformable (ShapeTree token s) where
  translateBy p = overShapeTree (translateBy p)
  stretchBy   p = overShapeTree (stretchBy   p)
  simpleTransformWith t = overShapeTree (simpleTransformWith t)

instance (Space s) => Transformable (ShapeTree token s) where
  rotateBy   a = overShapeTree (rotateBy a)
  transformWith t = overShapeTree (transformWith t)

instance (Space s) => Projectable (ShapeTree token s) where
  projectOnto path = overShapeTree (projectOnto path)

instance (Space s) => SimpleTransformable (CompoundTree s) where
  translateBy p = overCompoundTree (translateBy p)
  stretchBy   p = overCompoundTree (stretchBy   p)
  simpleTransformWith t = overCompoundTree (simpleTransformWith t)

instance (Space s) => Transformable (CompoundTree s) where
  rotateBy   a = overCompoundTree (rotateBy a)
  transformWith t = overCompoundTree (transformWith t)

instance (Space s) => Projectable (CompoundTree s) where
  projectOnto path = overCompoundTree (projectOnto path)

liftShapeTree f (ShapeTree a) (ShapeTree b) = ShapeTree $ f a b
liftCompoundTree f (CompoundTree a) (CompoundTree b) = CompoundTree $ f a b


-- | Type of melding of compound shapes.
data Compound
    -- | Addition of shapes.
    = CompoundAdd
    -- | Substraction of the first shape from the second.
    | CompoundSubtract
    deriving (Ord, Eq, Show)

-- Invert a compound type
invertCompound :: Compound -> Compound
invertCompound compound =
    case compound of
        CompoundAdd      -> CompoundSubtract
        CompoundSubtract -> CompoundAdd

-- | Type of overlapping two seperate shapes.
data Overlap = Overlap deriving (Show, Eq)

instance HasDefault Compound where
  defaultValue = CompoundAdd

instance HasDefault Overlap where
  defaultValue = Overlap

-- | An SMask defines an individual shape and it's metadata.
data SMask token tex rep = SMask
  { _sMaskToken     :: Maybe token
  , _sMaskSubstance :: Substance tex (SpaceOf rep)  -- rep
  , _sMask          :: rep
  }
makeLenses ''SMask

instance CanBox rep => CanBox (SMask token tex rep) where
  boxOf = boxOf . view sMask

deriving instance (Space (SpaceOf rep), Show token, Show rep, Show tex) => Show (SMask token tex rep)

instance HasSpace rep => HasSpace (SMask token tex rep) where
  type SpaceOf (SMask token tex rep) = SpaceOf rep

-- | A container for a ShapeTree that indicates the background color.
data Scene t = Scene
  { _sceneBackgroundColor :: Color
  , _sceneShapeTree       :: t
  }
makeLenses ''Scene
deriving instance Show t => Show (Scene t)
