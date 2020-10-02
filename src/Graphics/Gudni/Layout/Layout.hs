{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Collect
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Top level functions for creating bounding box based layouts.

module Graphics.Gudni.Layout.Layout
  ( IsLayout(..)
  , Layout(..)
  , LayoutRep(..)
  , CompoundLayout(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree

import Graphics.Gudni.Layout.Token
import Graphics.Gudni.Layout.Fill
import Graphics.Gudni.Layout.Empty
import Graphics.Gudni.Layout.Alignment
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.Overlappable
import Graphics.Gudni.Layout.Compound
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Linear
import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Control.Applicative
import Control.Monad

data LayoutRep style
  = Glyph
  { _layGlyphStyle :: style
  , _layGlyph      :: CodePoint
  }
  | LayoutShape
  { _layShape :: Shape (SpaceOf style)
  }
makeLenses ''LayoutRep

deriving instance (Show style, Show (SpaceOf style)) => Show (LayoutRep style)

data CompoundLayout style =
    CompoundLayout {
        _compoundLayout :: TransTree (ProximityMeld style Compound) (Maybe (LayoutRep style))
    }
makeLenses ''CompoundLayout

deriving instance ( Show style
                  , IsStyle style
                  ) => Show (CompoundLayout style)

data Layout style =
     Layout {
        _layout :: TransTree (ProximityMeld style Overlap) (Maybe (SRep (TokenOf style) NamedTexture (CompoundLayout style)))
     }
makeLenses ''Layout

deriving instance ( Show style
                  , Show (TokenOf style)
                  , IsStyle style
                  ) => Show (Layout style)

instance (IsStyle style) => HasSpace (LayoutRep style) where
  type SpaceOf (LayoutRep style) = SpaceOf style

instance (IsStyle style) => HasSpace (Layout style) where
  type SpaceOf (Layout style) = SpaceOf style

instance (IsStyle style) => HasSpace (CompoundLayout style) where
  type SpaceOf (CompoundLayout style) = SpaceOf style

instance (IsStyle style) => CanFill (Layout style) where
  type UnFilled (Layout style) = CompoundLayout style
  withFill substance = Layout . SLeaf . SItem . Just . SRep Nothing substance

class (HasStyle t, HasMeld t, HasEmpty t) => IsLayout t where
  type UnPlaced t :: *
  place   :: UnPlaced t -> t
  nextTo  :: ToEitherAxis axis => axis -> StyleOf t -> Maybe Alignment -> Meld t -> t -> t -> t
  onTopOf :: StyleOf t -> Maybe Alignment -> Maybe Alignment -> Meld t -> t -> t -> t

overLayouts f (Layout a) (Layout b) = Layout (f a b)

instance HasDefault style => Overlappable (Layout style) where
  combine = overLayouts (SMeld defaultValue)

instance HasStyle (Layout style) where
  type StyleOf (Layout style) = style

instance HasMeld (Layout style) where
  type Meld (Layout style) = Overlap

instance HasEmpty (Layout style) where
  emptyItem = Layout (SLeaf . SItem $ Nothing)
  isEmpty (Layout (SLeaf (SItem Nothing))) = True
  isEmpty _ = False

instance IsStyle style => HasToken (Layout style) where
  type TokenOf (Layout style) = TokenOf style

instance (IsStyle style, Show (TokenOf style)) => Tokenized (Layout style) where
  overToken fToken (Layout tree) = Layout . mapSItem (fmap (over sRepToken fToken)) $ tree

instance (IsStyle style) => IsLayout (Layout style) where
  type UnPlaced (Layout style) = ShapeTree (TokenOf style) (SpaceOf style)
  place = liftToLayoutTree
  nextTo axis style alignment         meld = overLayouts (SMeld (ProximityMeld (NextTo (eitherAxis axis) alignment) style meld))
  onTopOf style mAlignVert mAlignHori meld = overLayouts (SMeld (ProximityMeld (OnTopOf mAlignVert mAlignHori)      style meld))

liftToLayoutTree :: (IsStyle style) => ShapeTree (TokenOf style) (SpaceOf style) -> Layout style
liftToLayoutTree (ShapeTree tree) = Layout . mapBranchMeld (ProximityMeld defaultValue defaultValue) . mapSItem (fmap (overSRep liftToCompoundLayoutTree)) $ tree

liftCompoundLayout f (CompoundLayout a) (CompoundLayout b) = CompoundLayout (f a b)

instance (IsStyle style) => Compoundable (CompoundLayout style) where
  addOver      = liftCompoundLayout addOver
  subtractFrom = liftCompoundLayout subtractFrom -- the subtracted shape must be above what is being subtracted in the stack.

instance HasStyle (CompoundLayout style) where
  type StyleOf (CompoundLayout style) = style

instance HasMeld (CompoundLayout style) where
  type Meld (CompoundLayout style) = Compound

instance HasEmpty (CompoundLayout style) where
  emptyItem = CompoundLayout (SLeaf . SItem $ Nothing)
  isEmpty (CompoundLayout (SLeaf (SItem Nothing))) = True
  isEmpty _ = False

instance HasDefault style => Overlappable (CompoundLayout style) where
  combine = liftCompoundLayout (SMeld defaultValue)

instance (IsStyle style) => IsLayout (CompoundLayout style) where
  type UnPlaced (CompoundLayout style) = CompoundTree (SpaceOf style)
  place = liftToCompoundLayoutTree
  nextTo  axis style alignment  meld       = liftCompoundLayout $ SMeld (ProximityMeld (NextTo (eitherAxis axis) alignment) style meld)
  onTopOf style mAlignVert mAlignHori meld = liftCompoundLayout $ SMeld (ProximityMeld (OnTopOf mAlignVert mAlignHori)      style meld)

liftToCompoundLayoutTree :: HasDefault style => CompoundTree (SpaceOf style) -> CompoundLayout style
liftToCompoundLayoutTree (CompoundTree tree) = CompoundLayout . mapBranchMeld (ProximityMeld defaultValue defaultValue) . mapSItem (fmap LayoutShape) $ tree

instance (IsStyle style) =>
         Compoundable (STree (BranchTree_ (ProximityMeld style Compound) tag item)) where
  addOver      = SMeld (ProximityMeld defaultValue defaultValue CompoundAdd     )
  subtractFrom = SMeld (ProximityMeld defaultValue defaultValue CompoundSubtract) -- the subtracted shape must be above what is being subtracted in the stack.

instance (IsStyle style) => SimpleTransformable (Layout style) where
  translateBy p = over layout (translateBy p)
  stretchBy   p = over layout (stretchBy   p)
  simpleTransformWith t = over layout (simpleTransformWith t)

instance (IsStyle style) => Transformable (Layout style) where
  rotateBy   a = over layout (rotateBy a)
  transformWith t = over layout (transformWith t)

instance (IsStyle style) => Projectable (Layout style) where
  projectOnto path = over layout (projectOnto path)

instance (IsStyle style) => SimpleTransformable (CompoundLayout style) where
  translateBy p = over compoundLayout (translateBy p)
  stretchBy   p = over compoundLayout (stretchBy   p)
  simpleTransformWith t = over compoundLayout (simpleTransformWith t)

instance (IsStyle style) => Transformable (CompoundLayout style) where
  rotateBy   a = over compoundLayout (rotateBy a)
  transformWith t = over compoundLayout (transformWith t)

instance (IsStyle style) => Projectable (CompoundLayout style) where
  projectOnto path = over compoundLayout (projectOnto path)
