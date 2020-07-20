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

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.MaybeBox
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Data Type for tracking bounding boxes.

module Graphics.Gudni.Layout.MaybeBox
  ( MaybeBox(..)
  , maybeItem
  , maybeBox
  , WithBox(..)
  , withItem
  , withBox
  , withBoxToMaybeBox
  , meldWithBox
  , collapseMaybeBox
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Layout.Empty
import Control.Lens
import Control.Applicative
import Graphics.Gudni.Util.Util

data MaybeBox a
   = MaybeBox { _maybeItem :: a
              , _maybeBox  :: Maybe (Box (SpaceOf a))
              }
makeLenses ''MaybeBox

instance HasSpace a => HasSpace (MaybeBox a) where
  type SpaceOf (MaybeBox a) = SpaceOf a

data WithBox a
   = WithBox { _withItem :: a
             , _withBox :: Box (SpaceOf a)
             }
makeLenses ''WithBox

withBoxToMaybeBox :: WithBox a -> MaybeBox a
withBoxToMaybeBox (WithBox item box) = MaybeBox item (Just box)

instance HasSpace a => HasSpace (WithBox a) where
  type SpaceOf (WithBox a) = SpaceOf a

simpleTransformWithBox :: (a -> a) -> (Box (SpaceOf a) -> Box (SpaceOf a)) -> WithBox a -> WithBox a
simpleTransformWithBox f b = over withBox b . over withItem f

instance (SimpleTransformable a) => SimpleTransformable (WithBox a) where
    translateBy p = simpleTransformWithBox (translateBy p) (translateBy p)
    scaleBy     s = simpleTransformWithBox (scaleBy     s) (scaleBy     s)
    stretchBy   p = simpleTransformWithBox (stretchBy   p) (stretchBy   p)

simpleTransformMaybeBox :: (a -> a) -> (Box (SpaceOf a) -> Box (SpaceOf a)) -> MaybeBox a -> MaybeBox a
simpleTransformMaybeBox f b = over maybeBox (fmap b) . over maybeItem f

instance (SimpleTransformable a) => SimpleTransformable (MaybeBox a) where
    translateBy p = simpleTransformMaybeBox (translateBy p) (translateBy p)
    scaleBy     s = simpleTransformMaybeBox (scaleBy     s) (scaleBy     s)
    stretchBy   p = simpleTransformMaybeBox (stretchBy   p) (stretchBy   p)

transformMaybeBox f = over maybeItem f . set maybeBox Nothing

instance (Transformable a) => Transformable (MaybeBox a) where
    rotateBy a = transformMaybeBox (rotateBy a)

instance ( s~SpaceOf a
         , CanProject (BezierSpace s) a)
         => CanProject (BezierSpace s) (MaybeBox a) where
    projectionWithStepsAccuracy debug max_steps m_accuracy bSpace =
      transformMaybeBox (projectionWithStepsAccuracy debug max_steps m_accuracy bSpace)

collapseMaybeBox :: forall i
                . ( HasBox (Leaf i)
                  , CanProject (BezierSpace (SpaceOf (Leaf i))) (Leaf i)
                  , Transformable (Leaf i)
                  )
                => MaybeBox (STree i)
                -> WithBox (STree i)
collapseMaybeBox tree =
    let collapsedTree = collapseTransformations (tree ^. maybeItem)
        go :: STree meld leaf -> Box (SpaceOf leaf)
        go tree =
           case tree of
             STransform t child -> error "should not contain transform nodes."
             SMeld meld a b -> minMaxBox (go a) (go b)
             SLeaf leaf -> boxOf leaf
        box = go collapsedTree
    in  WithBox collapsedTree box

meldWithBox :: ( HasSpace leaf
               , HasBox leaf
               )
            => meld
            -> ( WithBox (STree meld leaf)
               , WithBox (STree meld leaf)
               )
            -> WithBox (STree meld leaf)
meldWithBox meld (WithBox aTree aBox, WithBox bTree bBox) = WithBox (SMeld meld aTree bTree) (minMaxBox aBox bBox)
