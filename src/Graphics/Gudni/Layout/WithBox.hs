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
-- Module      :  Graphics.Gudni.Layout.WithBox
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Data Type for tracking bounding boxes.

module Graphics.Gudni.Layout.WithBox
  ( WithBox(..)
  , withItem
  , withBox
  , meldWithBox
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Layout.Empty
import Control.Lens
import Control.Applicative
import Graphics.Gudni.Util.Util

data WithBox a
   = WithBox { _withItem :: a
             , _withBox :: Box (SpaceOf a)
             }
makeLenses ''WithBox

instance SimpleTransformable a => SimpleTransformable (WithBox a) where
  translateBy delta  (WithBox item box) = WithBox (translateBy delta item) (translateBox delta box)
  stretchBy   size   (WithBox item box) = WithBox (stretchBy size item) (stretchBox size box)

instance HasSpace a => HasSpace (WithBox a) where
  type SpaceOf (WithBox a) = SpaceOf a

meldWithBox :: ( HasSpace (Leaf i)
               , CanBox (Leaf i)
               )
            => Meld i
            -> ( WithBox (STree i)
               , WithBox (STree i)
               )
            -> WithBox (STree i)
meldWithBox meld (WithBox aTree aBox, WithBox bTree bBox) = WithBox (SMeld meld aTree bTree) (minMaxBox aBox bBox)
