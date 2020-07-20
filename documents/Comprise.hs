{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Gudni.Layout.Adjacent
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Top level functions for creating bounding box based layouts.

module Graphics.Gudni.Layout.Comprise
  ( Comprised(..)
  )
where

import Graphics.Gudni.Figure

class (Show (ComprisedOf t), Show (CompriseInfo t), HasSpace (ComprisedOf t), Show t)
      => Comprised t where
    type ComprisedOf  t :: *
    type CompriseInfo t :: *
    form :: CompriseInfo t -> ComprisedOf t -> t

instance (Show meld, Space (SpaceOf leaf), Comprised leaf) => Comprised (STree meld leaf) where
    type ComprisedOf (STree meld leaf) = ComprisedOf leaf
    type CompriseInfo (STree meld leaf) = CompriseInfo leaf
    form info leaf = SLeaf (form info leaf)

instance forall token textureLabel rep . (Show rep, HasSpace rep, Show token, Show textureLabel) => Comprised (SRep token textureLabel rep) where
    type ComprisedOf  (SRep token textureLabel rep) = rep
    type CompriseInfo (SRep token textureLabel rep) = (Maybe token, Substance textureLabel (SpaceOf rep))
    form (mToken, substance) rep = SRep mToken substance rep

instance (Space s, Show (f (Bezier s))) => Comprised (Shape_ f s) where
    type ComprisedOf (Shape_ f s)  = [Outline_ f s]
    type CompriseInfo (Shape_ f s) = ()
    form () outlines = Shape outlines
