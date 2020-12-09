{-# LANGUAGE GADTs         #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Fabric.Transformer.Type
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Possible ways of altering the path of a ray through the scene dag.

module Graphics.Gudni.Raster.Dag.Fabric.Transformer.Type
  ( FTransformer(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

data FTransformer s where
    FAffine   :: Affine s -> Affine s -> FTransformer s
    FFacet    :: Facet s  -> FTransformer s
    FConvolve :: s        -> FTransformer s
    deriving (Generic)

instance Show s => Show (FTransformer s) where
    show trans =
        case trans of
            FAffine   a b -> "FAffine"
            FFacet    f   -> "FFacet"
            FConvolve  s  -> "FConvolve"

instance (Out s) => Out (FTransformer s) where
  doc trans =
     case trans of
       FAffine forward _ -> doc forward
       FFacet  f         -> doc f
       FConvolve s       -> text "FConvolve" <+> doc s
