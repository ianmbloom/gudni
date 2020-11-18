{-# LANGUAGE GADTs         #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Fabric.Ray.Transformer
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
--
-- Possible ways of altering the path of a ray through the dag.

module Graphics.Gudni.Raster.Dag.Fabric.Ray.Transformer
  ( FTransformer(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Filter

data FTransformer s where
    FAffine   :: Affine s -> Affine s -> FTransformer s
    FFacet    :: Facet s  -> FTransformer s
    FFilter   :: FFilter  -> FTransformer s
    FConvolve :: s        -> FTransformer s
    deriving (Generic)

instance Show s => Show (FTransformer s) where
    show trans =
        case trans of
            FAffine   a b -> "FAffine"
            FFacet    f   -> "FFacet"
            FFilter   f   -> "FFilter " ++ show f
            FConvolve  s  -> "FConvolve"

instance (Out s) => Out (FTransformer s) where
  doc trans =
     case trans of
       FAffine forward _ -> doc forward
       FFacet  f         -> doc f
       FFilter f         -> doc f
       FConvolve s       -> text "FConvolve" <+> doc s
