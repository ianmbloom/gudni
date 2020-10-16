{-# LANGUAGE UndecidableInstances #-} -- Show (SpaceOf leaf)
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
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

module Graphics.Gudni.Raster.Dag.Fabric
  ( FabricType(..)
  , Fabric(..)
  , FCombineType(..)
  , FTransformer(..)
  , FFilter(..)
  , FSubstance(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Serial.Slice

class HasSpace i => FabricType i where
    type FChild    i :: *
    type FTex      i :: *
    type FGeometry i :: *
    type FQuery    i :: *
    type FCombiner i :: *

data Fabric i where
    FCombine   :: FCombiner i              -> FChild i -> FChild i -> Fabric i
    FTransform :: FTransformer (SpaceOf i) -> FChild i             -> Fabric i
    FLeaf      :: FSubstance i                                     -> Fabric i

data FCombineType
    = FComposite
    | FMask -- Mask , Multiply and FloatAnd are the same
    | FAdd
    | FFloatOr  -- x + y - (x*y)
    | FFloatXor -- x + y - 2(x*y)
     --      | RGTE
     --      | RGT
    deriving (Show)

data FSubstance i where
     FGeometry  :: FGeometry i -> FSubstance i
     FConst     :: FQuery i    -> FSubstance i
     FTexture   :: FTex   i    -> FSubstance i
     FLinear    ::                FSubstance i
     FQuadrance ::                FSubstance i

deriving instance (Show (FGeometry i), Show (FQuery i), Show (FTex i)) => Show (FSubstance i)

data FTransformer s where
     FAffineRay :: Affine s -> FTransformer s
     FFacet     :: Facet s  -> FTransformer s
     FFilter    :: FFilter  -> FTransformer s
     FConvolve  :: s        -> FTransformer s
     deriving (Show)

data FFilter where
     FSqrt     :: FFilter
     FInvert   :: FFilter -- (1-x)
     FCos      :: FFilter
     FSin      :: FFilter
     FSaturate :: FFilter
     deriving (Enum, Show)
