--{-# LANGUAGE UndecidableInstances  #-} -- Show (SpaceOf leaf)
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

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
-- A ShapeTree is the main input data structure for the Gudni RasterState. A client program
-- generates a Scene which contains a ShapeTree for each frame that they wish to render.

module Graphics.Gudni.Raster.Dag.Fabric.Type
  ( FabricType(..)
  , Fabric(..)
  , FLeaf(..)
  , FTreeLeaf(..)
  , ForStorage(..)
  , fabricDepth
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.WithBox

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Transformer
import Graphics.Gudni.Raster.Dag.ConfineTree.Tag
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice

import Control.Lens

class HasSpace i => FabricType i where
    type FRootType      i :: *
    type FChildType     i :: *
    type FLeafType      i :: *
    type FCombinerType  i :: *

data Fabric i where
    FCombine   :: FCombinerType i          -> FChildType i -> FChildType i -> Fabric i
    FTransform :: FTransformer (SpaceOf i) -> FChildType i                 -> Fabric i
    FLeaf      :: FLeafType i                                              -> Fabric i

data FLeaf i where
    FShape     :: WithBox (Shape (SpaceOf i)) -> FLeaf i
    FSubstance :: FSubstance i                -> FLeaf i

data FTreeLeaf i where
    FTree          :: Reference (TreeRoot (SpaceOf i)) -> FabricTagId -> FTreeLeaf i
    FTreeSubstance :: FSubstance i                                    -> FTreeLeaf i

data ForStorage s

instance Space s => HasSpace (ForStorage s) where
    type SpaceOf       (ForStorage s) = s

instance Space s => FabricType (ForStorage s) where
    type FRootType     (ForStorage s) = FabricTagId
    type FChildType    (ForStorage s) = FabricTagId
    type FLeafType     (ForStorage s) = FTreeLeaf (ForStorage s)
    type FCombinerType (ForStorage s) = (FCombineType, ShapeId)

deriving instance ( Show (SpaceOf i)
                  , Show (FChildType     i)
                  , Show (FLeafType      i)
                  , Show (FCombinerType  i)
                  ) => Show (Fabric i)

deriving instance ( Show (SpaceOf     i)
                  , Show (FChildType  i)
                  , Show (FQuery      i)
                  , Show (FTex        i)
                  ) => Show (FTreeLeaf i)

instance ( Out (SpaceOf       i)
         , Out (FRootType     i)
         , Out (FChildType    i)
         , Out (FLeafType     i)
         , Out (FCombinerType i)
         ) => Out (Fabric i) where
    doc tree =
        case tree of
            FCombine ty a b ->
                 text "FCombine" <+> doc ty
                 $$
                 nest 4 ( doc a )
                 $$
                 nest 4 ( doc b )
            FTransform trans child ->
                 text "FTrans" <+> doc trans
                 $$
                 nest 4 ( doc child )
            FLeaf leaf ->
                 doc leaf
    docPrec _ = doc

instance (Chain f, Out s) => ( Out (Outline_ f s )) where
    doc outline = brackets (foldl1 (<+>) (fmap doc (outline ^. outlineSegments)))
    docPrec _ = doc

instance (Chain f, Out s) => Out (Shape_ f s) where
    doc shape = text "Shape " <+> foldl1 (<+>) (fmap doc (shape ^. shapeOutlines))
    docPrec _ = doc

instance ( Out (FSubstance i)
         ) => Out (FLeaf i) where
    doc tree =
      case tree of
          FShape shape ->
               text "FShape" <+> text "Shape X"
          FSubstance substance ->
               text "FSubstance" <+> doc substance
    docPrec _ = doc

instance ( Out (FSubstance i)
         ) => Out (FTreeLeaf i) where
    doc tree =
      case tree of
          FTree treeId child ->
               text "FTree" <+> text (show treeId) $$ doc child
          FTreeSubstance substance ->
               text "FTreeSubstance" <+> doc substance
    docPrec _ = doc

fabricDepth :: (FChildType i~Fabric i) => Fabric i -> Int
fabricDepth fabric =
    case fabric of
        FCombine ty above below -> max (fabricDepth above) (fabricDepth below) + 1
        FTransform t child -> fabricDepth child + 1
        FLeaf leaf -> 1
