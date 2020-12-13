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

module Graphics.Gudni.Raster.Fabric.Type
  ( FabricType(..)
  , Fabric(..)
  , showFabricHead
  , FLeaf(..)
  , FTreeLeaf(..)
  , FStacker(..)
  , fabricDepth
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.WithBox

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.Fabric.Combine.Type
import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Transformer.Type
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Slice

import Control.Lens

class HasSpace i => FabricType i where
    type FChildType  i :: *
    type FBinaryType i :: *
    type FPostType   i :: *
    type FPreType    i :: *
    type FLeafType   i :: *

data Fabric i where
    FBinary    :: FBinaryType i -> FChildType i -> FChildType i -> Fabric i
    FUnaryPost :: FPostType   i -> FChildType i                 -> Fabric i
    FUnaryPre  :: FPreType    i -> FChildType i                 -> Fabric i
    FLeaf      :: FLeafType   i                                 -> Fabric i

data FLeaf i where
    FShape     :: WithBox (Shape (SpaceOf i)) -> FLeaf i
    FSubstance :: FSubstance i                -> FLeaf i

data FTreeLeaf i where
    FTree          :: DecoTagId (SpaceOf i) -> ConfineTagId (SpaceOf i) -> FTreeLeaf i
    FTreeSubstance :: FSubstance i                                      -> FTreeLeaf i

data FStacker = FStacker FabricTagId

showFabricHead :: ( Show (FBinaryType i)
                  , Show (FPostType   i)
                  , Show (FPreType    i)
                  , Show (FLeafType   i)
                  )
               => Fabric i
               -> String
showFabricHead fabric =
    case fabric of
      FBinary    ty _ _ -> "FBinary   " ++ show ty
      FUnaryPost ty _   -> "FUnaryPost" ++ show ty
      FUnaryPre  ty _   -> "FUnaryPre " ++ show ty
      FLeaf      ty     -> "FLeaf     " ++ show ty


deriving instance ( Show (SpaceOf i)
                  , Show (FChildType  i)
                  , Show (FBinaryType i)
                  , Show (FPostType   i)
                  , Show (FPreType    i)
                  , Show (FLeafType   i)
                  ) => Show (Fabric i)

deriving instance ( Show (SpaceOf      i)
                  , Show (FChildType   i)
                  , Show (FQuery       i)
                  , Show (FTex         i)
                  , Show (WithBox (Shape (SpaceOf i)))
                  ) => Show (FLeaf i)

deriving instance ( Show (SpaceOf      i)
                  , Show (FChildType   i)
                  , Show (FQuery       i)
                  , Show (FTex         i)
                  ) => Show (FTreeLeaf i)


instance ( Out (SpaceOf     i)
         , Out (FChildType  i)
         , Out (FBinaryType i)
         , Out (FPostType   i)
         , Out (FPreType    i)
         , Out (FLeafType   i)
         ) => Out (Fabric i) where
    doc tree =
        case tree of
            FBinary ty a b ->
                 text "FBinary" <+> doc ty
                 $$
                 nest 4 ( doc a )
                 $$
                 nest 4 ( doc b )
            FUnaryPost post child ->
                 text "FUnaryPost" <+> doc post
                 $$
                 nest 4 ( doc child )
            FUnaryPre pre child ->
                 text "FUnaryPre" <+> doc pre
                 $$
                 nest 4 ( doc child )
            FLeaf leaf ->
                 doc leaf
    docPrec _ = doc

instance Out FStacker where
    doc (FStacker tagId) = text "Stacker" <+> doc tagId
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
          FTree treeId decoId ->
               text "FTree" <+> text (show treeId) <+> text (show decoId)
          FTreeSubstance substance ->
               text "FTreeSubstance" <+> doc substance
    docPrec _ = doc

fabricDepth :: (FChildType i~Fabric i) => Fabric i -> Int
fabricDepth fabric =
    case fabric of
        FBinary _ above below -> max (fabricDepth above) (fabricDepth below) + 1
        FUnaryPost t child -> fabricDepth child + 1
        FUnaryPre  t child -> fabricDepth child + 1
        FLeaf leaf -> 1
