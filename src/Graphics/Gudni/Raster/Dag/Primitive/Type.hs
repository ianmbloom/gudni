{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric        #-}

module Graphics.Gudni.Raster.Dag.Primitive.Type
  ( Primitive(..)
  , primFabricTagId
  , primType
  , PrimType(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Fabric.Type

import Control.Lens

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

newtype PrimId = PrimId {unPrimId :: Int} deriving (Eq, Ord)

data PrimType s
    = PrimBezier  (Bezier s)
    | PrimFacet   (Facet  s)
    | PrimRect    (Box s)
    | PrimEllipse (Box s)
    deriving (Show, Generic)

data Primitive s = Prim
  { _primFabricTagId :: FabricTagId
  , _primType        :: PrimType s
  } deriving (Show, Generic)
makeLenses ''Primitive

instance Out s => Out (PrimType  s)
instance Out s => Out (Primitive s)

instance Space s => HasSpace (Primitive s) where
    type SpaceOf (Primitive s) = s

instance Space s => CanBox (Primitive s) where
    boxOf = boxOf . view primType

instance Space s => HasSpace (PrimType s) where
    type SpaceOf (PrimType s) = s

instance Space s => CanBox (PrimType s) where
    boxOf item =
        case item of
            PrimBezier  bez   -> boxOf bez
            PrimFacet   facet -> boxOf facet
            PrimRect    box   -> box
            PrimEllipse box   -> box
