{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric        #-}

module Graphics.Gudni.Raster.ConfineTree.Primitive.Type
  ( Primitive(..)
  , primFabricTagId
  , primType
  , PrimType(..)
  , PrimTag(..)
  , PrimTagId(..)
  , BezierId(..)
  , FacetId(..)
  , BoxId(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Primitive.Constants

import Graphics.Gudni.Raster.Serial.Reference

import Control.Lens
import Foreign.Storable
import Foreign.Ptr

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

newtype PrimTag    = PrimTag   {unPrimTag   :: PrimTag_            } deriving (Eq, Ord, Generic)
newtype PrimTagId  = PrimTagId {unPrimTagId :: Reference PrimTag   } deriving (Eq, Ord, Generic)
newtype BezierId s = BezierId  {unBezierId  :: Reference (Bezier s)} deriving (Eq, Ord, Generic, Show )
newtype FacetId  s = FacetId   {unFacetId   :: Reference (Facet  s)} deriving (Eq, Ord, Generic, Show )
newtype BoxId    s = BoxId     {unBoxId     :: Reference (Box    s)} deriving (Eq, Ord, Generic, Show )

instance Show PrimTagId where
  show = show . unRef . unPrimTagId

instance Out PrimTag
instance Out PrimTagId where
    doc tagId = doc . unRef . unPrimTagId $ tagId
    docPrec _ = doc
instance Out s => Out (BezierId s)
instance Out s => Out (FacetId  s)
instance Out s => Out (BoxId  s)

instance Out s => Out (Primitive s) where
    doc (Prim shapeId ty) = text "Prim" <+> doc shapeId <+> doc ty
instance Out s => Out (PrimType s) where
    doc prim = case prim of
                    PrimBezier  bez   -> doc bez
                    PrimFacet   facet -> doc facet
                    PrimRect    rect  -> doc rect
                    PrimEllipse box   -> doc box
    docPrec _ = doc

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

instance Storable PrimTagId where
  sizeOf    (PrimTagId i) = sizeOf    (undefined :: Reference PrimTag )
  alignment (PrimTagId i) = alignment (undefined :: Reference PrimTag )
  peek ptr =  PrimTagId <$>  peek (castPtr ptr)
  poke ptr   (PrimTagId i) = poke (castPtr ptr) i

instance Storable PrimTag where
  sizeOf    (PrimTag i) = sizeOf    (undefined :: PrimTag_ )
  alignment (PrimTag i) = alignment (undefined :: PrimTag_ )
  peek ptr =  PrimTag <$>  peek (castPtr ptr)
  poke ptr   (PrimTag i) = poke (castPtr ptr) i

instance Storable s => Storable (BezierId s) where
  sizeOf    (BezierId i) = sizeOf    (undefined :: Reference (Bezier s) )
  alignment (BezierId i) = alignment (undefined :: Reference (Bezier s) )
  peek ptr = BezierId <$>  peek (castPtr ptr)
  poke ptr  (BezierId i) = poke (castPtr ptr) i

instance Storable s => Storable (FacetId  s) where
  sizeOf    (FacetId i) = sizeOf    (undefined :: Reference (Facet s) )
  alignment (FacetId i) = alignment (undefined :: Reference (Facet s) )
  peek ptr = FacetId <$>  peek (castPtr ptr)
  poke ptr  (FacetId i) = poke (castPtr ptr) i

instance Storable s => Storable (BoxId  s) where
  sizeOf    (BoxId i) = sizeOf    (undefined :: Reference (Box s) )
  alignment (BoxId i) = alignment (undefined :: Reference (Box s) )
  peek ptr = BoxId <$>  peek (castPtr ptr)
  poke ptr  (BoxId i) = poke (castPtr ptr) i
