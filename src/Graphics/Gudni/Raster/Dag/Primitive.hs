{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Raster.Dag.Primitive
  ( Primitive(..)
  , primToFabricTagId
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree
import Graphics.Gudni.Raster.Dag.Fabric
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Thresholds.SubstanceInfo

newtype PrimId = PrimId {unPrimId :: Int} deriving (Eq, Ord)

data Primitive s
    = PrimBezier FabricTagId (Bezier s)
    | PrimFacet  FabricTagId (Facet  s)
    -- | PrimBox    (Box s)
    -- | PrimElipse (Box s)
    deriving (Show)

primToFabricTagId prim =
    case prim of
        PrimBezier fabricTagId _ -> fabricTagId
        PrimFacet  fabricTagId _ -> fabricTagId

instance Space s => HasSpace (Primitive s) where
    type SpaceOf (Primitive s) = s

instance Space s => CanBox (Primitive s) where
    boxOf item =
        case item of
            PrimBezier child bez   -> boxOf bez
            PrimFacet  child facet -> boxOf facet
            -- PrimBox box -> box
            -- PrimElipse box -> box
