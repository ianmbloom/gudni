{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Graphics.Gudni.Raster.ConfineTree.Primitive.WithinBox
  ( primIsWithinBox
  , primsWithinBox
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type

primIsWithinBox :: Space s => s -> Box s -> Primitive s -> Bool
primIsWithinBox limit boundary (Prim fabricTagId ty) =
  case ty of
      PrimBezier  bez   -> bezierIsWithinBox limit boundary bez
      PrimFacet   facet -> facetIsWithinBox  limit boundary facet
      PrimRect    box   -> includesBox       boundary box
      PrimEllipse box   -> includesBox       boundary box

primsWithinBox :: Space s => s -> Box s -> Primitive s -> [Primitive s]
primsWithinBox limit boundary prim@(Prim fabricTagId ty) =
    case ty of
        PrimBezier bez -> map (Prim fabricTagId . PrimBezier) $ bezierWithinBox limit boundary bez
        _ -> if primIsWithinBox limit boundary prim
             then [prim]
             else []
