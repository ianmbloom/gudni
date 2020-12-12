{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Cross
  ( rectIncludesPoint
  , ellipseIncludesPoint
  , crossesRectAlong
  , crossesEllipseAlong
  , crossesPrimAlong
  , crossesPrim
  , crossesPrimHorizontal
  , crossesPrimVertical
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Facet
-- import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Type
-- import Graphics.Gudni.Raster.Dag.ConfineTree.Type

import Linear.Metric
import Control.Applicative
import Control.Lens

rectIncludesPoint :: Ord s
                  => Box s
                  -> Point2 s
                  -> Bool
rectIncludesPoint boundary p =
       p ^. pX  >  boundary ^. maxBox . pX
    || p ^. pY  >  boundary ^. maxBox . pY
    || p ^. pX  <= boundary ^. minBox . pX
    || p ^. pY  <= boundary ^. minBox . pY

ellipseIncludesPoint :: ( Space s
                        )
                     => Box s
                     -> Point2 s
                     -> Bool
ellipseIncludesPoint boundary p =
  let center = centerBox boundary
      size   = sizeBox boundary
      adjustedDist = liftA2 (/) (p ^-^ center) (size ^/ 2)
  in  size ^. pX > 0 && size ^. pY > 0 && rectIncludesPoint boundary p && (quadrance adjustedDist >= 4)

crossesRectAlong :: ( Axis axis
                    , Space s
                    )
                 => axis
                 -> Along axis s
                 -> Athwart axis s
                 -> Along axis s
                 -> Box s
                 -> Bool
crossesRectAlong axis start baseline end rect =
  let s = pointAlongAxis axis start baseline
      e = pointAlongAxis axis end   baseline
  in  rectIncludesPoint rect s /=
      rectIncludesPoint rect e

crossesEllipseAlong :: ( Axis axis
                       , Space s
                       )
                    => axis
                    -> Along axis s
                    -> Athwart axis s
                    -> Along axis s
                    -> Box s
                    -> Bool
crossesEllipseAlong axis start baseline end rect =
  let s = pointAlongAxis axis start baseline
      e = pointAlongAxis axis end   baseline
  in  ellipseIncludesPoint rect s /=
      ellipseIncludesPoint rect e

crossesPrimAlong :: ( Axis axis
                    , Space s)
                 => s
                 -> axis
                 -> Along axis s
                 -> Athwart axis s
                 -> Along axis s
                 -> Primitive s
                 -> Bool
crossesPrimAlong limit axis start baseline end prim =
  case prim ^. primType of
      PrimBezier  bez   -> crossesBezierAlong  limit axis start baseline end bez
      PrimFacet   facet -> crossesFacetAlong   limit axis start baseline end facet
      PrimRect    box   -> crossesRectAlong          axis start baseline end box
      PrimEllipse box   -> crossesEllipseAlong       axis start baseline end box

crossesPrim :: (Space s) => s -> Point2 s -> Point2 s -> Primitive s -> Bool
crossesPrim limit start end prim =
    let iP = interimPoint start end
    in
    crossesPrimAlong limit Vertical   (start ^. pY) (start ^. pX) (iP  ^. pY) prim /=
    crossesPrimAlong limit Horizontal (iP    ^. pX) (iP    ^. pY) (end ^. pX) prim

crossesPrimHorizontal :: (Space s) => s -> Point2 s -> Point2 s -> Primitive s -> Bool
crossesPrimHorizontal limit start end prim =
     crossesPrimAlong limit Horizontal (start ^. pX) (end ^. pY) (end ^. pX) prim

crossesPrimVertical :: (Space s) => s -> Point2 s -> Point2 s -> Primitive s -> Bool
crossesPrimVertical limit start end prim =
   crossesPrimAlong limit Vertical   (start ^. pY) (start ^. pX) (end  ^. pY) prim