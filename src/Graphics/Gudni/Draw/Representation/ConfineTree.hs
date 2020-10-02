{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE TypeFamilies #-}


module Graphics.Gudni.Draw.Representation.ConfineTree
  ( constructConfineTree
  , constructConfine
  , reasonableBoundaries
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ConfineTree.Traverse

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Text
import Graphics.Gudni.Draw.ArrowHead
import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.ConfineQuery
import Graphics.Gudni.Util.Debug

import GHC.Exts
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List
import Text.PrettyPrint.GenericPretty
import qualified Data.Map as M

class (Axis axis) => AxisColor axis where
  axisColor :: axis -> Color

instance AxisColor Vertical where
  axisColor Vertical   = red

instance AxisColor Horizontal where
  axisColor Horizontal = blue

reasonableBoundaries :: Space s => Box s
reasonableBoundaries =
  let highValue = 2 ^ 12
      highPoint  = pure highValue
  in  Box (negate highPoint) highPoint

overlapBlock :: (Axis axis, Space s) => axis -> Athwart axis s -> Athwart axis s -> Box s -> Box s
overlapBlock axis cut overlap boundary =
    let minPoint = pointAlongAxis axis (boundary ^. minBox . along axis) cut
        maxPoint = pointAlongAxis axis (boundary ^. maxBox . along axis) overlap
    in Box minPoint maxPoint

axisLine :: (Axis axis, Space s) => axis -> Athwart axis s -> Box s -> Bezier s
axisLine axis cut boundary =
    let minPoint = pointAlongAxis axis (boundary ^. minBox . along axis) cut
        maxPoint = pointAlongAxis axis (boundary ^. maxBox . along axis) cut
    in  line (constrainPoint minPoint) (constrainPoint maxPoint)

bezierArrow :: (IsStyle style) => Bezier (SpaceOf style) -> Layout style
bezierArrow bz@(Bez v0 c v1) =
     let r = 5
         w = 10
         h = 7.5
     in
     overlap [ withColor black . place . withArrowHead (Point2 w h) PointingForward $ bz
             , withColor black . mask . stroke 1 $ makeOpenCurve [bz]
             ]

constructConfine :: forall axis style
                 .  ( IsStyle style
                    , AxisColor axis
                    )
                 => axis
                 -> M.Map ItemTagId Color
                 -> Confine axis (SpaceOf style)
                 -> Box (SpaceOf style)
                 -> Layout style
constructConfine axis colorMap tree boundary =
    let thickness :: SpaceOf style
        thickness = 1
        cut       = tree ^. confineCut
        overhang  = tree ^. confineOverhang
        aColor    = axisColor axis
        aLine :: Bezier (SpaceOf style)
        aLine = axisLine axis cut boundary
        axisLayout :: Layout style
        axisLayout = withColor (transparent 0.2 aColor) . mask . stroke thickness . makeOpenCurve $ [aLine]
        overhangBox :: Box (SpaceOf style)
        overhangBox = overlapBlock axis cut overhang boundary
        overhangLayout :: Layout style
        overhangLayout = withColor (transparent 0.01 aColor) . mask . boxToRectangle $ overhangBox
        curve :: Layout style
        curve = bezierArrow $ (tree ^. confineCurve . tBez)
        text = blurb (show $ tree ^. confineCurve . tCurveTag)
        label :: Layout style
        label = translateBy (eval 0.5 (tree ^. confineCurve . tBez)) .
                scaleBy 40 .
                withColor (transparent 0.5 purple) $
                text
    in  overlap $ [
                  -- label
                  -- ,
                  -- curve
                  -- ,
                  axisLayout
                  -- ,
                  -- overhangLayout
                  ]

constructConfineTree :: forall style
                     .  (IsStyle style)
                     => M.Map ItemTagId Color
                     -> ConfineTree (SpaceOf style)
                     -> Layout style
constructConfineTree colorMap =
  go Vertical 0 reasonableBoundaries
  where
  go :: ( Axis axis
        , AxisColor axis
        , AxisColor (PerpendicularTo axis)
        , axis~PerpendicularTo(PerpendicularTo axis))
     => axis
     -> Int
     -> Box (SpaceOf style)
     -> Branch axis (SpaceOf style)
     -> Layout style
  go axis depth boundary mTree =
        if widthOf boundary > 0 && heightOf boundary > 0
        then case mTree of
               Nothing   -> emptyItem
               Just tree ->
                      let confine = constructConfine axis colorMap tree boundary
                          cut     = tree ^. confineCut
                          (lessBound, moreBound) = splitBox axis cut boundary
                          lessBranch = go (perpendicularTo axis) (depth + 1) lessBound (tree ^. confineLessCut)
                          moreBranch = go (perpendicularTo axis) (depth + 1) moreBound (tree ^. confineMoreCut)
                      in  overlap [confine, lessBranch, moreBranch]
        else emptyItem
