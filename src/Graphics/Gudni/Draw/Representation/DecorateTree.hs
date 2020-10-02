{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE TypeFamilies #-}


module Graphics.Gudni.Draw.Representation.DecorateTree
  ( constructDecorateTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.ItemInfo
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Raster.ConfineTree.TaggedBezier
import Graphics.Gudni.Raster.ConfineTree.ItemStack
import Graphics.Gudni.Raster.ConfineTree.Traverse

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Text

import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.ConfineQuery
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Monad
-- import Data.Maybe
-- import Data.List
import qualified Data.Map as M

reasonableBoundaries :: Space s => Box s
reasonableBoundaries =
  let highValue = 2 ^ 12
      highPoint  = pure highValue
  in  Box (negate highPoint) highPoint

rectangleAround :: IsStyle style => Point2 (SpaceOf style) -> Layout style
rectangleAround point =
  let size = 16
  in  withColor black . translateBy point . translateBy (pure (-size/2)) . mask $ openRectangle 2 (pointToBox (pure size))

constructDecorateTree :: forall style
                      .  (IsStyle style)
                      => M.Map ItemTagId Color
                      -> DecorateTree (SpaceOf style)
                      -> Layout style
constructDecorateTree colorMap =
  go Vertical 0 (toAlong Horizontal minBound) (toAlong Vertical minBound) [] reasonableBoundaries
  where
  go :: ( Axis axis
        , axis~PerpendicularTo(PerpendicularTo axis))
     => axis
     -> Int
     -> Athwart axis (SpaceOf style)
     -> Along   axis (SpaceOf style)
     -> ItemStack
     -> Box (SpaceOf style)
     -> DecoTree axis (SpaceOf style)
     -> Layout style
  go axis depth parentCut parentLine layers boundary tree =
        let anchor = pointAlongAxis (perpendicularTo axis) parentCut parentLine
        in
        overlap
        [ constructLayerStack colorMap anchor layers
        , rectangleAround anchor
        , case tree of
              DecoLeaf  -> emptyItem
              DecoBranch {} ->
                  goBranch axis depth parentCut parentLine layers boundary tree
        ]
  goBranch :: ( Axis axis
              , axis~PerpendicularTo(PerpendicularTo axis)
              )
           => axis
           -> Int
           -> Athwart axis (SpaceOf style)
           -> Along   axis (SpaceOf style)
           -> ItemStack
           -> Box (SpaceOf style)
           -> DecoTree axis (SpaceOf style)
           -> Layout style
  goBranch axis depth parentCut parentLine layers boundary branch =
                  let cut      = branch ^?! decoCut
                      anchor = pointAlongAxis (perpendicularTo axis) parentCut parentLine
                      endPoint = pointAlongAxis axis parentLine cut
                      cornerString = (show $ branch ^?! decoCurveTag) ++{- "->" ++ (show $ branch ^. confineCrossedCurves) ++-} " ?> "  -- (show end)
                      cornerText :: Layout style
                      cornerText = translateBy endPoint . rotateBy (15 @@ deg) . translateByXY 10 0 . scaleBy 20 . withColor blue . blurb $ cornerString
                  in
                  if widthOf boundary > 0 && heightOf boundary > 0
                  then
                     let layers' = combineItemStacks (branch ^. decoCrossings) layers
                         (lessBound, moreBound) = splitBox axis cut boundary
                         lessBranch = go (perpendicularTo axis) (depth + 1) parentLine cut layers' lessBound (branch ^?! decoLessCut)
                         moreBranch = go (perpendicularTo axis) (depth + 1) parentLine cut layers' moreBound (branch ^?! decoMoreCut)
                         pathLine :: Layout style
                         pathLine  = withColor (transparent 0.2 black) . mask . stroke 1 . makeOpenCurve $ [line anchor endPoint]
                     in  overlap [ pathLine
                                 , lessBranch
                                 , moreBranch
                                 ]
                  else emptyItem
