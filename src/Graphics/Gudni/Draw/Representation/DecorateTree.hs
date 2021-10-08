{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Graphics.Gudni.Draw.Representation.DecorateTree
  ( constructDecorateTree
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.ConfineTree.Storage
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.ConfineTree.Primitive.Stack
import Graphics.Gudni.Raster.Dag.Fabric.Traverse
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.ConfineTree.Traverse

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Text

import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.RayQuery
import Graphics.Gudni.Util.Debug

import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.Slice

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import Foreign.Storable

reasonableBoundaries :: Space s => Box s
reasonableBoundaries =
  let highValue = 2 ^ 12
      highPoint  = pure highValue
  in  Box (negate highPoint) highPoint

rectangleAround :: IsStyle style => Point2 (SpaceOf style) -> Layout style
rectangleAround point =
  let size = 16
  in  withColor black . translateBy point . translateBy (pure (-size/2)) . mask $ openRectangle 2 (sizeToBox (pure size))

constructDecorateTree :: forall style m
                      .  ( IsStyle style
                         , MonadIO m
                         , Storable (SpaceOf style)
                         )
                      => DecoTagId (SpaceOf style)
                      -> DagMonad (SpaceOf style) m (Layout style)
constructDecorateTree =
  go Vertical 0 (toAlong Horizontal minBound) (toAlong Vertical minBound) [] reasonableBoundaries
  where
  go :: ( Axis axis
        , axis~PerpendicularTo(PerpendicularTo axis))
     => axis
     -> Int
     -> Athwart axis (SpaceOf style)
     -> Along   axis (SpaceOf style)
     -> ShapeStack
     -> Box (SpaceOf style)
     -> DecoTagId (SpaceOf style)
     -> DagMonad (SpaceOf style) m (Layout style)
  go axis depth parentCut parentLine layers boundary treeId =
      do let anchor = pointAlongAxis (perpendicularTo axis) parentCut parentLine
         layerStack <- error "constructDecorateTree not implemented" -- constructLayerStack anchor layers
         if treeId == nullDecoTagId
         then return $ overlap [rectangleAround anchor, layerStack]
         else do tree <- inTree $ loadDecoTag treeId
                 branchLayout <- goBranch axis depth parentCut parentLine layers boundary tree
                 return $ overlap [rectangleAround anchor, layerStack, branchLayout]

  goBranch :: ( Axis axis
              , axis~PerpendicularTo(PerpendicularTo axis)
              )
           => axis
           -> Int
           -> Athwart axis (SpaceOf style)
           -> Along   axis (SpaceOf style)
           -> ShapeStack
           -> Box (SpaceOf style)
           -> DecoTag (SpaceOf style)
           -> DagMonad (SpaceOf style) m (Layout style)
  goBranch axis depth parentCut parentLine layers boundary tree =
                  let cut      = toAthwart axis (tree ^. decoTagCut)
                      anchor   = pointAlongAxis (perpendicularTo axis) parentCut parentLine
                      endPoint = pointAlongAxis axis parentLine cut
                  in
                  if widthOf boundary > 0 && heightOf boundary > 0
                  then
                     do  newStack <- mapSliceM (fromPileS (dagTreeStorage . treeCrossingPile)) (tree ^. decoTagCrossings)
                         let layers' = combineShapeStacks newStack layers
                             (lessBound, moreBound) = splitBox axis cut boundary
                         lessBranch <- go (perpendicularTo axis) (depth + 1) parentLine cut layers' lessBound (tree ^. decoTagLessCut)
                         moreBranch <- go (perpendicularTo axis) (depth + 1) parentLine cut layers' moreBound (tree ^. decoTagMoreCut)
                         let pathLine :: Layout style
                             pathLine  = withColor (transparent 0.2 black) . mask . stroke 1 . makeOpenCurve $ [line anchor endPoint]
                         return $ overlap [ pathLine
                                          , lessBranch
                                          , moreBranch
                                          ]
                  else return emptyItem
