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
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Stack
import Graphics.Gudni.Raster.Dag.Fabric.Traverse
import Graphics.Gudni.Raster.Dag.State
import Graphics.Gudni.Raster.Dag.ConfineTree.Traverse

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Text

import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.RayQuery
import Graphics.Gudni.Util.Debug

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
                      => DecorateTree (SpaceOf style)
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
     -> DecoTree axis (SpaceOf style)
     -> DagMonad (SpaceOf style) m (Layout style)
  go axis depth parentCut parentLine layers boundary tree =
      do let anchor = pointAlongAxis (perpendicularTo axis) parentCut parentLine
         layerStack <- error "constructDecorateTree not implemented" -- constructLayerStack anchor layers
         case tree of
             DecoLeaf -> return $ overlap [rectangleAround anchor, layerStack]
             DecoBranch {} -> do branchLayout <- goBranch axis depth parentCut parentLine layers boundary tree
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
           -> DecoTree axis (SpaceOf style)
           -> DagMonad (SpaceOf style) m (Layout style)
  goBranch axis depth parentCut parentLine layers boundary branch =
                  let cut      = branch ^?! decoCut
                      anchor = pointAlongAxis (perpendicularTo axis) parentCut parentLine
                      endPoint = pointAlongAxis axis parentLine cut
                  in
                  if widthOf boundary > 0 && heightOf boundary > 0
                  then
                     do  let layers' = combineItemStacks (branch ^. decoCrossings) layers
                             (lessBound, moreBound) = splitBox axis cut boundary
                         lessBranch <- go (perpendicularTo axis) (depth + 1) parentLine cut layers' lessBound (branch ^?! decoLessCut)
                         moreBranch <- go (perpendicularTo axis) (depth + 1) parentLine cut layers' moreBound (branch ^?! decoMoreCut)
                         let pathLine :: Layout style
                             pathLine  = withColor (transparent 0.2 black) . mask . stroke 1 . makeOpenCurve $ [line anchor endPoint]
                         return $ overlap [ pathLine
                                          , lessBranch
                                          , moreBranch
                                          ]
                  else return emptyItem
