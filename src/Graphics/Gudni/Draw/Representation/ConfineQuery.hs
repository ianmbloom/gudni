{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}

module Graphics.Gudni.Draw.Representation.ConfineQuery
  ( constructLayerStack
  , checkPoint
  , ring
  --, tracePoint
  , checkBox
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout
import Graphics.Gudni.Raster.Dag.ConfineTree.Type
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.WithTag
import Graphics.Gudni.Raster.Dag.ConfineTree.Query

import Graphics.Gudni.Raster.Dag.Query
import Graphics.Gudni.Raster.Dag.PrimColorQuery
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type
import Graphics.Gudni.Raster.Dag.Primitive.Cross
import Graphics.Gudni.Raster.Dag.State

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Elipse
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.Symbols
import Graphics.Gudni.Draw.Text
import Graphics.Gudni.Draw.Representation.Class
import Graphics.Gudni.Draw.Representation.Primitive

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Subdividable

import Foreign.Storable
import GHC.Exts
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.List
import Linear.V3
import qualified Data.Map as M
import Text.PrettyPrint.GenericPretty

ring :: IsStyle style
     => SpaceOf style
     -> Int
     -> Color
     -> Layout style
ring r layer color =
    let l = fromIntegral layer
    in
    if layer > 0
    then withColor color $ (scaleBy (l * r) . mask $ circle) `subtractFrom` (scaleBy ((l + 1) * r) . mask $ circle)
    else withColor color $ (scaleBy ((l + 1) * r) . mask $ circle)

layerRing :: forall token style
           .  ( IsStyle style
              , Storable (SpaceOf style)
              )
           => M.Map PrimTagId Color
           -> Point2 (SpaceOf style)
           -> Int
           -> PrimTagId
           -> Layout style
layerRing colorMap point depth primTagId =
  let color = transparent 1 $ (M.!) colorMap primTagId
  in  translateBy point $ ring 4 (depth + 1) color

makeList a = [a]

makeColorMap :: ( MonadIO m
                , Space s
                , Storable s
                )
             => PrimStack
             -> FabricMonad s m (M.Map PrimTagId Color)
makeColorMap primStack =
  do primColors <- mapM loadPrimColorS primStack
     return $ M.fromList $ zip primStack primColors

constructLayerStack :: forall style m
                    .  ( IsStyle style
                       , Storable (SpaceOf style)
                       , MonadIO m
                       )
                    => Point2 (SpaceOf style)
                    -> PrimStack
                    -> FabricMonad (SpaceOf style) m (Layout style)
constructLayerStack point stack =
  do  colorMap <- makeColorMap stack
      return $ overlap $ (withColor black . translateBy point $ hatch 1 8):(imap (layerRing colorMap point) $ stack) -- ++ [withColor black . translateBy point . translateByXY 4 (-7.5) . scaleBy 15 $ text]


slopedPrim :: Space s => Primitive s -> Bool
slopedPrim (Prim fabricTagId ty) =
    case ty of
        PrimBezier bez -> bezierSlopeLTEZero Vertical bez
        _ -> False

checkPoint :: forall style m
           .  ( IsStyle style
              , Storable (SpaceOf style)
              , MonadIO m
              )
           => M.Map PrimTagId Color
           -> ConfineTree (SpaceOf style)
           -> DecorateTree (SpaceOf style)
           -> Point2 (SpaceOf style)
           -> FabricMonad (SpaceOf style) m (Layout style)
checkPoint colorMap confineTree decoTree point =
  do  (anchor, anchorStack, stack, primTags) <- queryConfineTreePointWithInfo loadPrimS confineTree decoTree point
      let primList :: [Primitive (SpaceOf style)]
          primList = concat $ map ({-subdivideBeziers 4 .-}  makeList . view tPrim) $ primTags
          numCrossings = length . filter id . map (crossesPrim anchor point) $ primList

          markPrim :: Space s
                   => Point2 s
                   -> Point2 s
                   -> Primitive s
                   -> (Bool, Bool, Bool, Primitive s)
          markPrim anchor point prim = let ch = crossesPrimHorizontal anchor point prim
                                           cv = crossesPrimVertical   anchor point prim
                                           sl = slopedPrim prim
                                       in  (ch, cv, sl, prim)

          colorPrim :: IsStyle style
                    => (Bool, Bool, Bool, Primitive (SpaceOf style))
                    -> Layout style
          colorPrim (ch, cv, sl, prim) =
              let color = case (ch, cv, sl) of
                             (True,  True,  True ) -> red
                             (True,  True,  False) -> light (light red)
                             (True,  False, True ) -> purple
                             (True,  False, False) -> light (light purple)
                             (False, True,  True ) -> blue
                             (False, True,  False) -> light (light blue)
                             (False, False, True ) -> green
                             (False, False, False) -> yellow
              in  withColor color . drawPrim $ prim

          drawMarkedPrim :: IsStyle style
                         => Point2 (SpaceOf style)
                         -> Point2 (SpaceOf style)
                         -> Primitive (SpaceOf style)
                         -> Layout style
          drawMarkedPrim anchor point prim = colorPrim . markPrim anchor point $ prim

          primLayout :: Layout style
          primLayout = overlap $ map (drawMarkedPrim anchor point) primList
          label :: Layout style
          label = if length stack > 0
                  then translateBy point . translateByXY 10 0 . scaleBy 12 . withColor black . blurb . show $ length primList
                  else emptyItem
      layerStack <- constructLayerStack point stack
      return $
          overlap [
                  -- label
                  --,
                  -- constructLayerStack colorMap anchor anchorTags
                  --,
                  layerStack
                  --,
                  -- curvesLayout
                  --,
                  -- withColor (transparent 0.5 white) . mask . stroke 1 . makeOpenCurve $ [line anchor  point]
                  -- ,
                  --  withColor (transparent 0.5 white) . mask . stroke 1 . makeOpenCurve $ [line anchor (interimPoint anchor point)]
                  -- ,
                  --  withColor (transparent 0.5 white) . mask . stroke 1 . makeOpenCurve $ [line (interimPoint anchor point) point]
                  ]

checkBox :: forall style m
         .  ( IsStyle style
            , Storable (SpaceOf style)
            , MonadIO m)
         => M.Map PrimTagId Color
         -> ConfineTree (SpaceOf style)
         -> DecorateTree (SpaceOf style)
         -> Point2 (SpaceOf style)
         -> FabricMonad (SpaceOf style) m (Layout style)
checkBox colorMap confineTree decoTree point =
  do  let end = point + Point2 50 50
          box = Box point end
      (stack, prims) <- queryConfineTreeBox loadPrimS confineTree decoTree box
      let markBez bez = let sl  = bezierSlopeLTEZero Vertical bez
                        in  (sl, bez)

          colorBez :: IsStyle style =>
                      (Bool, Bezier (SpaceOf style))
                      -> Layout style
          colorBez (sl, bez) =
              let color = case sl of
                             (True ) -> dark green
                             (False) -> dark blue
              in  withColor color . mask . stroke 2 . makeOpenCurve . makeList $ bez

          drawPrim :: IsStyle style
                    => Primitive (SpaceOf style)
                    -> Layout style
          drawPrim (Prim fabricTagId ty) =
              case ty of
                  PrimBezier bez -> colorBez . markBez $ bez
                  _ -> error $ "not implemented primitive type " ++ show ty
      return $
          overlap [ -- constructLayerStack colorMap point stack
                    -- ,
                    withColor black . mask . openRectangle 1 $ box
                    ,
                    overlap $ map (drawPrim . view tPrim) prims
                  ]
