{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TypeFamilies        #-}

module Graphics.Gudni.Draw.Representation.Primitive
  ( drawPrim
  , labelPrim
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Draw.Rectangle
import Graphics.Gudni.Draw.ArrowHead
import Graphics.Gudni.Draw.Text

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Linear.V3

makeList :: a -> [a]
makeList = pure

v3ToList :: V3 a -> [a]
v3ToList (V3 a b c) = [a, b, c]

drawPrim :: IsStyle style
         => Primitive (SpaceOf style)
         -> CompoundLayout style
drawPrim (Prim fabricTagId ty) =
    case ty of
        PrimBezier  bez   -> mask . stroke 0.1 . makeOpenCurve . makeList $ bez
        PrimFacet   facet -> mask . stroke 0.1 . makeOpenCurve . v3ToList . facetToBeziers $ facet
        PrimRect    box   -> mask . boxToRectangle $ box
        PrimEllipse box   -> mask . boxToRectangle $ box

labelPrim :: IsStyle style => PrimTagId -> Primitive (SpaceOf style) -> CompoundLayout style
labelPrim primTagId (Prim _ ty) =
    let size  = 40
        text  = blurb (show primTagId)
        pos   = case ty of
                    PrimBezier  bez   -> eval 0.5 bez
                    PrimFacet   facet -> centerBox . boxOf $ facet
                    PrimRect    box   -> centerBox box
                    PrimEllipse box   -> centerBox box
    in  translateBy pos .
        scaleBy size $
        text
