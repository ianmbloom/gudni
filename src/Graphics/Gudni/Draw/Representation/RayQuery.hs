
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Draw.Representation.RayQuery
  ( constructRayQuery
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Answer
import Graphics.Gudni.Raster.Dag.Fabric.Traverse

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

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

ring :: IsStyle style
     => SpaceOf style
     -> Int
     -> CompoundLayout style
ring r layer =
    let l = fromIntegral layer
    in
    if layer > 0
    then (scaleBy (l * r) . mask $ circle) `subtractFrom` (scaleBy ((l + 1) * r) . mask $ circle)
    else (scaleBy ((l + 1) * r) . mask $ circle)

layerRing :: IsStyle style
          => Int
          -> Color (SpaceOf style)
          -> Layout style
layerRing depth color = withColor color $ ring 16 (depth + 1)

ringStack :: IsStyle style
          => Point2 (SpaceOf style)
          -> [Color (SpaceOf style)]
          -> Layout style
ringStack point stack =
      translateBy point .
      overlap $ (withColor black $ hatch 1 8):
                imap layerRing stack

constructRayQuery :: forall style m
                  .  ( IsStyle style
                     , MonadIO m
                     )
                  => FabricTagId
                  -> Point2 (SpaceOf style)
                  -> RayMonad (SpaceOf style) m (Layout style)
constructRayQuery root point =
  do (ColorStack stack) <- traverseFabric point root :: RayMonad (SpaceOf style) m (ColorStack (SpaceOf style))
     return $ ringStack point $ reverse stack