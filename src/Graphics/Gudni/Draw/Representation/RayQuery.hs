
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gudni.Draw.Representation.RayQuery
  ( constructRayQuery
  , constructRayColor
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.Fabric.Ray.Class
import Graphics.Gudni.Raster.Fabric.Ray.Answer
import Graphics.Gudni.Raster.Fabric.Traverse

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
                  => SpaceOf style
                  -> Point2 (SpaceOf style)
                  -> RayMonad (SpaceOf style) m (Layout style)
constructRayQuery limit point =
  do (ColorStack stack) <- traverseFabric limit point (Point2 0 0) :: RayMonad (SpaceOf style) m (ColorStack (SpaceOf style))
     return $ ringStack point $ reverse stack

sizeCircle :: Space s => s
sizeCircle = 10

constructRayColor :: forall style m
                  .  ( IsStyle style
                     , MonadIO m
                     )
                  => SpaceOf style
                  -> Point2 (SpaceOf style)
                  -> RayMonad (SpaceOf style) m (Layout style)
constructRayColor limit point =
  do (color :: Color (SpaceOf style)) <- traverseFabric limit point (Point2 0 0)
     return $ translateBy point $
              overlap [ withColor black $ hatch 1 4
                      , withColor color . scaleBy sizeCircle . mask $ circle
                      , withColor black $ (scaleBy sizeCircle . mask $ circle) `subtractFrom` (scaleBy (sizeCircle + 1) . mask $ circle)
                      ]
