{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Draw.Representation.Class
  ( HasRepresentation(..)
  , openCircle
  , closedCircle
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Draw.ArrowHead
import Graphics.Gudni.Draw.Elipse
import Graphics.Gudni.Draw.Stroke
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Debug
import Linear.V2
import Linear.V3
import Control.Monad
import Control.Applicative
import Control.Lens

import Graphics.Gudni.Layout.Combinators

mkLine :: Space s => V2 (Point2 s) -> Bezier s
mkLine (V2 a b) = line a b

openCircle :: forall style . (IsStyle style) => SpaceOf style -> Layout Mono style
openCircle r = (scaleBy (r/2) . place $ circle) `subtractFrom` (scaleBy r . place $ circle)

closedCircle :: forall style . (IsStyle style) => SpaceOf style -> Layout Mono style
closedCircle r =  scaleBy r . place $ circle

class (HasSpace t) => HasRepresentation t where
    represent :: (IsStyle style, SpaceOf t ~ SpaceOf style) => Bool -> t -> Layout Rgba style

instance (Space s) => HasRepresentation (Bezier s) where
    represent _ bz@(Bez v0 c v1) =
        let r = 5
            w = 10
            h = 7.5
        in  overlap [ --withColor black . place . withArrowHead (Point2 w h) PointingForward $ bz
                      withColor black . withArrowHead (Point2 w h) PointingForward $ bz
                    --, withColor red   $ translateBy v0 $ closedCircle r
                    , withColor (light blue) . translateBy  c . openCircle $ r
                    --, withColor red   $ translateBy v1 $ closedCircle r
                    , withColor black . place . stroke 1 $ makeOpenCurve [bz]
                    ]

instance (Alternative f, Monad f, Foldable f, Space s) => HasRepresentation (Outline_ f s) where
    represent dk = overlap . fmap (represent dk) . (if dk then join . fmap (replaceKnob Vertical) else id) . view outlineSegments

instance (Functor f, Foldable f, Space s) => HasRepresentation (OpenCurve_ f s) where
    represent dk = overlap . fmap (represent dk) .  view curveSegments

instance (Space s) => HasRepresentation (Shape s) where
    represent dk = overlap . fmap (overlap . fmap (represent dk) . view outlineSegments) . view shapeOutlines

instance (Space s) => HasRepresentation (Facet s) where
    represent dk = overlap . fmap (represent dk) . facetToBeziers
