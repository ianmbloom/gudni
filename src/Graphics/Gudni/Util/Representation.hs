{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Util.Representation
  ( HasRepresentation(..)
  , openCircle
  , closedCircle
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Util.FlattenTree
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Debug
import Linear.V2
import Linear.V3
import Control.Monad
import Control.Applicative
import Control.Lens

mkLine :: Space s => V2 (Point2 s) -> Bezier s
mkLine (V2 a b) = line a b

openCircle :: forall token s .  (Space s) => s -> CompoundTree s
openCircle r = (scaleBy (r/2) . mask $ circle) `subtractFrom` (scaleBy r . mask $ circle)

closedCircle :: forall token s . Space s => s -> CompoundTree s
closedCircle r =  scaleBy r . mask $ circle

class (HasSpace t) => HasRepresentation t where
    represent :: (HasDefault token) => Bool -> t -> ShapeTree token (SpaceOf t)

instance forall s . (Space s) => HasRepresentation (Bezier s) where
    represent _ bz@(Bez v0 c v1) =
        let r = 5
            w = 10
            h = 7.5
        in  overlap [ -- (withColor black . mask . withArrowHead (Point2 w h) PointingForward $ bz) :: ShapeTree token s
                    --, withColor red   $ translateBy v0 $ closedCircle r
                    withColor (light blue) . translateBy  c . openCircle $ r
                    --, withColor red   $ translateBy v1 $ closedCircle r
                    , withColor black . mask . stroke 1 $ makeOpenCurve [bz]
                    ]

instance (Alternative f, Monad f, Foldable f, Space s) => HasRepresentation (Outline_ f s) where
    represent dk = overlap . fmap (represent dk) . (if dk then join . fmap (replaceKnob verticalAxis) else id) . view outlineSegments

instance (Functor f, Foldable f, Space s) => HasRepresentation (OpenCurve_ f s) where
    represent dk = overlap . fmap (represent dk) .  view curveSegments

instance (Space s) => HasRepresentation (Shape s) where
    represent dk = overlap . fmap (overlap . fmap (represent dk) . view outlineSegments) . view shapeOutlines

instance HasEmpty (ShapeTree token s) where
   emptyItem = ShapeTree . SLeaf . SItem $ Nothing
   isEmpty (ShapeTree (SLeaf (SItem Nothing))) = True
   isEmpty _ = False

instance HasEmpty (CompoundTree s) where
   emptyItem = CompoundTree . SLeaf . SItem $ Nothing
   isEmpty (CompoundTree (SLeaf (SItem Nothing))) = True
   isEmpty _ = False

instance (Space s, Show token) => HasRepresentation (FinalTree token s) where
    represent dk tree = overlap . fmap (represent dk) . flatten $ tree

instance (Space s) => HasRepresentation (FacetSide s) where
    represent dk = represent dk . view sceneSide

instance (Space s) => HasRepresentation (Facet_ s) where
    represent dk = overlap . fmap (represent dk) . view facetSides


{-
instance (Space s, Space t, s~t) => HasRepresentation (HardFacet_ s t) where
  represent hardFacet@(HardFacet face texture) =
    let faceRep = withColor (light blue) overlap . fmap (
    overlap (overlap (fmap
-}
