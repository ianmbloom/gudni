{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Graphics.Gudni.Util.Representation
  ( HasRepresentation(..)
  , openCircle
  , closedCircle
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Debug
import Linear.V2
import Linear.V3
import Control.Monad
import Control.Applicative

mkLine :: Space s => V2 (Point2 s) -> Bezier s
mkLine (V2 a b) = line a b

openCircle :: forall s . Space s => s -> CompoundTree s
openCircle r = mask . strokeOffset 0 (r/2) . scaleBy r $ (circle :: Outline s)
closedCircle :: forall s . Space s => s -> CompoundTree s
closedCircle r = mask . Shape . pure . scaleBy r $ (circle :: Outline s)

class (HasSpace t) => HasRepresentation t where
    represent :: Bool -> t -> ShapeTree Int (SpaceOf t)

instance Space s => HasRepresentation (Bezier s) where
    represent _ bz@(Bez v0 c v1) =
        let r = 5 in
        overlap [ colorWith red   $ translateBy v0 $ closedCircle r
                , colorWith blue  $ translateBy  c $ openCircle r
                , colorWith red   $ translateBy v1 $ closedCircle r
                , colorWith black $ mask . stroke 1 $ bz
                ]

instance (Alternative f, Monad f, Foldable f, Space s) => HasRepresentation (Outline_ f s) where
    represent dk (Outline bs) = overlap . fmap (represent dk) . (if dk then join . fmap deKnob else id) $ bs

instance (Functor f, Foldable f, Space s) => HasRepresentation (OpenCurve_ f s) where
    represent dk (OpenCurve bs) = overlap . fmap (represent dk) $ bs

instance Space s => HasRepresentation (Shape s) where
    represent dk (Shape outlines) = overlap . fmap (represent dk) $ outlines

instance (Space s, Show token) => HasRepresentation (ShapeTree token s) where
    represent dk tree = overlap . fmap (represent dk) . flattenShapeTree $ tree


instance (Space s, Space t, s~t) => HasRepresentation (FacetSide s t) where
    represent dk facetSide@(FacetSide sceneSide textureSide) =
        overlap [represent dk sceneSide, colorWith green . mask . stroke 0.25 . mkLine $ textureSide]

instance (Space s, Space t, s~t) => HasRepresentation (Facet_ s t) where
    represent dk facet@(Facet sides) = overlap . fmap (represent dk) $ sides



{-
instance (Space s, Space t, s~t) => HasRepresentation (HardFacet_ s t) where
  represent hardFacet@(HardFacet face texture) =
    let faceRep = colorWith (light blue) overlap . fmap (
    overlap (overlap (fmap
-}
