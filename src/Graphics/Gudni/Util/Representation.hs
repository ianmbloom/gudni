{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Gudni.Util.Representation
  (
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Layout
import Linear.V2
import Linear.V3

mkLine :: Space s => V2 (Point2 s) -> Bezier s
mkLine (V2 a b) = line a b

openCircle :: forall s . Space s => s -> CompoundTree s
openCircle r = mask . stroke 0.25 . scaleBy r $ (circle :: Outline s)
closedCircle :: forall s . Space s => s -> CompoundTree s
closedCircle r = mask . Shape . pure . scaleBy r $ (circle :: Outline s)

class HasSpace t => HasRepresentation t where
  represent :: t -> ShapeTree Int (SpaceOf t)

instance Space s => HasRepresentation (Bezier s) where
  represent bz@(Bez v0 c v1) =
    let r = 3
    in
    overlap [ colorWith red   $ translateBy v0 $ closedCircle r
            , colorWith blue  $ translateBy  c $ openCircle r
            , colorWith red   $ translateBy v1 $ closedCircle r
            , colorWith black $ mask . stroke 1 $ bz
            ]

instance (Space s, Space t, s~t) => HasRepresentation (FacetSide s t) where
  represent facetSide@(FacetSide sceneSide textureSide) =
    overlap [represent sceneSide, colorWith green . mask . stroke 0.25 . mkLine $ textureSide]

instance (Space s, Space t, s~t) => HasRepresentation (Facet_ s t) where
  represent facet@(Facet sides) = overlap . fmap represent $ sides

{-
instance (Space s, Space t, s~t) => HasRepresentation (HardFacet_ s t) where
  represent hardFacet@(HardFacet face texture) =
    let faceRep = colorWith (light blue) overlap . fmap (
    overlap (overlap (fmap
-}
