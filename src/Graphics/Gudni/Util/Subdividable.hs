{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Util.Subdividable
  ( CanSubdivide(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Figure.ShapeTree
import Graphics.Gudni.Layout
import Graphics.Gudni.Raster.TraverseShapeTree
--import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Debug
import Linear.V2
import Linear.V3
import Control.Monad
import Control.Applicative
import Control.Lens

class (HasSpace t) => CanSubdivide t where
    subdivide :: Int -> t -> t

subdivideBeziers :: forall f s . (Space s, Alternative f, Monad f) => Int -> f (Bezier s) -> f (Bezier s)
subdivideBeziers steps = join . fmap (go steps)
       where go :: Int -> Bezier s -> f (Bezier s)
             go steps bz =
                if steps > 0
                then let (left, right) = splitBezier 0.5 bz
                     in go (steps - 1) left <|> go (steps - 1) right
                else pure bz

instance (Alternative f, Monad f, Space s) => CanSubdivide (Outline_ f s) where
    subdivide steps (Outline bs) = Outline (subdivideBeziers steps bs)

instance (Alternative f, Monad f, Space s) => CanSubdivide (OpenCurve_ f s) where
    subdivide steps (OpenCurve bs) = OpenCurve (subdivideBeziers steps bs)

instance Space s => CanSubdivide (Shape s) where
    subdivide steps (Shape outlines) = Shape $ fmap (subdivide steps) outlines

instance (CanSubdivide (Leaf i)) => CanSubdivide (STree i) where
    subdivide steps = mapSLeaf (subdivide steps)

instance (CanSubdivide rep) => CanSubdivide (SRep token label rep) where
    subdivide steps = over sRep (subdivide steps)

instance IsStyle style => CanSubdivide (Layout style) where
    subdivide steps (Layout tree) = Layout $ mapSItem (fmap (subdivide steps)) tree

instance IsStyle style => CanSubdivide (CompoundLayout style) where
    subdivide steps (CompoundLayout tree) = CompoundLayout $ mapSItem (fmap (subdivide steps)) tree

instance IsStyle style => CanSubdivide (LayoutRep style) where
    subdivide steps rep =
      case rep of
        Glyph style c -> Glyph style c
        LayoutShape shape -> LayoutShape (subdivide steps shape)
{-
instance (Space s, Space t, s~t) => CanSubdivide (FacetSide s t) where
    represent dk facetSide@(FacetSide sceneSide textureSide) =
        overlap [represent dk sceneSide, withColor green . mask . stroke 0.25 . mkLine $ textureSide]

instance (Space s, Space t, s~t) => HasRepresentation (Facet_ s t) where
    represent dk facet@(Facet sides) = overlap . fmap (represent dk) $ sides


instance (Space s, Space t, s~t) => HasRepresentation (HardFacet_ s t) where
  represent hardFacet@(HardFacet face texture) =
    let faceRep = withColor (light blue) overlap . fmap (
    overlap (overlap (fmap
-}
