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
import Graphics.Gudni.Raster.TraverseShapeTree
--import Graphics.Gudni.Layout
import Graphics.Gudni.Util.Debug
import Linear.V2
import Linear.V3
import Control.Monad
import Control.Applicative

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

instance (Alternative f, Monad f, Space s, HasSpace (f (Bezier s))) => CanSubdivide (Outline_ f s) where
    subdivide steps (Outline bs) = Outline (subdivideBeziers steps bs)

instance (Alternative f, Monad f, Space s, HasSpace (f (Bezier s))) => CanSubdivide (OpenCurve_ f s) where
    subdivide steps (OpenCurve bs) = OpenCurve (subdivideBeziers steps bs)

instance Space s => CanSubdivide (Shape s) where
    subdivide steps (Shape outlines) = Shape $ fmap (subdivide steps) outlines



{-
instance (Space s, Space t, s~t) => CanSubdivide (FacetSide s t) where
    represent dk facetSide@(FacetSide sceneSide textureSide) =
        overlap [represent dk sceneSide, colorWith green . mask . stroke 0.25 . mkLine $ textureSide]

instance (Space s, Space t, s~t) => HasRepresentation (Facet_ s t) where
    represent dk facet@(Facet sides) = overlap . fmap (represent dk) $ sides


instance (Space s, Space t, s~t) => HasRepresentation (HardFacet_ s t) where
  represent hardFacet@(HardFacet face texture) =
    let faceRep = colorWith (light blue) overlap . fmap (
    overlap (overlap (fmap
-}