{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gudni.Util.Subdividable
  ( CanSubdivide(..)
  , subdivideBeziers
  )
where

import Graphics.Gudni.Figure
<<<<<<< HEAD
import Graphics.Gudni.Layout.Layout
import Graphics.Gudni.Layout.Style
=======
>>>>>>> origin/flatpath
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

instance (Alternative f, Monad f, Space s) => CanSubdivide (Shape_ f s) where
    subdivide steps (Shape outlines) = Shape $ fmap (subdivide steps) outlines
