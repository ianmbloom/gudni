{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Graphics.Gudni.Layout.Stroke
  ( CanStroke(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Subdividable


import Control.Lens
import Control.Applicative
import Control.Monad

class (Space (SpaceOf t), HasSpace t) => CanStroke t where
  type Stroked (t :: *)
  strokeOffset :: SpaceOf t -> SpaceOf t -> t -> Stroked t

  stroke :: SpaceOf t -> t -> Stroked t
  stroke thickness = strokeOffset (negate thickness/2) thickness

instance Space s => CanStroke (BezierSpace s) where
  type Stroked (BezierSpace s) = Shape s
  strokeOffset offset thickness bSpace =
    let lengths = bezierSpaceLengths bSpace
        rect = segmentedRectangle thickness lengths
    in  Shape . pure . projectOnto False bSpace . translateByXY 0 offset $ rect

instance Space s => CanStroke (OpenCurve s) where
  type Stroked (OpenCurve s) = Shape s
  strokeOffset offset thickness path =
    let bSpace = makeBezierSpace arcLength (view curveSegments path)
    in  strokeOffset offset thickness bSpace

instance Space s => CanStroke (Bezier s) where
  type Stroked (Bezier s) = Shape s
  strokeOffset offset thickness bz =
    strokeOffset offset thickness (makeOpenCurve [bz])

instance Space s => CanStroke (Outline s) where
  type Stroked (Outline s) = Shape s
  strokeOffset offset thickness outline =
    let bSpace  = makeBezierSpace arcLength . view outlineSegments $ outline
        lengths = bezierSpaceLengths bSpace
        inner   = segmentedLine offset lengths
        outer   = segmentedLine (offset + thickness) lengths
    in  Shape [projectOnto False bSpace (Outline outer), projectOnto False bSpace (Outline inner)]

instance Space s => CanStroke (Shape s) where
  type Stroked (Shape s) = Shape s
  strokeOffset offset thickness = Shape . join . fmap (view shapeOutlines .strokeOffset offset thickness) . view shapeOutlines
-- |
segmentedLine :: (Space s, Chain f) => s -> f s -> f (Bezier s)
segmentedLine y ls =
  let xs = scanlChain (+) 0 ls
      segments = zipWithChain (,) xs (rest xs)
      mkLine y (start, end) = line (Point2 start y) (Point2 end y)
  in  fmap (mkLine y) segments

-- | Create a rectangle outline that is already split at specified lengths.
segmentedRectangle :: (Space s, Chain t, Show (t (Bezier s)), HasSpace (t (Bezier s))) => s -> t s -> Outline_ t s
segmentedRectangle height ls =
  let top = segmentedLine 0 ls
      bottom = reverseChain . fmap reverseBezier . segmentedLine height $ ls
      startCap = line (lastLink bottom ^. bzEnd) (firstLink top ^. bzStart)
      endCap = line (lastLink top ^. bzEnd) (firstLink bottom ^. bzStart)
   in subdivide 3 $ Outline $ top <|> pure endCap <|> bottom <|> pure startCap
