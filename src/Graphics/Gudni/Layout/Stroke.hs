{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Graphics.Gudni.Layout.Stroke
  ( CanStroke(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.Deknob
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Loop
import Graphics.Gudni.Util.Debug
import qualified Data.Vector as V

import Control.Lens
import Control.Applicative
import Control.Monad

class (Space (SpaceOf t), HasSpace t) => CanStroke t where
  strokeOffset :: (Chain f) => SpaceOf t -> SpaceOf t -> t -> Shape_ f (SpaceOf t)

  stroke :: (Chain f) => SpaceOf t -> t -> Shape_ f (SpaceOf t)
  stroke thickness = strokeOffset (negate thickness/2) thickness

strokeBezierSpace :: forall s f . (Space s, Chain f) => s -> s -> BezierSpace s -> Shape_ f s
strokeBezierSpace offset thickness bSpace =
  let lengths :: f s
      lengths = bezierSpaceLengths bSpace
      rect :: Shape_ f s
      rect = segmentedRectangle thickness lengths
  in  projectDefault False bSpace . applyTranslation (Point2 0 offset) $ rect

instance (Space s, Chain f, Show (f(Bezier s))) => CanStroke (OpenCurve_ f s) where
  strokeOffset offset thickness path =
    let bSpace = makeBezierSpace arcLength path
    in  strokeBezierSpace offset thickness bSpace

instance (Space s, s~SpaceOf [Bezier s]) => CanStroke (Bezier s) where
  strokeOffset offset thickness bz =
    strokeOffset offset thickness (makeOpenCurve [bz])

instance (Chain f, Space s, Show (f(Bezier s))) => CanStroke (Outline_ f s) where
  strokeOffset offset thickness outline =
    let bSpace  = makeBezierSpace arcLength $ OpenCurve $ view outlineSegments outline
        lengths = bezierSpaceLengths bSpace
        inner   = segmentedLine offset lengths
        outer   = segmentedLine (offset + thickness) lengths
        innerProjected = closeOpenCurve $ projectDefault False bSpace (makeOpenCurve outer)
        outerProjected = closeOpenCurve $ projectDefault False bSpace (makeOpenCurve inner)
    in  Shape [outerProjected, innerProjected]

instance  (Chain f, Space s, Show (f(Bezier s))) => CanStroke (Shape_ f s) where
  strokeOffset offset thickness = Shape . join . fmap (view shapeOutlines . strokeOffset offset thickness) . view shapeOutlines

-- | Build a line that doesn't require any breaks when projected.
segmentedLine :: (Space s, Chain f) => s -> f s -> f (Bezier s)
segmentedLine y ls =
  let xs = scanlChain (+) 0 ls
      segments = zipWithChain (,) xs (rest xs)
      mkLine y (start, end) = line (Point2 start y) (Point2 end y)
  in  fmap (mkLine y) segments

-- | Create a rectangle outline that is already split at specified lengths.
segmentedRectangle :: (Space s, Chain t) => s -> t s -> Shape_ t s
segmentedRectangle height ls =
  let top = segmentedLine 0 ls
      bottom = reverseChain . fmap reverseBezier . segmentedLine height $ ls
      startCap = line (lastLink bottom ^. bzEnd) (firstLink top ^. bzStart)
      endCap = line (lastLink top ^. bzEnd) (firstLink bottom ^. bzStart)
   in Shape . pure . Outline $ top <|> pure endCap <|> bottom <|> pure startCap
