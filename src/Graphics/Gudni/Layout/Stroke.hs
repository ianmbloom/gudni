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


import Control.Lens
import Control.Applicative

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
    in  pure . projection bSpace . translateByXY 0 (Ortho offset) $ rect

instance Space s => CanStroke (OpenCurve s) where
  type Stroked (OpenCurve s) = Shape s
  strokeOffset offset thickness path =
    let bSpace = makeBezierSpace (Ortho . arcLength) (view curveSegments path)
    in  strokeOffset offset thickness bSpace

-- | Create a rectangle outline that is already split at specified lengths.
segmentedRectangle :: (Space s, Chain t, Show (t (Bezier s))) => s -> t s -> Outline_ t s
segmentedRectangle height ls =
  let xs = scanlChain (+) 0 ls
      segments = zipWithChain (,) xs (rest xs)
      reverseSegments = zipWithChain (,) (rest xs) xs
      mkLine y (start, end) = line (Point2 start y) (Point2 end y)
      top = fmap (mkLine 0) segments
      bottom = reverseChain . fmap (mkLine height) $ reverseSegments
      startCap = line (lastLink bottom ^. bzEnd) (firstLink top ^. bzStart)
      endCap = line (lastLink top ^. bzEnd) (firstLink bottom ^. bzStart)
   in tr "segmentedRectangle" $ Outline $ top <|> pure endCap <|> bottom <|> pure startCap
