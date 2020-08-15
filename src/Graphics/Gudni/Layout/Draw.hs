{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Draw
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic functions for constructing drawings.

module Graphics.Gudni.Layout.Draw
  ( CanMask(..)
  , rectangle
  , boxToRectangle
  , openRectangle
  , circle
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Layout
import Graphics.Gudni.Layout.Fill
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Chain
import Graphics.Gudni.Util.Plot
import Graphics.Gudni.Raster.TraverseShapeTree

import Graphics.Gudni.Layout.Collect
import Graphics.Gudni.Util.Debug

import Control.Lens
import qualified Data.Vector as V
import Control.Applicative

class HasSpace t => CanMask t where
    mask :: Shape (SpaceOf t) -> t

instance Space s => CanMask (CompoundTree s) where
    mask = CompoundTree . SLeaf . SItem . Just

instance IsStyle style => CanMask (CompoundLayout style) where
    mask = CompoundLayout . SLeaf . SItem . Just . LayoutShape

-- | Basic circleCurve
circleCurve :: (Space s, Chain f, Show (f (Bezier s))) => OpenCurve_ f s
circleCurve = makeArc fullTurn

circle :: (Space s, Chain f, Show (f (Bezier s))) => Shape_ f s
circle = Shape . pure . closeOpenCurve $ circleCurve

-- | Create a series of segments based on the size of a rectangle.
rectangleCurve :: (Alternative f, Space s) => Point2 s -> f (Bezier s)
rectangleCurve v =
        ( pure $ line (makePoint 0         0        ) (makePoint (v ^. pX) 0        ) )
    <|> ( pure $ line (makePoint (v ^. pX) 0        ) (makePoint (v ^. pX) (v ^. pY)) )
    <|> ( pure $ line (makePoint (v ^. pX) (v ^. pY)) (makePoint 0         (v ^. pY)) )
    <|> ( pure $ line (makePoint 0         (v ^. pY)) (makePoint 0         0        ) )

boxToRectangle :: (Space s, Chain f) => Box s -> Shape_ f s
boxToRectangle box = applyTranslation (box ^. minBox) . Shape . pure . Outline . rectangleCurve . sizeBox $ box

combineShape (Shape as) (Shape bs) = Shape (as <|> bs)

-- | Draw an open rectangle without the computational complexity of stroking.
openRectangle :: (Space s, Chain f) => s -> Box s -> Shape_ f s
openRectangle thickness box =
  let offset = Point2 thickness thickness
      d = 2 * thickness
      outer = boxToRectangle box
      inner = boxToRectangle (Box (box ^. minBox + offset) (box ^. maxBox - offset))
  in
  if  box ^. widthBox > d && box ^. heightBox > d
  then combineShape outer inner
  else outer

rectangle :: (Space s, Chain f) => Point2 s -> Shape_ f s
rectangle = Shape . pure . Outline . rectangleCurve
