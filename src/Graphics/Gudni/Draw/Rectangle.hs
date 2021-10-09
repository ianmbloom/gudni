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

module Graphics.Gudni.Draw.Rectangle
  ( rectangle
  , boxToRectangle
  , openRectangle
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure

import Control.Lens
import Control.Applicative


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
  if  box ^. widthBox > toAlong Horizontal d && box ^. heightBox > toAlong Vertical d
  then combineShape outer inner
  else outer

rectangle :: (Space s, Chain f) => Point2 s -> Shape_ f s
rectangle = Shape . pure . Outline . rectangleCurve
