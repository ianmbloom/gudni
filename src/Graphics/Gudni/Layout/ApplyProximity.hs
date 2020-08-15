{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE StandaloneDeriving    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Proximity
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for applying proximity relationships

module Graphics.Gudni.Layout.ApplyProximity
  ( applyNextTo
  , applyAlign
  , applyAlignWithSize
  , applyProximity
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Raster.TraverseShapeTree
import Graphics.Gudni.Layout.Fill
import Graphics.Gudni.Layout.Empty
import Graphics.Gudni.Layout.Alignment
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Proximity
import Graphics.Gudni.Layout.Overlappable
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Util
import Linear
import Data.List
import Data.Char
import Data.Maybe
import Control.Lens
import Control.Applicative

combineDeltas (a0, b0) (a1, b1) = (a0 + a1, b0 + b1)

applyMaybeAlign :: ( Space s
                   , Axis axis
                   )
                => axis
                -> Maybe Alignment
                -> ( Box s, Box s)
                -> ( Point2 s, Point2 s)
applyMaybeAlign axis mAlign boxes = case mAlign of
                                  Nothing -> (zeroPoint, zeroPoint)
                                  Just alignment -> applyAlign axis alignment boxes

applyAlignWithSize :: ( Space s
                 , Axis axis
                 )
              => axis
              -> Alignment
              -> s
              -> Box s
              -> Point2 s
applyAlignWithSize axis alignment size box =
    let boxSize = box ^. acrossBox axis
        offset = case alignment of
                    AlignMin    -> 0
                    AlignMax    -> size - boxSize
                    AlignCenter -> (size - boxSize) / 2
    in  deltaOnAxis (nextAxis axis) offset

overBoth f (a, b) = (f a, f b)

applyAlign :: ( Space s
              , Axis axis)
           => axis
           -> Alignment
           -> (Box s, Box s)
           -> (Point2 s, Point2 s)
applyAlign axis alignment (a, b) =
  let (aSize, bSize) = overBoth (view (acrossBox axis)) (a, b)
      size  = max aSize bSize
  in  overBoth (applyAlignWithSize axis alignment size) (a, b)

applyNextTo :: ( Space s
               , Axis axis
               )
            => axis
            -> (Box s, Box s)
            -> (Point2 s, Point2 s)
applyNextTo axis (a, b) =
  let d = a ^. maxBox . along axis - b ^. minBox . along axis
  in  (zeroPoint, deltaOnAxis (nextAxis axis) d)

applyProximity :: ( Space s
                  , IsStyle style)
               => style
               -> Proximity
               -> (Box s, Box s)
               -> ( Point2 s, Point2 s)
applyProximity style proximity (aBox, bBox) =
        case proximity of
            NextTo eAxis mAlignment ->
               let nextToDeltas = (fromEitherAxis (applyNextTo     Horizontal) (applyNextTo     Vertical) eAxis) (aBox, bBox)
                   alignDeltas  =  fromEitherAxis (applyMaybeAlign Vertical) (applyMaybeAlign Horizontal) eAxis mAlignment (aBox, bBox)
               in  combineDeltas nextToDeltas alignDeltas
            OnTopOf mAlignHori mAlignVert ->
               let alignHoriDeltas = applyMaybeAlign Horizontal mAlignHori (aBox, bBox)
                   alignVertDeltas = applyMaybeAlign Vertical   mAlignVert (aBox, bBox)
                in combineDeltas alignHoriDeltas alignVertDeltas
