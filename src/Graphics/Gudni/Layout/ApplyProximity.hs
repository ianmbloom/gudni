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
import Graphics.Gudni.ShapeTree
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

applyMaybeAlign :: ( IsStyle style
                   , Axis axis
                   , StyleAxis axis
                   )
                => style
                -> axis
                -> Maybe Alignment
                -> (Box    (SpaceOf style), Box    (SpaceOf style))
                -> (Point2 (SpaceOf style), Point2 (SpaceOf style))
applyMaybeAlign style axis mAlign boxes =
    case mAlign of
        Nothing -> (zeroPoint, zeroPoint)
        Just alignment -> applyAlign style axis alignment boxes

applyAlignWithSize :: ( IsStyle style
                      , Axis axis
                      , StyleAxis axis
                      )
                   => style
                   -> axis
                   -> Alignment
                   -> Along axis (SpaceOf style)
                   -> Box (SpaceOf style)
                   -> Point2 (SpaceOf style)
applyAlignWithSize style axis alignment size box =
    let boxSize = box ^. acrossBox axis
        offset = case alignment of
                    AlignMin    -> 0
                    AlignMax    -> size - boxSize
                    AlignCenter -> (size - boxSize) / 2
    in  deltaOnAxis axis offset

overBoth f (a, b) = (f a, f b)

applyAlign :: ( IsStyle style
              , Axis axis
              , StyleAxis axis
              )
           => style
           -> axis
           -> Alignment
           -> (Box    (SpaceOf style), Box    (SpaceOf style))
           -> (Point2 (SpaceOf style), Point2 (SpaceOf style))
applyAlign style axis alignment (a, b) =
  let (aSize, bSize) = overBoth (view (acrossBox axis)) (a, b)
      size  = max aSize bSize
  in  overBoth (applyAlignWithSize style axis alignment size) (a, b)

applyNextTo :: ( IsStyle style
               , Axis axis
               , StyleAxis axis
               )
            => style
            -> axis
            -> (Box    (SpaceOf style), Box    (SpaceOf style))
            -> (Point2 (SpaceOf style), Point2 (SpaceOf style))
applyNextTo style axis (a, b) =
  let d = a ^. maxBox . along axis - b ^. minBox . along axis
  in  (zeroPoint, deltaOnAxis axis (d + toAlong axis (styleGap axis style)) )

applyProximity :: ( IsStyle style)
               => style
               -> Proximity
               -> (Box    (SpaceOf style), Box    (SpaceOf style))
               -> (Point2 (SpaceOf style), Point2 (SpaceOf style))
applyProximity style proximity (aBox, bBox) =
        case proximity of
            NextTo eAxis mAlignment ->
               let nextToDeltas = (fromEitherAxis (applyNextTo     style Horizontal) (applyNextTo     style Vertical  ) eAxis)          (aBox, bBox)
                   alignDeltas  =  fromEitherAxis (applyMaybeAlign style Vertical)   (applyMaybeAlign style Horizontal) eAxis mAlignment (aBox, bBox)
               in  combineDeltas nextToDeltas alignDeltas
            OnTopOf mAlignHori mAlignVert ->
               let alignHoriDeltas = applyMaybeAlign style Horizontal mAlignHori (aBox, bBox)
                   alignVertDeltas = applyMaybeAlign style Vertical   mAlignVert (aBox, bBox)
                in combineDeltas alignHoriDeltas alignVertDeltas
