{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Draw.Symbols
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic functions for constructing symbolic drawings.

module Graphics.Gudni.Draw.Symbols
  ( hatch
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree
import Graphics.Gudni.Layout
import Graphics.Gudni.Draw.Stroke

hatch :: IsStyle style
      => SpaceOf style
      -> SpaceOf style
      -> CompoundLayout style
hatch thickness size =
  let s = size / 2
  in
  overlap [ mask . stroke thickness . makeOpenCurve $ [line (Point2 (-s) (-s)) (Point2 s s)]
          , mask . stroke thickness . makeOpenCurve $ [line (Point2 s (-s)) (Point2 (-s) s)]
          ]
