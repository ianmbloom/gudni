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

module Graphics.Gudni.Draw.Elipse
  ( circle
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout

import Graphics.Gudni.Draw.Plot

-- | Basic circleCurve
circleCurve :: (Space s, Chain f, Show (f (Bezier s))) => OpenCurve_ f s
circleCurve = makeArc fullTurn

circle :: (Space s, Chain f, Show (f (Bezier s))) => Shape_ f s
circle = Shape . pure . closeOpenCurve $ circleCurve
