-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.ScreenMode
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple type for defining a window or fullscreen interface.

module Graphics.Gudni.Interface.ScreenMode
  ( ScreenMode(..)
  )
where

import Graphics.Gudni.Figure

data ScreenMode
  -- | Constructor for windows of a particular size.
  = Window (Point2 PixelSpace)
  -- | Constructor for full screen interface.
  | FullScreen
