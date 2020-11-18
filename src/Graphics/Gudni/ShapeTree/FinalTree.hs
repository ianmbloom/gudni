-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Thresholds.Constants
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- The final tree format delivered to the rasterizer.

module Graphics.Gudni.ShapeTree.FinalTree
  ( FinalTree(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.ShapeTree.STree

type FinalTree token s = Tree Overlap (SMask token NamedTexture (Tree Compound (Shape s)))
