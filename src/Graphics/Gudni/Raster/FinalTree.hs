-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Constants
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- The final tree format delivered to the rasterizer.

module Graphics.Gudni.Raster.FinalTree
  ( FinalTree(..)
  )
where

import Graphics.Gudni.Figure

type FinalTree token s = Tree Overlap (SRep token NamedTexture (Tree Compound (Shape s)))
