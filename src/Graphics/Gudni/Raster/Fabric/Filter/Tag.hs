
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Filter.Tag
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for converting filter type two and from fabricTags

module Graphics.Gudni.Raster.Fabric.Filter.Tag
   ( fabTagFilter
   , makeFilterTag
   )
where

import Graphics.Gudni.Raster.Fabric.Filter.Type
import Graphics.Gudni.Raster.Fabric.Tag
import Graphics.Gudni.Raster.TagTypes

fabTagFilter :: FabricTag -> FFilter
fabTagFilter tag
  | fabTagIsSqrt   tag = FSqrt   --
  | fabTagIsInvert tag = FInvert --
  | fabTagIsCos    tag = FCos    --
  | fabTagIsSin    tag = FSin    --
  | fabTagIsClamp  tag = FClamp  --

makeFilterTag :: FFilter -> FabricTag
makeFilterTag filt =
  case filt of
      FSqrt   -> makeFabTagSqrt   --
      FInvert -> makeFabTagInvert --
      FCos    -> makeFabTagCos    --
      FSin    -> makeFabTagSin    --
      FClamp  -> makeFabTagClamp  --
