{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Grid
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for simple grid layouts.

module Graphics.Gudni.Layout.Grid
  ( rowOf
  , columnOf
  , gridOf
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Glyph
import Graphics.Gudni.Layout.Scaffolding
import Graphics.Gudni.Util.Util

rowOf :: SimpleTransformable a => SpaceOf a -> [a] -> [a]
rowOf s = zipWith ($) (map translateBy $ iterate (^+^ Point2 s 0) zeroPoint)

columnOf :: SimpleTransformable a => SpaceOf a -> [a] -> [a]
columnOf s = zipWith ($) (map translateBy $ iterate (^+^ Point2 0 s) zeroPoint)

gridOf :: (SimpleTransformable a)
         => SpaceOf a
         -> Int
         -> Int
         -> [a]
         -> [a]
gridOf s width height = concat . take height . columnOf s . map (take width . rowOf s) . breakList width
