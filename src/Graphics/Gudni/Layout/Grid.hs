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
  ( horizontallySpacedBy
  , verticallySpacedBy
  , gridOf
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util

horizontallySpacedBy :: (HasSpace a, SimpleTransformable a) => SpaceOf a -> [a] -> [a]
horizontallySpacedBy s = zipWith ($) (map translateBy $ iterate (^+^ Point2 s 0) zeroPoint)

verticallySpacedBy :: (HasSpace a, SimpleTransformable a) => SpaceOf a -> [a] -> [a]
verticallySpacedBy s = zipWith ($) (map translateBy $ iterate (^+^ Point2 0 s) zeroPoint)

verticallySpacedListBy :: (HasSpace a, SimpleTransformable a) => SpaceOf a -> [[a]] -> [[a]]
verticallySpacedListBy s = zipWith ($) (map (\i -> map (translateBy i)) $ iterate (^+^ Point2 0 s) zeroPoint)

gridOf :: (HasSpace a, SimpleTransformable a)
       => SpaceOf a
       -> Int
       -> Int
       -> [a]
       -> [a]
gridOf s width height = concat . take height . verticallySpacedListBy s . map (take width . horizontallySpacedBy s) . breakList width
