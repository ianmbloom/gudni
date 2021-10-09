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
  , gridFrom
  , gridOf
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util
import Control.Lens

horizontallySpacedBy :: (HasSpace a)
                     => (Point2 (SpaceOf a) -> a -> a)
                     -> Point2 (SpaceOf a)
                     -> Ax Horizontal (SpaceOf a)
                     -> [a]
                     -> [a]
horizontallySpacedBy f topLeft s = zipWith ($) (map f $ iterate (^+^ makePoint s 0) (makePoint (topLeft ^. pX) 0))

verticallySpacedBy :: (HasSpace a)
                   => (Point2 (SpaceOf a) -> a -> a)
                   -> Point2 (SpaceOf a)
                   -> Ax Vertical (SpaceOf a)
                   -> [a]
                   -> [a]
verticallySpacedBy f topLeft s = zipWith ($) (map f $ iterate (^+^ makePoint 0 s) (makePoint 0 (topLeft ^. pY)))

verticallySpacedListBy :: (HasSpace a)
                       => (Point2 (SpaceOf a) -> a -> a)
                       -> Point2 (SpaceOf a)
                       -> Ax Vertical (SpaceOf a)
                       -> [[a]]
                       -> [[a]]
verticallySpacedListBy f topLeft s = zipWith ($) (map (\i -> map (f i)) $ iterate (^+^ makePoint 0 s) (makePoint 0 (topLeft ^. pY)))

gridFrom :: (HasSpace a)
         => (Point2 (SpaceOf a) -> a -> a)
         -> Point2 (SpaceOf a)
         -> SpaceOf a
         -> Int
         -> Int
         -> [a]
         -> [a]
gridFrom f topLeft s width height = concat . take height . verticallySpacedListBy f topLeft (toAlong Vertical s) . map (take width . horizontallySpacedBy f topLeft (toAlong Horizontal s)) . breakList width

gridOf :: (HasSpace a, Transformable a)
       => SpaceOf a
       -> Int
       -> Int
       -> [a]
       -> [a]
gridOf = gridFrom translateBy zeroPoint
