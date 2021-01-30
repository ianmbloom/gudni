{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Graphics.Gudni.Raster.ConfineTree.Primitive.Stack
  ( toggleItemActive
  , combineItemStacks
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Raster.TagTypes
import Graphics.Gudni.Raster.ConfineTree.Primitive.Type
import Graphics.Gudni.Raster.ConfineTree.Primitive.Cross
import Graphics.Gudni.Raster.ConfineTree.Type
import Graphics.Gudni.Util.Debug

import Linear.Metric
import Control.Applicative
import Control.Lens

toggleItemActive :: Ord item => item -> [item] -> [item]
toggleItemActive i stack =
    case stack of
        (x:xs) | i >  x -> i:x:xs
               | i == x -> xs
               | i <  x -> x:toggleItemActive i xs
        [] -> [i]

combineItemStacks :: Ord item => [item] -> [item] -> [item]
combineItemStacks = flip (foldl (flip toggleItemActive))
