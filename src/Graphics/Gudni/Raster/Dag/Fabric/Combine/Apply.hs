{-# LANGUAGE UndecidableInstances  #-} -- Show (SpaceOf leaf)
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Fabric.Combine.Apply
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for combining colors according to the various combination types.

module Graphics.Gudni.Raster.Dag.Fabric.Combine.Apply
   ( combineColor
   )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type

import Linear.Vector
import Linear.Metric
import Control.Applicative
import Control.Lens

combineColor :: Space s => FCombineType -> Color s -> Color s -> Color s
combineColor combine (Color a) (Color b) =
    case combine of
        FComposite   -> composite (Color a) (Color b)
        FMask        -> Color $ a * b
        FAdd         -> Color $ a ^+^ b
        FFloatOr     -> Color $ a ^+^ b ^-^ (a * b)
        FFloatXor    -> Color $ a ^+^ b ^-^ (2 *^ (a * b))
        FMin         -> Color $ liftA2 min a b
        FMax         -> Color $ liftA2 max a b
        FHsvAdjust   -> hsvAdjust (Color a) (Color b)
        FTransparent -> set cAlpha (view cAlpha (Color a)) (Color b)
