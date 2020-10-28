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
{-# LANGUAGE DeriveGeneric         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.ShapeTree
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A Substance is basically one of stock ways that a shape or area can be filled.
-- This includes textures that are access to an image.

module Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
   ( FCombineType(..)
   )
where

import Graphics.Gudni.Base

data FCombineType
    = FComposite
    | FMask -- Mask , Multiply and FloatAnd are the same
    | FAdd
    | FFloatOr  -- x + y - (x*y)
    | FFloatXor -- x + y - 2(x*y)
     --      | RGTE
     --      | RGT
    deriving (Show, Generic)

instance Out FCombineType
