{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic color combination type.

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
    | FMin
    | FMax
    | FHsvAdjust
    | FTransparent
     --      | RGTE
     --      | RGT
    deriving (Show, Generic)

instance Out FCombineType
