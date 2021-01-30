{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Combine.Type
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic color combination type.

module Graphics.Gudni.Raster.Fabric.Combine.Type
   ( FCombineType(..)
   , combineMaybeBox
   )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Util.Util

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
     -- | RGTE
     -- | RGT
    deriving (Show, Generic)

instance Out FCombineType

instance HasDefault FCombineType where
    defaultValue = FComposite

andMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
andMaybe f (Just a) (Just b) = Just (f a b)
andMaybe f _ _ = Nothing

combineMaybeBox :: Space s => FCombineType -> Maybe (Box s) -> Maybe (Box s) -> Maybe (Box s)
combineMaybeBox ty mBoxA mBoxB =
    case ty of
        FComposite   -> andMaybe minMaxBox mBoxA mBoxB
        FMask        -> mBoxA
        FAdd         -> andMaybe minMaxBox mBoxA mBoxB
        FFloatOr     -> andMaybe minMaxBox mBoxA mBoxB
        FFloatXor    -> andMaybe minMaxBox mBoxA mBoxB
        FMin         -> andMaybe minMaxBox mBoxA mBoxB
        FMax         -> andMaybe minMaxBox mBoxA mBoxB
        FHsvAdjust   -> mBoxB
        FTransparent -> mBoxB
