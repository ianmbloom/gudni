{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.Dag.Primitive.WithTag
  ( TPrim(..)
  , tPrim
  , tPrimTagId
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.Primitive.Type

import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Util

import Control.Lens

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

data TPrim s =
    TPrim
    { _tPrimTagId :: PrimTagId
    , _tPrim      :: Primitive s
    } deriving (Generic)
makeLenses ''TPrim

instance Show s => Show (TPrim s) where
  show tp = show (tp ^. tPrimTagId, tp ^. tPrim)

instance Out s => Out (TPrim s)
