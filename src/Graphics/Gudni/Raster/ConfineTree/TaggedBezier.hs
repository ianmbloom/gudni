{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.ConfineTree.TaggedBezier
  ( TBezier(..)
  , tBez
  , tPrimTagId
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Raster.Dag.TagTypes
import Graphics.Gudni.Raster.Dag.PrimTag

import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Util

import Control.Lens

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

data TBezier s =
    TBezier
    { _tPrimTagId :: PrimTagId
    , _tBez       :: Bezier s
    } deriving (Generic)
makeLenses ''TBezier

instance Show s => Show (TBezier s) where
  show tb = show (tb ^. tPrimTagId, tb ^. tBez)

instance Out s => Out (TBezier s)
{-
instance StorableM (TBezier SubSpace) where
    sizeOfM _ = do sizeOfM (undefined :: Bezier SubSpace)
                   sizeOfM (undefined :: PrimTagId )
    alignmentM _ = do alignmentM (undefined :: Bezier SubSpace)
                      alignmentM (undefined :: PrimTagId)
    peekM = do bez       <- peekM
               primTagId <- peekM
               return $ TBezier bez primTagId
    pokeM (TBezier bez primTagId) = do pokeM bez
                                       pokeM primTagId

instance Storable (TBezier SubSpace) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV
-}
