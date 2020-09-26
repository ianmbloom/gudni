{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module Graphics.Gudni.Raster.ConfineTree.TaggedBezier
  ( TaggedBezier(..)
  )
where

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Raster.ItemInfo

import Graphics.Gudni.Util.StorableM
import Text.PrettyPrint.GenericPretty

data TaggedBezier s =
    TaggedBezier
    { tBez    :: Bezier s
    , tBezTag ::  ItemTagId
    } deriving (Show, Generic)

instance StorableM (TaggedBezier SubSpace) where
    sizeOfM _ = do sizeOfM (undefined :: Bezier SubSpace)
                   sizeOfM (undefined :: ItemTagId )
    alignmentM _ = do alignmentM (undefined :: Bezier SubSpace)
                      alignmentM (undefined :: ItemTagId)
    peekM = do bez <- peekM
               tag <- peekM
               return $ TaggedBezier bez tag
    pokeM (TaggedBezier bez tag) = do pokeM bez
                                      pokeM tag

instance Storable (TaggedBezier SubSpace) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV
