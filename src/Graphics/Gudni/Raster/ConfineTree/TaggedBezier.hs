{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Gudni.Raster.ConfineTree.TaggedBezier
  ( CurveTag(..)
  , TaggedBezier(..)
  , tBez
  , tCurveTag
  , tBezItem
  , ItemBezier(..)
  , iBez    
  , iBezItem
  )
where

import Graphics.Gudni.Figure.Primitive
import Graphics.Gudni.Figure.Bezier.Type
import Graphics.Gudni.Raster.ItemInfo

import Graphics.Gudni.Util.StorableM

import Data.Char
import Control.Lens

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

newtype CurveTag = CurveTag Int deriving (Num, Eq, Ord)

instance Show CurveTag where
  show (CurveTag i) =
    let rem = i `mod` 26
        den = i `div` 26
        pre = if den > 0 then show (CurveTag den) else ""
    in
    pre ++ (pure $ chr $ rem + (ord 'A'))

instance Out CurveTag where
  doc x = text (show x)
  docPrec _ = doc

data TaggedBezier s =
    TaggedBezier
    { _tBez      :: Bezier s
    , _tCurveTag :: CurveTag
    , _tBezItem  :: ItemTagId
    } deriving (Generic)
makeLenses ''TaggedBezier

instance Show s => Show (TaggedBezier s) where
  show tb = show (tb ^. tCurveTag, tb ^. tBezItem, tb ^. tBez)

data ItemBezier s =
    ItemBezier
    { _iBez      :: Bezier s
    , _iBezItem  :: ItemTagId
    } deriving (Generic)
makeLenses ''ItemBezier

instance Show s => Show (ItemBezier s) where
  show ib = show (ib ^. iBezItem, ib ^. iBez)


instance StorableM (ItemBezier SubSpace) where
    sizeOfM _ = do sizeOfM (undefined :: Bezier SubSpace)
                   sizeOfM (undefined :: ItemTagId )
    alignmentM _ = do alignmentM (undefined :: Bezier SubSpace)
                      alignmentM (undefined :: ItemTagId)
    peekM = do bez       <- peekM
               itemTagId <- peekM
               return $ ItemBezier bez itemTagId
    pokeM (ItemBezier bez itemTagId) = do pokeM bez
                                          pokeM itemTagId

instance Storable (ItemBezier SubSpace) where
    sizeOf = sizeOfV
    alignment = alignmentV
    peek = peekV
    poke = pokeV
