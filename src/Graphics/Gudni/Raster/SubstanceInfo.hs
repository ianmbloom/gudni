{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.ItemInfo
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructors for attaching metadata to shapes∘

module Graphics.Gudni.Raster.SubstanceInfo
  ( SubstanceInfo (..)
  , SubstanceTagId (..)
  , noSubstanceTag
  , SubstanceTag (..)
  , ColorId(..)
  , TextureId(..)
  , substanceTagToInfo
  , substanceInfoToTag
  )
where

import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Raster.Constants
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Util.Pile

import Data.Bits
import Control.Lens
import Foreign.C.Types (CUInt, CChar, CULong)
import Control.DeepSeq

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

type SubstanceTag_ = CULong

newtype SubstanceTag
    = SubstanceTag
    { unSubstanceTag :: SubstanceTag_
    } deriving (Ord, Eq, Num, Enum)

newtype SubstanceTagId = SubstanceTagId {unSubstanceTagId :: Reference SubstanceTag} deriving (Ord, Eq, Num, Enum)

noSubstanceTag = SubstanceTag nOsUBSTANCEtAG

instance Show SubstanceTagId where
    show (SubstanceTagId i) = show i ++ "sid"

newtype ColorId = ColorId {unColorId :: Reference CChar} deriving (Show, Ord, Eq, Num, Enum)

newtype TextureId = TextureId {unTextureId :: Reference CChar} deriving (Show, Ord, Eq, Num, Enum)

data SubstanceInfo
    = SolidInfo
    { _itemSolidColorId :: ColorId
    }
    | TextureInfo
    { _itemSubstanceMem :: TextureId
    }
    deriving (Show)
makeLenses ''SubstanceInfo

-- | Make a SubstanceTag from a SubstanceInfo
substanceInfoToTag :: SubstanceInfo -> SubstanceTag
substanceInfoToTag info =
    SubstanceTag $
    case info of
      SolidInfo   (ColorId   colorId  ) -> sUBSTANCEtAGtYPEsOLIDcOLOR .|. (fromIntegral colorId   .&. sUBSTANCEtAGrEFbITMASK)
      TextureInfo (TextureId textureId) -> sUBSTANCEtAGtYPEtEXTURE    .|. (fromIntegral textureId .&. sUBSTANCEtAGrEFbITMASK)

-- | Extract a SubstanceInfo from a SubstanceTag
substanceTagToInfo :: SubstanceTag -> SubstanceInfo
substanceTagToInfo (SubstanceTag tag) =
  let ref = tag .&. sUBSTANCEtAGrEFbITMASK
      substanceType = tag .&. sUBSTANCEtAGtYPEbITmASK
      info
        | substanceType == sUBSTANCEtAGtYPEsOLIDcOLOR = SolidInfo . ColorId . Ref . fromIntegral $ ref
        | substanceType == sUBSTANCEtAGtYPEtEXTURE    = TextureInfo . TextureId . Ref . fromIntegral $ ref
        | otherwise                                   = error "substanceType not supported"
  in  info

instance Show SubstanceTag where
  show = show . substanceTagToInfo

instance NFData ColorId where
  rnf (ColorId a) = rnf a

instance NFData TextureId where
  rnf (TextureId a) = rnf a

instance NFData SubstanceInfo where
  rnf (SolidInfo   a) = rnf a
  rnf (TextureInfo a) = rnf a

  -- * Instances
instance NFData SubstanceTagId where
  rnf (SubstanceTagId a) = a `deepseq` ()

instance NFData SubstanceTag where
  rnf (SubstanceTag a) = a `deepseq` ()

instance StorableM SubstanceTag where
  sizeOfM _ =
    do sizeOfM (undefined :: SubstanceTag_   ) -- picture references must fit into data size of half4.
  alignmentM _ =
    do alignmentM (undefined :: SubstanceTag_  )
  peekM =
    do  tag <- peekM
        return (SubstanceTag tag)
  pokeM (SubstanceTag tag) =
        pokeM tag

instance Storable SubstanceTag where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance Storable SubstanceTagId where
  sizeOf (SubstanceTagId i) = sizeOf i
  alignment (SubstanceTagId i) = alignment i
  peek i = SubstanceTagId <$> peek (castPtr i)
  poke i (SubstanceTagId a) = poke (castPtr i) a
