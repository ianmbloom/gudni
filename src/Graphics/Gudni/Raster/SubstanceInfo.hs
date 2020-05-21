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
  ( DescriptionRef(..)
  , SubstanceTagId (..)
  , noSubstanceTag
  , SubstanceTag (..)
  , substanceAndRefToTag
  )
where

import Graphics.Gudni.Figure
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

type DescriptionRef = Reference CChar
type SubstanceTag_  = CULong

newtype SubstanceTag
    = SubstanceTag
    { unSubstanceTag :: SubstanceTag_
    } deriving (Ord, Eq, Num, Enum)

newtype SubstanceTagId = SubstanceTagId {unSubstanceTagId :: Reference SubstanceTag} deriving (Ord, Eq, Num, Enum)

noSubstanceTag = SubstanceTag nOsUBSTANCEtAG

instance Show SubstanceTagId where
    show (SubstanceTagId i) = show i ++ "sid"

-- | Make a SubstanceTag from a SubstanceInfo
substanceAndRefToTag :: Substance textureLabel s -> DescriptionRef -> SubstanceTag
substanceAndRefToTag substance descriptionRef =
    let tagType = case substance of
                      Solid   {} -> sUBSTANCEtAGtYPEsOLIDcOLOR
                      Texture {} -> sUBSTANCEtAGtYPEtEXTURE
                      Linear  {} -> sUBSTANCEtAGtYPElINEARgRADIENT
                      Radial  {} -> sUBSTANCEtAGtYPErADIALgRADIENT
                      TransformSubstance {} -> error "no tag for TransformSubstance"
    in SubstanceTag $ tagType .|. (fromIntegral descriptionRef .&. sUBSTANCEtAGrEFbITMASK)

-- | Show SubstanceTag
instance Show SubstanceTag where
  show (SubstanceTag tag) =
    let ref = tag .&. sUBSTANCEtAGrEFbITMASK
        substanceType = tag .&. sUBSTANCEtAGtYPEbITmASK
        typeString
          | substanceType == sUBSTANCEtAGtYPEsOLIDcOLOR     = "Solid  "
          | substanceType == sUBSTANCEtAGtYPEtEXTURE        = "Texture"
          | substanceType == sUBSTANCEtAGtYPElINEARgRADIENT = "Linear "
          | substanceType == sUBSTANCEtAGtYPErADIALgRADIENT = "Radial "
          | otherwise = error "substanceType " ++ show substanceType ++ " not supported"
    in  typeString ++ " ref " ++ show ref
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

instance StorableM (Substance PictureMemoryReference SubSpace) where
  sizeOfM substance =
    case substance of
        Solid color            -> sizeOfM color
        Texture pictUse        -> sizeOfM pictUse
        Linear linearGradient  -> sizeOfM linearGradient
        Radial radialGradient  -> sizeOfM radialGradient
        TransformSubstance a b -> error "cannot store TransformSubstance"
  alignmentM substance =
      case substance of
          Solid color            -> alignmentM color
          Texture pictUse        -> alignmentM pictUse
          Linear linearGradient  -> alignmentM linearGradient
          Radial radialGradient  -> alignmentM radialGradient
          TransformSubstance a b -> error "cannot store TransformSubstance"
  peekM = error "cannot peek substance"
  pokeM substance =
    case substance of
        Solid color            -> pokeM color
        Texture pictUse        -> pokeM pictUse
        Linear linearGradient  -> pokeM linearGradient
        Radial radialGradient  -> pokeM radialGradient
        TransformSubstance a b -> error "cannot store TransformSubstance"

instance Storable (Substance PictureMemoryReference SubSpace) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
