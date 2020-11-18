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

module Graphics.Gudni.Raster.Thresholds.SubstanceInfo
  ( DescriptionRef(..)
  , SubstanceTagId (..)
  , noSubstanceTag
  , SubstanceTag (..)
  , SubstanceTag_(..)
  , substanceAndRefToTag
  , SubstanceStorage(..)
  , substanceTagPile
  , substanceDescriptionPile
  , initSubstanceStorage
  , storeSubstance
  , loadSubstance
  , freeSubstanceStorage
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Thresholds.Constants
import Graphics.Gudni.Raster.TextureReference

import Graphics.Gudni.Util.StorableM
import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile


import Data.Bits
import Control.Lens
import Foreign.C.Types (CUInt, CChar, CULong)

import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

type DescriptionRef = Reference CChar
type SubstanceTag_  = CULong

newtype SubstanceTag
    = SubstanceTag
    { unSubstanceTag :: SubstanceTag_
    } deriving (Ord, Eq, Num, Enum)

substanceTagToType (SubstanceTag tag) = tag .&. sUBSTANCEtAGtYPEbITmASK
substanceTagToDesc (SubstanceTag tag) = Ref . fromIntegral $ tag .&. sUBSTANCEtAGrEFbITMASK

newtype SubstanceTagId = SubstanceTagId {unSubstanceTagId :: Reference SubstanceTag} deriving (Ord, Eq, Num, Enum)

noSubstanceTag = SubstanceTag nOsUBSTANCEtAG

instance Show SubstanceTagId where
    show (SubstanceTagId i) = show i ++ "sid"

data SubstanceStorage = SubstanceStorage
      -- | A pile of every substance collected from the scene.
    { _substanceTagPile :: Pile SubstanceTag
      -- | A heap of all substance descriptions
    , _substanceDescriptionPile :: BytePile
    }
makeLenses ''SubstanceStorage

-- | Make a SubstanceTag from a SubstanceInfo
substanceAndRefToTag :: Substance textureLabel s -> DescriptionRef -> SubstanceTag
substanceAndRefToTag substance descriptionRef =
    let tagType = case substance of
                      Solid   {} -> sUBSTANCEtAGtYPEsOLIDcOLOR
                      Texture {} -> sUBSTANCEtAGtYPEtEXTURE
                      Linear  {} -> sUBSTANCEtAGtYPElINEARgRADIENT
                      Radial  {} -> sUBSTANCEtAGtYPErADIALgRADIENT
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

initSubstanceStorage :: IO SubstanceStorage
initSubstanceStorage =
     do subTagPile      <- newPile
        descriptionPile <- newPile
        return  $ SubstanceStorage subTagPile descriptionPile


storeSubstance :: SubstanceStorage -> Substance PictureMemoryReference SubSpace -> IO (SubstanceTagId, SubstanceStorage)
storeSubstance (SubstanceStorage substanceTagPile descriptionPile) substance =
   do (descriptionPile', descriptionReference) <- addToPile descriptionPile (asBytes substance)
      let substanceTag = substanceAndRefToTag substance descriptionReference
      (substanceTagPile', substanceTagId) <- over _2 SubstanceTagId <$> addToPile substanceTagPile substanceTag
      return (substanceTagId, SubstanceStorage substanceTagPile' descriptionPile')

loadSubstanceType :: BytePile -> SubstanceTag_ -> Reference CChar -> IO (Substance PictureMemoryReference SubSpace)
loadSubstanceType descriptionPile substanceType descriptionPointer
     | substanceType == sUBSTANCEtAGtYPEsOLIDcOLOR     = Solid   . unAsBytes <$> fromPile descriptionPile descriptionPointer
     | substanceType == sUBSTANCEtAGtYPEtEXTURE        = Texture . unAsBytes <$> fromPile descriptionPile descriptionPointer
     | substanceType == sUBSTANCEtAGtYPElINEARgRADIENT = Linear  . unAsBytes <$> fromPile descriptionPile descriptionPointer
     | substanceType == sUBSTANCEtAGtYPErADIALgRADIENT = Radial  . unAsBytes <$> fromPile descriptionPile descriptionPointer

loadSubstance :: SubstanceStorage -> SubstanceTagId -> IO (Substance PictureMemoryReference SubSpace)
loadSubstance (SubstanceStorage subTagPile descriptionPile) substanceTagId =
   do substanceTag <- fromPile subTagPile (unSubstanceTagId substanceTagId)
      let substanceType      = substanceTagToType substanceTag
          descriptionPointer = substanceTagToDesc substanceTag
      loadSubstanceType descriptionPile substanceType descriptionPointer

freeSubstanceStorage :: SubstanceStorage -> IO ()
freeSubstanceStorage storage =
  do freePile $ storage ^. substanceTagPile
     freePile $ storage ^. substanceDescriptionPile

instance StorableM (Substance PictureMemoryReference SubSpace) where
  sizeOfM substance =
    case substance of
        Solid color            -> sizeOfM color
        Texture pictUse        -> sizeOfM pictUse
        Linear linearGradient  -> sizeOfM linearGradient
        Radial radialGradient  -> sizeOfM radialGradient
  alignmentM substance =
      case substance of
          Solid color            -> alignmentM color
          Texture pictUse        -> alignmentM pictUse
          Linear linearGradient  -> alignmentM linearGradient
          Radial radialGradient  -> alignmentM radialGradient
  peekM = error "cannot peek substance"
  pokeM substance =
    case substance of
        Solid color            -> pokeM color
        Texture pictUse        -> pokeM pictUse
        Linear linearGradient  -> pokeM linearGradient
        Radial radialGradient  -> pokeM radialGradient

instance Storable (Substance PictureMemoryReference SubSpace) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV
