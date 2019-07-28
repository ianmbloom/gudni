{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.ShapeInfo
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Constructors for attaching metadata to shapes∘

module Graphics.Gudni.Raster.ShapeInfo
  ( ShapeInfo (..)
  , SubstanceInfo (..)
  , SubstanceId (..)
  , shapeCombine
  , SubstanceType(..)
  , substanceToSubstanceType
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM

import Graphics.Gudni.Raster.Constants
import Data.Bits
import Numeric

import Control.Lens
import Control.DeepSeq

import Foreign.C.Types (CUInt, CULong)
import Foreign.Storable
import Foreign.Ptr

-- | SubstanceType defines a flag for the rendering kernel to select a coloring algorithm.
data SubstanceType = SubstanceSolidColor | SubstancePicture deriving (Show, Ord, Eq)

-- | A group id is unique value assigned to each shape.
type SubstanceId_ = CULong
newtype SubstanceId = SubstanceId {unSubstanceId :: SubstanceId_} deriving (Ord, Eq, Num, Enum)
instance Show SubstanceId where
    show (SubstanceId i) = show i ++ "sid"

-- | ShapeInfo includes substance flags and the combination method for a particular shape.
data ShapeInfo = ShapeInfo
    { _shapeSubstanceType :: SubstanceType
    , _shapeCombine       :: Compound
    , _shapeSubstanceId   :: SubstanceId
    } deriving (Show)
makeLenses ''ShapeInfo

-- | A shape tag is the shape value combined with flags
-- The layout of the ShapeTag flags is currently as follows from the leftmost bit to right.
-- Bit 31 - 30 - Determines the method used to color the interior of the shape. (Solid or bitmap)
-- Bit 29 - 28 - Determines the combine method of the shape, continuation, add or subtract.
-- Bit 27 - 0  - Encodes the shape id∘

type ShapeTag_ = CULong
newtype ShapeTag = ShapeTag {unShapeTag :: ShapeTag_} deriving (Ord, Eq, Num, Enum)

-- | The max shapeId is determined by the maximum number of unique values that can be stored in the bottom
-- 27 bits of the ShapeTag. Currently the max shapeId is 268,435,455
mAXsHAPEID = sHAPETAGsUBSTANCEIDbITMASK - 1 :: CULong

-- | A shape header includes the id and tags.
newtype SubstanceInfo = SubstanceInfo
   { substanceInfoSubstance :: Substance PictId -- the information used to color the interior of the shape.
   } deriving Show

instance NFData SubstanceInfo where
  rnf (SubstanceInfo a) = rnf a

-- | Get the substancetype flag from a substance.
substanceToSubstanceType :: Substance n -> SubstanceType
substanceToSubstanceType (Solid   {}) = SubstanceSolidColor
substanceToSubstanceType (Texture {}) = SubstancePicture

-- | Make a shapeTag
makeShapeTag :: ShapeInfo -> ShapeTag
makeShapeTag (ShapeInfo substance combine groupId) =
  let substanceFlag = case substance of
                          SubstanceSolidColor -> sHAPETAGsUBSTANCEtYPEsOLIDcOLOR
                          SubstancePicture    -> sHAPETAGsUBSTANCEtYPEpICTURE
      combineFlag   = case combine of
                          CompoundContinue     -> sHAPETAGcOMPOUNDtYPEcONTINUE
                          CompoundAdd          -> sHAPETAGcOMPOUNDtYPEaDD
                          CompoundSubtract     -> sHAPETAGcOMPOUNDtYPEsUBTRACT
  in  if unSubstanceId groupId <= mAXsHAPEID
      then ShapeTag (substanceFlag .|. combineFlag .|. (unSubstanceId groupId .&. sHAPETAGsUBSTANCEIDbITMASK))
      else error "shapeID out of bounds"

-- | Extract the substancetype from a ShapeTag.
tagToSubstanceType :: ShapeTag -> SubstanceType
tagToSubstanceType tag = tagBitsToSubstanceType (unShapeTag tag .&. sHAPETAGsUBSTANCEtYPEbITmASK)

-- | Select the 'SubstanceType' from the masked 'ShapeTag_'.
tagBitsToSubstanceType :: ShapeTag_ -> SubstanceType
tagBitsToSubstanceType tagBits
  | tagBits == sHAPETAGsUBSTANCEtYPEsOLIDcOLOR = SubstanceSolidColor
  | tagBits == sHAPETAGsUBSTANCEtYPEpICTURE    = SubstancePicture
  | otherwise = error "bitMask does not correspond to a valid SubstanceType."

-- | Extract the 'Compound' from a 'ShapeTag'.
tagToCompound :: ShapeTag -> Compound
tagToCompound tag = tagBitsToCompound (unShapeTag tag .&. sHAPETAGcOMPOUNDtYPEbITmASK)

-- | Select the 'Compound' from the masked 'ShapeTag_'.
tagBitsToCompound :: ShapeTag_ -> Compound
tagBitsToCompound tagBits
  | tagBits == sHAPETAGcOMPOUNDtYPEcONTINUE = CompoundContinue
  | tagBits == sHAPETAGcOMPOUNDtYPEaDD      = CompoundAdd
  | tagBits == sHAPETAGcOMPOUNDtYPEsUBTRACT = CompoundSubtract
  | otherwise = error "bitMask does not correspond to a valid Compound."

-- | Extract the 'SubstanceId' from the 'ShapeTag'.
tagToSubstanceId :: ShapeTag -> SubstanceId
tagToSubstanceId tag = SubstanceId (unShapeTag tag .&. sHAPETAGsUBSTANCEIDbITMASK)

-- | Extract the 'ShapeInfo' from the 'ShapeTag'.
extractShapeInfo :: ShapeTag -> ShapeInfo
extractShapeInfo tag = let substanceType = tagToSubstanceType tag
                           combineType   = tagToCompound tag
                           groupId       = tagToSubstanceId tag
                      in  ShapeInfo substanceType combineType groupId

-- * Instances

instance StorableM SubstanceInfo where
  sizeOfM _ =
    do sizeOfM (undefined :: Color   ) -- picture references must fit into data size of half4.
  alignmentM _ =
    do alignmentM (undefined :: Color   )
  peekM =
    do  color <- peekM :: Offset Color
        return (SubstanceInfo (Solid color))
  pokeM (SubstanceInfo substance) =
        case substance of
               Solid color -> pokeM (color :: Color)
               Texture ref -> do pokeM (ref :: PictId)
                                 pokeM (0   :: CUInt ) -- padding

instance Storable SubstanceInfo where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

instance NFData ShapeInfo where
  rnf (ShapeInfo a b c) = a `deepseq` b `deepseq` c `deepseq` ()

instance StorableM ShapeInfo where
  sizeOfM _ =
    do sizeOfM (undefined :: ShapeTag        )
  alignmentM _ =
    do alignmentM (undefined :: ShapeTag        )
  peekM = do shapeTag <- peekM
             return $ extractShapeInfo shapeTag
  pokeM shapeInfo =
    do  let shapeTag = makeShapeTag shapeInfo
        pokeM (shapeTag :: ShapeTag)

instance Storable ShapeTag where
  sizeOf _ = sizeOf (undefined :: ShapeTag_)
  alignment _ = alignment (undefined :: ShapeTag_)
  peek ptr = ShapeTag <$> peek (castPtr ptr)
  poke ptr shapeTag = poke (castPtr ptr) (unShapeTag shapeTag)

instance NFData SubstanceType where
  rnf _ = ()

instance NFData SubstanceId where
  rnf (SubstanceId i) = i `deepseq` ()
