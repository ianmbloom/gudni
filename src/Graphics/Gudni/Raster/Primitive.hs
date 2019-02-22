{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Graphics.Gudni.Raster.Primitive
  ( Primitive (..)
  , ShapeHeader (..)
  , GroupId (..)
  , primCombine
  , SubstanceType(..)
  , substanceToSubstanceType
  )
where

import Graphics.Gudni.Figure

import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.Pile
import Graphics.Gudni.Util.StorableM

import Data.Bits
import Numeric

import Control.Lens
import Control.DeepSeq

import Foreign.C.Types
import Foreign.Storable

----------------- GroupId -------------------

-- A shape id is unique value assigned to each shape.
type GroupId_ = CULong

newtype GroupId = GroupId {unGroupId :: GroupId_} deriving (Ord, Eq, Num, Enum)

instance Show GroupId where
  show (GroupId i) = show i ++ "sid"

instance NFData GroupId where
  rnf (GroupId i) = i `deepseq` ()

-- A shape tag is the shape value combined with flags
type ShapeTag = CULong
-- The layout of the flags is currently as follows from the leftmost bit to right.
-- Bit 31 - 30 - Determines the method used to color the interior of the shape. (Solid or bitmap)
-- Bit 29 - 28 - Determines the combine method of the shape, continuation, add or subtract.
-- Bit 27 - 0  - Encodes the shape id∘

-- Currently the max shapeId is 268,435,455

-- SubstanceType allows shapes to be colored by various algorithms.
data SubstanceType = SubstanceSolidColor | SubstancePicture deriving (Show, Ord, Eq)

substanceToSubstanceType (Solid  _ ) = SubstanceSolidColor
substanceToSubstanceType (Texture _) = SubstancePicture

instance NFData SubstanceType where
  rnf _ = ()

-- Bits 31 - 30
sHAPETAGsUBSTANCEtYPEbITmASK    = 0xC000000000000000
sHAPETAGsUBSTANCEtYPEsOLIDcOLOR = 0x8000000000000000
sHAPETAGsUBSTANCEtYPEpICTURE    = 0x4000000000000000

-- Bits 29 - 28
sHAPETAGcOMBINEtYPEbITmASK      = 0x3000000000000000
sHAPETAGcOMBINEtYPEcONTINUE     = 0x1000000000000000
sHAPETAGcOMBINEtYPEaDD          = 0x2000000000000000
sHAPETAGcOMBINEtYPEsUBTRACT     = 0x3000000000000000

-- Bits 27 - 0
sHAPEIDBITMASK                  = 0x0FFFFFFFFFFFFFFF

mAXsHAPEID = sHAPEIDBITMASK - 1

makeShapeTag :: SubstanceType -> CombineType -> GroupId -> ShapeTag
makeShapeTag substance combine (GroupId shapeId) =
  let substanceFlag = case substance of
                          SubstanceSolidColor -> sHAPETAGsUBSTANCEtYPEsOLIDcOLOR
                          SubstancePicture    -> sHAPETAGsUBSTANCEtYPEpICTURE
      combineFlag   = case combine of
                          CombineContinue     -> sHAPETAGcOMBINEtYPEcONTINUE
                          CombineAdd          -> sHAPETAGcOMBINEtYPEaDD
                          CombineSubtract     -> sHAPETAGcOMBINEtYPEsUBTRACT
  in  if shapeId <= mAXsHAPEID
      then substanceFlag .|. combineFlag .|. (shapeId .&. sHAPEIDBITMASK)
      else error "shapeID out of bounds"

tagToSubstanceType :: ShapeTag -> SubstanceType
tagToSubstanceType tag = tagBitsToSubstanceType (tag .&. sHAPETAGsUBSTANCEtYPEbITmASK)

tagBitsToSubstanceType tagBits
  | tagBits == sHAPETAGsUBSTANCEtYPEsOLIDcOLOR = SubstanceSolidColor
  | tagBits == sHAPETAGsUBSTANCEtYPEpICTURE    = SubstancePicture
  | otherwise = error "bitMask does not correspond to a valid SubstanceType."

tagToCombineType :: ShapeTag -> CombineType
tagToCombineType tag = tagBitsToCombineType (tag .&. sHAPETAGcOMBINEtYPEbITmASK)

tagBitsToCombineType tagBits
  | tagBits == sHAPETAGcOMBINEtYPEcONTINUE = CombineContinue
  | tagBits == sHAPETAGcOMBINEtYPEaDD      = CombineAdd
  | tagBits == sHAPETAGcOMBINEtYPEsUBTRACT = CombineSubtract

tagToGroupId :: ShapeTag -> GroupId
tagToGroupId tag = GroupId (tag .&. sHAPEIDBITMASK)

extractShapeTag :: ShapeTag -> (SubstanceType, CombineType, GroupId)
extractShapeTag tag = let substanceType = tagToSubstanceType tag
                          combineType   = tagToCombineType tag
                          shapeId       = tagToGroupId tag
                     in (substanceType, combineType, shapeId)

-- A shape header includes the id and tags.
data ShapeHeader = ShapeHeader
   { shapeHeaderSubstance :: Substance PictId -- the information used to color the interior of the shape.
   } deriving Show

instance NFData ShapeHeader where
  rnf (ShapeHeader a ) = a `deepseq` ()

instance StorableM ShapeHeader where
  sizeOfM _ =
    do sizeOfM (undefined :: Color   ) -- picture references must fit into data size of half4.
  alignmentM _ =
    do alignmentM (undefined :: Color   )
  peekM =
    do  color <- peekM :: Offset Color
        return (ShapeHeader (Solid color))
  pokeM (ShapeHeader substance) =
        case substance of
               Solid color -> pokeM (color :: Color)
               Texture ref -> do pokeM (ref :: PictId)
                                 pokeM (0   :: CUInt ) -- padding

instance Storable ShapeHeader where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

-- A primitive just includes transform information alongside information about how to combine
-- an enclosure with other enclosures.

data Primitive
  = Primitive
  { _primSubstance   :: SubstanceType
  , _primCombine     :: CombineType
  , _primShapeGroup  :: GroupId
  } deriving (Show)
makeLenses ''Primitive

instance NFData Primitive where
  rnf (Primitive a b c) = a `deepseq` b `deepseq` c `deepseq` ()

instance StorableM Primitive where
  sizeOfM _ =
    do sizeOfM (undefined :: ShapeTag        )
  alignmentM _ =
    do alignmentM (undefined :: ShapeTag        )
  peekM = do shapeTag <- peekM
             let (substanceType, combineType, shapeId) = extractShapeTag shapeTag
             return (Primitive substanceType combineType shapeId)
  pokeM (Primitive substanceType combineType shapeId) =
    do  let shapeTag = makeShapeTag substanceType combineType shapeId
        pokeM (shapeTag :: ShapeTag)
