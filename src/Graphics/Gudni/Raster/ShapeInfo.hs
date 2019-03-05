{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
    , _shapeCombine       :: CombineType
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
mAXsHAPEID = sHAPEiDbITMASK - 1 :: CULong

-- | A shape header includes the id and tags.
data SubstanceInfo = SubstanceInfo
   { substanceInfoSubstance :: Substance PictId -- the information used to color the interior of the shape.
   } deriving Show

-- | Get the substancetype flag from a substance.
substanceToSubstanceType :: Substance a -> SubstanceType
substanceToSubstanceType (Solid  _ ) = SubstanceSolidColor
substanceToSubstanceType (Texture _) = SubstancePicture

-- | Make a shapeTag
makeShapeTag :: ShapeInfo -> ShapeTag
makeShapeTag (ShapeInfo substance combine groupId) =
  let substanceFlag = case substance of
                          SubstanceSolidColor -> sHAPETAGsUBSTANCEtYPEsOLIDcOLOR
                          SubstancePicture    -> sHAPETAGsUBSTANCEtYPEpICTURE
      combineFlag   = case combine of
                          CombineContinue     -> sHAPETAGcOMBINEtYPEcONTINUE
                          CombineAdd          -> sHAPETAGcOMBINEtYPEaDD
                          CombineSubtract     -> sHAPETAGcOMBINEtYPEsUBTRACT
  in  if unSubstanceId groupId <= mAXsHAPEID
      then ShapeTag (substanceFlag .|. combineFlag .|. (unSubstanceId groupId .&. sHAPEiDbITMASK))
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

-- | Extract the 'CombineType' from a 'ShapeTag'.
tagToCombineType :: ShapeTag -> CombineType
tagToCombineType tag = tagBitsToCombineType (unShapeTag tag .&. sHAPETAGcOMBINEtYPEbITmASK)

-- | Select the 'CombineType' from the masked 'ShapeTag_'.
tagBitsToCombineType :: ShapeTag_ -> CombineType
tagBitsToCombineType tagBits
  | tagBits == sHAPETAGcOMBINEtYPEcONTINUE = CombineContinue
  | tagBits == sHAPETAGcOMBINEtYPEaDD      = CombineAdd
  | tagBits == sHAPETAGcOMBINEtYPEsUBTRACT = CombineSubtract
  | otherwise = error "bitMask does not correspond to a valid CombineType."

-- | Extract the 'SubstanceId' from the 'ShapeTag'.
tagToSubstanceId :: ShapeTag -> SubstanceId
tagToSubstanceId tag = SubstanceId (unShapeTag tag .&. sHAPEiDbITMASK)

-- | Extract the 'ShapeInfo' from the 'ShapeTag'.
extractShapeInfo :: ShapeTag -> ShapeInfo
extractShapeInfo tag = let substanceType = tagToSubstanceType tag
                           combineType   = tagToCombineType tag
                           groupId       = tagToSubstanceId tag
                      in  ShapeInfo substanceType combineType groupId

-- * Instances
instance NFData SubstanceInfo where
  rnf (SubstanceInfo a ) = a `deepseq` ()

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
