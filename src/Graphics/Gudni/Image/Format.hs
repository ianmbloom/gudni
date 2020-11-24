module Graphics.Gudni.Image.Format
  ( mAXcHANNELsPACE
  , mAXcHANNELcfLOAT
  , cFloatToWord8
  , word8ToCFloat
  , colorToRGBA8
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Substance.Color
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Foreign.C.Types
import Data.Bits
import Data.Word
import Control.Lens


mAXcHANNELsPACE :: Space s => s
mAXcHANNELsPACE = 255.0

mAXcHANNELcfLOAT :: CFloat
mAXcHANNELcfLOAT = 255.0

word8ToCFloat :: Word8 -> CFloat
word8ToCFloat word = realToFrac word / mAXcHANNELcfLOAT

cFloatToWord8 :: CFloat -> Word8
cFloatToWord8 = round . (* mAXcHANNELcfLOAT)

colorToRGBA8 :: Space s => Color s -> Color Word8
colorToRGBA8 = Color . fmap (cFloatToWord8 . realToFrac) . view unColor
