{-# LANGUAGE PatternSynonyms #-}

-- A fast 2D grid allows fast mapping to and from pixel coordinates to indexes by
-- confining the possible widths of the image to powers of 2. This means a vector defined by a grid
-- will over allocate memory but have faster reverse indexing.
-- this means that the conversion from index to location does not
-- require div and mod but can be done with bitshifts instead.

module Graphics.Gudni.Image.Fast2D
  ( Location(..)
  , pattern Loc
  , BoxSize(..)
  , locationToSubSpace
  , maskBits
  , Grid2D(..)
  , newGrid2D
  , gridAllocation
  , indexToLocation
  , locationToIndex
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Data.Bits
import Data.Word
import Control.Lens

type Location = Point2 PixelSpace
pattern Loc a b = Point2 a b
type BoxSize = Location

locationToSubSpace :: Point2 PixelSpace -> Point2 SubSpace
locationToSubSpace = fmap fromIntegral

data Grid2D = Grid2D
  { gridLog :: Int
  , gridSize :: BoxSize
  }

newGrid2D :: BoxSize -> Grid2D
newGrid2D boxSize =
  let logWidth = adjustedLog (boxSize ^. pX)
      adjustedWidth = 2 ^ logWidth
  in  Grid2D
        { gridLog  = fromIntegral logWidth
        , gridSize = makePoint adjustedWidth (boxSize ^. pY)
        }

gridAllocation :: Grid2D -> Int
gridAllocation (Grid2D _ (Loc x y)) = fromIntegral $ x * y

maskBits :: Int -> Word32 -> Word32
maskBits b = (((complement $ shiftL (complement zeroBits :: Word32) b)) .&. )

indexToLocation :: Grid2D -> Int -> Location
indexToLocation grid i =
  let e = gridLog grid
      x = fromIntegral $ maskBits e (fromIntegral i :: Word32)
      y = fromIntegral $ shiftR   i e
  in  Loc x y

locationToIndex :: Grid2D -> Location -> Int
locationToIndex grid loc =
  let e = gridLog grid
      x = fromIntegral $ loc ^. pX :: Word32
      y = fromIntegral $ loc ^. pY :: Word32
      shifty = (shiftL y e)
  in  fromIntegral (shifty + x)
