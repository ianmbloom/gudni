{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE DatatypeContexts      #-}

module Graphics.Gudni.Figure.Box
  ( Box (..)
  , topRightBox
  , bottomLeftBox
  , topLeftBox
  , bottomRightBox
  , boxBoxes
  , boxPointList
  , boxPointVector
  , leftBox
  , topBox
  , rightBox
  , bottomBox
  , mapBox
  , heightBox
  , widthBox
  , sizeBox
  , areaBox
  , unionBox
  , pointToBox
  , emptyBox
  , translateBox
  , scaleBox
  , makeBox
  ) where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.DeepSeq
import Foreign.Storable
import Foreign.C.Types

--import Data.Primitive.SIMD
import Data.Traversable
import qualified Data.Vector.Storable as VS


pointToBox :: Num s => Point2 s -> Box s
pointToBox p = makeBox zeroPoint p

topRightBox ::Num s => Box s -> Point2 s
topRightBox   box = makePoint (rightBox box) (topBox box)
bottomLeftBox :: Num s => Box s -> Point2 s
bottomLeftBox box = makePoint (leftBox box)  (bottomBox box)


data Box s = Box !(Point2 s) !(Point2 s) deriving (Eq, Show)


topLeftBox     :: Box s -> Point2 s
topLeftBox     (Box topLeft _          ) = topLeft
bottomRightBox :: Box s -> Point2 s
bottomRightBox (Box _       bottomRight) = bottomRight
leftBox        :: Box s -> Ortho XDimension s
leftBox        (Box topLeft bottomRight) = pX topLeft

rightBox       :: Box s -> Ortho XDimension s
rightBox       (Box topLeft bottomRight) = pX bottomRight
topBox         :: Box s -> Ortho YDimension s
topBox         (Box topLeft bottomRight) = pY topLeft
bottomBox      :: Box s -> Ortho YDimension s
bottomBox      (Box topLeft bottomRight) = pY bottomRight
makeBox        :: Point2 s -> Point2 s -> Box s
makeBox  = Box
emptyBox       :: Num s => Box s
emptyBox = Box zeroPoint zeroPoint
scaleBox       :: Num s => s -> Box s -> Box s
scaleBox  scale (Box topLeft bottomRight) = makeBox (topLeft ^* scale) (bottomRight ^* scale)
translateBox   :: Num s => Point2 s -> Box s -> Box s
translateBox delta = mapBox (^+^ delta)

heightBox :: Num s => Box s -> Ortho YDimension s
heightBox box = bottomBox box - topBox  box
widthBox :: Num s => Box s -> Ortho XDimension s
widthBox  box = rightBox  box - leftBox box
sizeBox :: Num s => Box s -> Point2 s
sizeBox   box = makePoint (widthBox box ) (heightBox box)
areaBox :: Num s => Box s -> s
areaBox   box = unOrtho (widthBox box) * unOrtho (heightBox box)
unionBox :: (Num s, Ord s) => Box s -> Box s -> Box s
unionBox a b = makeBox (makePoint (min (leftBox a) (leftBox b)) (min (topBox a) (topBox b))) (makePoint (max (rightBox a) (rightBox b)) (max (bottomBox a) (bottomBox b)))

instance NFData s => NFData (Box s) where
  rnf (Box a b) = a `deepseq` b `deepseq` ()

instance (Storable s) => Storable (Box s) where
  sizeOf _ = 2 * sizeOf (undefined :: Point2 s)
  alignment _ = alignment (undefined :: Point2 s)
  peek ptr =
    do
      let sz = sizeOf (undefined :: Point2 s)
      topLeft     <- peekByteOff ptr 0
      bottomRight <- peekByteOff ptr sz
      return (Box topLeft bottomRight)
  poke ptr (Box topLeft bottomRight) =
    do
      let sz = sizeOf (undefined :: Point2 s)
      pokeByteOff ptr 0  topLeft
      pokeByteOff ptr sz bottomRight

instance Convertable a b => Convertable (Box a) (Box b) where
  convert (Box a b) = Box (convert a) (convert b)

isZeroBox :: (Eq s) => Box s -> Bool
isZeroBox b = (leftBox b == rightBox b) && (topBox b == bottomBox b)

mapBox :: (Point2 s -> Point2 s) -> Box s -> Box s
mapBox f box = makeBox (f . topLeftBox $ box) (f . bottomRightBox $ box)


boxBoxes :: (Num s, Ord s) => [Box s] -> Box s
boxBoxes list =
  if null list
  then emptyBox
  else foldr1 unionBox list

boxPointVector :: (Storable (Point2 s), Storable s, Ord s) => VS.Vector (Point2 s) -> Box s
boxPointVector vs =
  let top    = VS.minimum (VS.map pY vs)
      bottom = VS.maximum (VS.map pY vs)
      left   = VS.minimum (VS.map pX vs)
      right  = VS.maximum (VS.map pX vs)
  in makeBox (makePoint left top) (makePoint right bottom)

boxPointList :: (Show s, Ord s, Num s) => [Point2 s] -> Box s
boxPointList vs =
  let top    = minimum (map pY vs)
      bottom = maximum (map pY vs)
      left   = minimum (map pX vs)
      right  = maximum (map pX vs)
  in if null vs
     then emptyBox -- TODO make this a maybe
     else makeBox (makePoint left top) (makePoint right bottom)

class Boxable t s where
  getBoundingBox :: t -> Box s
{-
intersectBox :: Ord a => Boxlike a -> Boxlike a -> Boxlike a
intersectBox (Boxlike (Point x0 y0) (Point x1 y1)) (Boxlike (Point x0' y0') (Point x1' y1')) = Boxlike (Point (max x0 x0') (max y0 y0')) (Point (min x1 x1') (min y1 y1'))

possibleOverlap :: (Ord s) => Boxlike s -> Boxlike s -> Bool
possibleOverlap a b = not $ ((leftBox a > rightBox b) || (rightBox a < leftBox b)) && ((topBox a < bottomBox b) || (bottomBox a > topBox b))
-}
