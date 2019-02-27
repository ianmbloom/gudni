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
  , leftSide
  , topSide
  , rightSide
  , bottomSide
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
  , Boxable
  ) where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.DeepSeq
import Foreign.Storable
import Foreign.C.Types

import Control.Lens

--import Data.Primitive.SIMD
import Data.Traversable
import qualified Data.Vector.Storable as VS


pointToBox :: Num s => Point2 s -> Box s
pointToBox p = makeBox zeroPoint p



data Box s = Box !(Point2 s) !(Point2 s) deriving (Eq, Show)


topLeftBox     :: Lens' (Box s) (Point2 s)
topLeftBox elt_fn (Box topLeft bottomRight) = (\topLeft' -> Box topLeft' bottomRight) <$> elt_fn topLeft
bottomRightBox :: Lens' (Box s) (Point2 s)
bottomRightBox elt_fn (Box topLeft bottomRight) = (\bottomRight' -> Box topLeft bottomRight') <$> elt_fn bottomRight
topRightBox :: Lens' (Box s) (Point2 s)
topRightBox   elt_fn (Box (Point2 left top) (Point2 right bottom)) =
  (\(Point2 right' top') -> Box (Point2 left top') (Point2 right' bottom)) <$> elt_fn (Point2 right top)
bottomLeftBox :: Lens' (Box s) (Point2 s)
bottomLeftBox elt_fn (Box (Point2 left top) (Point2 right bottom)) =
  (\(Point2 left' bottom') -> Box (Point2 left' top) (Point2 right bottom')) <$> elt_fn (Point2 left bottom)
leftSide       :: Lens' (Box s) (Ortho XDimension s)
leftSide = topLeftBox . pX
rightSide      :: Lens' (Box s) (Ortho XDimension s)
rightSide = bottomRightBox . pX
topSide         :: Lens' (Box s) (Ortho YDimension s)
topSide  = topLeftBox . pY
bottomSide      :: Lens' (Box s) (Ortho YDimension s)
bottomSide = bottomRightBox . pY
makeBox        :: Point2 s -> Point2 s -> Box s
makeBox  = Box
emptyBox       :: Num s => Box s
emptyBox = Box zeroPoint zeroPoint
scaleBox       :: Num s => s -> Box s -> Box s
scaleBox  scale (Box topLeft bottomRight) = makeBox (topLeft ^* scale) (bottomRight ^* scale)
translateBox   :: Num s => Point2 s -> Box s -> Box s
translateBox delta = mapBox (^+^ delta)

heightBox :: Num s => Box s -> Ortho YDimension s
heightBox box = box ^. bottomSide - box ^. topSide
widthBox :: Num s => Box s -> Ortho XDimension s
widthBox  box = box ^. rightSide - box ^. leftSide
sizeBox :: Num s => Box s -> Point2 s
sizeBox   box = makePoint (widthBox box ) (heightBox box)
areaBox :: Num s => Box s -> s
areaBox   box = unOrtho (widthBox box) * unOrtho (heightBox box)
unionBox :: (Num s, Ord s) => Box s -> Box s -> Box s
unionBox a b = makeBox (makePoint (min (a ^. leftSide) (b ^. leftSide)) (min (b ^. topSide) (b ^. topSide))) (makePoint (max (b ^. rightSide) (b ^. rightSide)) (max (a ^. bottomSide) (b ^. bottomSide)))

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

isZeroBox :: (Num s, Eq s) => Box s -> Bool
isZeroBox b = (widthBox b == 0) && (heightBox b == 0)

mapBox :: (Point2 s -> Point2 s) -> Box s -> Box s
mapBox f box = makeBox (f $ box ^. topLeftBox) (f $ box ^. bottomRightBox)


boxBoxes :: (Num s, Ord s) => [Box s] -> Box s
boxBoxes list =
  if null list
  then emptyBox
  else foldr1 unionBox list

boxPointVector :: (Storable (Point2 s), Storable s, Ord s) => VS.Vector (Point2 s) -> Box s
boxPointVector vs =
  let top    = VS.minimum (VS.map (view pY) vs)
      bottom = VS.maximum (VS.map (view pY) vs)
      left   = VS.minimum (VS.map (view pX) vs)
      right  = VS.maximum (VS.map (view pX) vs)
  in makeBox (makePoint left top) (makePoint right bottom)

boxPointList :: (Show s, Ord s, Num s) => [Point2 s] -> Box s
boxPointList vs =
  let top    = minimum (map (view pY) vs)
      bottom = maximum (map (view pY) vs)
      left   = minimum (map (view pX) vs)
      right  = maximum (map (view pX) vs)
  in if null vs
     then emptyBox -- TODO make this a maybe
     else makeBox (makePoint left top) (makePoint right bottom)

class Boxable t s where
  getBoundingBox :: t -> Box s
{-
intersectBox :: Ord a => Boxlike a -> Boxlike a -> Boxlike a
intersectBox (Boxlike (Point x0 y0) (Point x1 y1)) (Boxlike (Point x0' y0') (Point x1' y1')) = Boxlike (Point (max x0 x0') (max y0 y0')) (Point (min x1 x1') (min y1 y1'))

possibleOverlap :: (Ord s) => Boxlike s -> Boxlike s -> Bool
possibleOverlap a b = not $ ((leftSide a > rightSide b) || (rightSide a < leftSide b)) && ((topSide a < bottomSide b) || (bottomSide a > topSide b))
-}
