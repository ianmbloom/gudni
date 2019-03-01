{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}
-- {-# LANGUAGE TemplateHaskell       #-}
-- {-# LANGUAGE DatatypeContexts      #-}

module Graphics.Gudni.Figure.Box
  ( Box (..)
  , Boxable(..)
  , BoundingBox(..)
  , pattern Box
  , topRightBox
  , bottomLeftBox
  , topLeftBox
  , bottomRightBox
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
  , makeBox
  ) where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Transformer

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.DeepSeq
import Foreign.Storable
import Foreign.C.Types

import Control.Lens

--import Data.Primitive.SIMD
import Data.Traversable
import qualified Data.Vector.Storable as VS
import Linear.V2

-- | Newtype wrapper for box types.
newtype Box s = Bx {unBx :: V2 (Point2 s)} deriving (Eq, Show)
-- | Pattern for taking apart boxes
pattern Box topLeft bottomRight = Bx (V2 topLeft bottomRight)

-- | Type synonym for bounding boxes
type BoundingBox = Box DisplaySpace

-- | 'Lens' for the top left point of a box.
topLeftBox     :: Lens' (Box s) (Point2 s)
topLeftBox elt_fn (Box topLeft bottomRight) = (\topLeft' -> Box topLeft' bottomRight) <$> elt_fn topLeft
-- | 'Lens' for the bottom right point of a box.
bottomRightBox :: Lens' (Box s) (Point2 s)
bottomRightBox elt_fn (Box topLeft bottomRight) = (\bottomRight' -> Box topLeft bottomRight') <$> elt_fn bottomRight
-- | 'Lens' for the top right corner of a box.
topRightBox :: Lens' (Box s) (Point2 s)
topRightBox   elt_fn (Box (Point2 left top) (Point2 right bottom)) =
  (\(Point2 right' top') -> Box (Point2 left top') (Point2 right' bottom)) <$> elt_fn (Point2 right top)
-- | 'Lens' for the bottom left corner of a box.
bottomLeftBox :: Lens' (Box s) (Point2 s)
bottomLeftBox elt_fn (Box (Point2 left top) (Point2 right bottom)) =
  (\(Point2 left' bottom') -> Box (Point2 left' top) (Point2 right bottom')) <$> elt_fn (Point2 left bottom)
-- | 'Lens' for the left side of a box.
leftSide       :: Lens' (Box s) (Ortho XDimension s)
leftSide = topLeftBox . pX
-- | 'Lens' for the right side of a box.
rightSide      :: Lens' (Box s) (Ortho XDimension s)
rightSide = bottomRightBox . pX
-- | 'Lens' for the top of a box.
topSide         :: Lens' (Box s) (Ortho YDimension s)
topSide  = topLeftBox . pY
-- | 'Lens' for the bottom of a box.
bottomSide      :: Lens' (Box s) (Ortho YDimension s)
bottomSide = bottomRightBox . pY
-- | Make a box from its four sides.
makeBox        :: Ortho XDimension s -> Ortho YDimension s -> Ortho XDimension s -> Ortho YDimension s -> Box s
makeBox l t r b = Box (makePoint l t) (makePoint r b)
-- | Make a box from the origin to a point.
pointToBox :: Num s => Point2 s -> Box s
pointToBox p = Box zeroPoint p
-- | Make an empty box at the origin.
emptyBox       :: Num s => Box s
emptyBox = Box zeroPoint zeroPoint
-- | Map over the top left and bottom right points of the box.
mapBox :: (Point2 s -> Point2 s) -> Box s -> Box s
mapBox f (Box tl br) = Box (f tl) (f br)

-- | Get the height of a box.
heightBox :: Num s => Box s -> Ortho YDimension s
heightBox box = box ^. bottomSide - box ^. topSide
-- | Get the width of a box.
widthBox :: Num s => Box s -> Ortho XDimension s
widthBox  box = box ^. rightSide - box ^. leftSide
-- | True if the box has zero height and zero width.
isZeroBox :: (Num s, Eq s) => Box s -> Bool
isZeroBox b = (widthBox b == 0) && (heightBox b == 0)
-- | Get a point that represents the height and width of the box.
sizeBox :: Num s => Box s -> Point2 s
sizeBox   box = makePoint (widthBox box ) (heightBox box)
-- | Calculate the area of a box.
areaBox :: Num s => Box s -> s
areaBox   box = unOrtho (widthBox box) * unOrtho (heightBox box)
-- | Calculate the smallest box that contains two boxes.
unionBox :: (Num s, Ord s) => Box s -> Box s -> Box s
unionBox a b = makeBox (min (a ^. leftSide ) (b ^. leftSide )) (min (b ^. topSide   ) (b ^. topSide   ))
                       (max (b ^. rightSide) (b ^. rightSide)) (max (a ^. bottomSide) (b ^. bottomSide))

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

-- | Typeclass for things that can calculate a bounding box.
class Boxable t where
  getBoundingBox :: t -> BoundingBox

instance Boxable [BoundingBox] where
  getBoundingBox list =
      if null list
      then emptyBox
      else foldr1 unionBox list

instance Boxable (VS.Vector (Point2 DisplaySpace)) where
  getBoundingBox vs =
      let top    = VS.minimum (VS.map (view pY) vs)
          bottom = VS.maximum (VS.map (view pY) vs)
          left   = VS.minimum (VS.map (view pX) vs)
          right  = VS.maximum (VS.map (view pX) vs)
      in makeBox left top right bottom

instance Boxable [Point2 DisplaySpace] where
  getBoundingBox vs =
      let top    = minimum (map (view pY) vs)
          bottom = maximum (map (view pY) vs)
          left   = minimum (map (view pX) vs)
          right  = maximum (map (view pX) vs)
      in if null vs
         then emptyBox -- TODO make this a maybe
         else makeBox left top right bottom

instance SimpleTransformable (BoundingBox) where
  tTranslate p = mapBox (^+^ p)
  tScale     s = mapBox (^* s)
