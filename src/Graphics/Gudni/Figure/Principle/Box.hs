{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE Rank2Types            #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Principle.Box
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple box type for bounding boxes.

module Graphics.Gudni.Figure.Principle.Box
  ( Box (..)
  , CanBox(..)
  , widthOf
  , heightOf
  , pattern Box
  , topRightBox
  , bottomLeftBox
  , topLeftBox
  , minBox
  , bottomRightBox
  , maxBox
  , leftSide
  , topSide
  , rightSide
  , bottomSide
  , acrossBox
  , widthBox
  , heightBox
  , mapBox
  , splitBox
  , sizeBox
  , areaBox
  , minMaxBox
  , minMaxBoxes
  , sizeToBox
  , emptyBox
  , makeBox
  , boxAroundPoints
  , boxToV4Points
  , addMarginsBox
  , excludesBox
  , includesBox
  , constrainBoundary
  , constrainPoint
  , constrainBox
  ) where

import Graphics.Gudni.Figure.Principle.Space
import Graphics.Gudni.Figure.Principle.Axis
import Graphics.Gudni.Figure.Principle.Point
import Graphics.Gudni.Figure.Principle.ArcLength

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug

import Control.Lens
import Control.Applicative

import Data.Traversable
import Data.Hashable
import qualified Data.Vector.Storable as VS
import Linear.V2
import Linear.V3
import Linear.V4
import Text.PrettyPrint.GenericPretty

-- | Newtype wrapper for box types.
newtype Box s = Bx {unBx :: V2 (Point2 s)} deriving (Eq, Ord, Generic)
-- | Pattern for taking apart boxes
pattern Box topLeft bottomRight = Bx (V2 topLeft bottomRight)
instance (Out s) => Out (Box s)

instance (Space s) => HasSpace (Box s) where
  type SpaceOf (Box s) = s

class CanBox t where
  boxOf :: t -> Box (SpaceOf t)

instance CanBox (Box s) where
  boxOf box = box

instance CanBox (Point2 s) where
  boxOf p = Box p p

instance (HasSpace t, CanBox t) => CanBox (V2 t) where
  boxOf = minMaxBoxes . fmap boxOf

instance (HasSpace t, CanBox t) => CanBox (V3 t) where
  boxOf = minMaxBoxes . fmap boxOf

instance (HasSpace t, CanBox t) => CanBox (V4 t) where
  boxOf = minMaxBoxes . fmap boxOf

-- | Get the width of a box.
widthOf :: (Num (SpaceOf a), CanBox a) => a -> SpaceOf a
widthOf a = let box = boxOf a in fromAlong Horizontal $ box ^. rightSide - box ^. leftSide

-- | Get the height of a box.
heightOf :: (Num (SpaceOf a), CanBox a) => a -> SpaceOf a
heightOf a = let box = boxOf a in fromAlong Vertical $ box ^. bottomSide - box ^. topSide

-----------------------------------------------------------------------------
-- Box optics.

-- | 'Lens' for the top left point of a box.
topLeftBox     :: Lens' (Box s) (Point2 s)
topLeftBox elt_fn (Box topLeft bottomRight) = (\topLeft' -> Box topLeft' bottomRight) <$> elt_fn topLeft
-- | 'Lens' for the bottom right point of a box.
bottomRightBox :: Lens' (Box s) (Point2 s)
bottomRightBox elt_fn (Box topLeft bottomRight) = (\bottomRight' -> Box topLeft bottomRight') <$> elt_fn bottomRight

minBox :: Lens' (Box s) (Point2 s)
minBox = topLeftBox
maxBox :: Lens' (Box s) (Point2 s)
maxBox = bottomRightBox

-- | 'Lens' for the top right corner of a box.
topRightBox :: Lens' (Box s) (Point2 s)
topRightBox   elt_fn (Box (Point2 left top) (Point2 right bottom)) =
  (\(Point2 right' top') -> Box (Point2 left top') (Point2 right' bottom)) <$> elt_fn (Point2 right top)
-- | 'Lens' for the bottom left corner of a box.
bottomLeftBox :: Lens' (Box s) (Point2 s)
bottomLeftBox elt_fn (Box (Point2 left top) (Point2 right bottom)) =
  (\(Point2 left' bottom') -> Box (Point2 left' top) (Point2 right bottom')) <$> elt_fn (Point2 left bottom)

acrossBox :: (Num s, Axis axis) => axis -> Lens' (Box s) (Along axis s)
acrossBox axis elt_fn box =
    (\across -> set (maxBox . along axis) (box ^. minBox . along axis + across) box)
    <$> elt_fn (box ^. maxBox . along axis - box ^. minBox . along axis)

widthBox :: Num s => Lens' (Box s) (Ax Horizontal s)
widthBox = acrossBox Horizontal

heightBox :: Num s => Lens' (Box s) (Ax Vertical s)
heightBox = acrossBox Vertical

-- | 'Lens' for the left side of a box.
leftSide       :: Lens' (Box s) (Ax Horizontal s)
leftSide = topLeftBox . pX
-- | 'Lens' for the right side of a box.
rightSide      :: Lens' (Box s) (Ax Horizontal s)
rightSide = bottomRightBox . pX
-- | 'Lens' for the top of a box.
topSide         :: Lens' (Box s) (Ax Vertical s)
topSide  = topLeftBox . pY
-- | 'Lens' for the bottom of a box.
bottomSide      :: Lens' (Box s) (Ax Vertical s)
bottomSide = bottomRightBox . pY

-----------------------------------------------------------------------------
-- Functions for creating and manipulating boxes∘

-- | Make a box from its four sides.
makeBox        :: Ax Horizontal s
               -> Ax Vertical   s
               -> Ax Horizontal s
               -> Ax Vertical   s
               -> Box s
makeBox l t r b = Box (makePoint l t) (makePoint r b)

boxAroundPoints :: Ord s => Point2 s -> Point2 s -> Box s
boxAroundPoints a b = makeBox (min (a ^. pX) (b ^. pX))
                              (min (a ^. pY) (b ^. pY))
                              (max (a ^. pX) (b ^. pX))
                              (max (a ^. pY) (b ^. pY))

boxAroundPoint :: Point2 s -> Box s
boxAroundPoint p = Box p p
-- | Make a box from the origin to a point.
sizeToBox :: Num s => Point2 s -> Box s
sizeToBox p = Box zeroPoint p

-- | Make an empty box at the origin.
emptyBox       :: Num s => Box s
emptyBox = Box zeroPoint zeroPoint
-- | Map over the top left and bottom right points of the box.
mapBox :: (Point2 t -> Point2 s) -> Box t -> Box s
mapBox f (Box tl br) = Box (f tl) (f br)

splitBox :: (Space s, Axis axis) => axis -> Athwart axis s -> Box s -> (Box s, Box s)
splitBox axis cutPoint box =
    ( set (maxBox . athwart axis) cutPoint box
    , set (minBox . athwart axis) cutPoint box
    )

-- | True if the box has zero height and zero width.
isZeroBox :: (Num s, Eq s) => Box s -> Bool
isZeroBox b = (widthOf b == 0) && (heightOf b == 0)
-- | Get a point that represents the height and width of the box.
sizeBox :: (Num s) => Box s -> Point2 s
sizeBox   box = makePoint (box ^. widthBox) (box ^. heightBox)
-- | Calculate the area of a box.
areaBox :: (CanBox (Box s), Num s) => Box s -> s
areaBox   box = widthOf box * heightOf box
-- | Calculate the smallest box that contains two boxes.
minMaxBox :: (SimpleSpace s) => Box s -> Box s -> Box s
minMaxBox a b = makeBox (min (a ^. leftSide ) (b ^. leftSide )) (min (a ^. topSide   ) (b ^. topSide   ))
                        (max (a ^. rightSide) (b ^. rightSide)) (max (a ^. bottomSide) (b ^. bottomSide))

minMaxBoxes :: (Space s, Foldable t) => t (Box s) -> Box s
minMaxBoxes = foldl minMaxBox (Box maxPoint minPoint)
  where  minPoint = Point2 minBound minBound
         maxPoint = Point2 maxBound maxBound

instance (Hashable s) => Hashable (Box s) where
    hashWithSalt s (Box tl br) = s `hashWithSalt` tl `hashWithSalt` br



sd = showFl . realToFrac

instance Show s => Show (Box s) where
  show (Box (Point2 left top) (Point2 right bottom)) = "Bx l" ++ show left ++ ", t" ++ show top ++ ", r" ++ show right ++ ", b" ++ show bottom

instance {-# OVERLAPS #-} Show (Box SubSpace) where
  show (Box (Point2 left top) (Point2 right bottom)) = "Bx l" ++ sd left ++ ", t" ++ sd top ++ ", r" ++ sd right ++ ", b" ++ sd bottom

boxToV4Points :: Box s -> V4 (Point2 s)
boxToV4Points box = V4 (box ^. topLeftBox) (box ^. topRightBox) (box ^. bottomRightBox) (box ^. bottomLeftBox)

addMarginsBox :: Space s => s -> Box s -> Box s
addMarginsBox scale = let margin =  Point2 scale scale
                      in  over minBox (`subtract` margin) . over maxBox (+ margin)

excludesBox :: Ord s
            => Box s
            -> Box s
            -> Bool
excludesBox boundary box =
       box ^. minBox . pX  >= boundary ^. maxBox . pX
    || box ^. minBox . pY  >= boundary ^. maxBox . pY
    || box ^. maxBox . pX  <= boundary ^. minBox . pX
    || box ^. maxBox . pY  <= boundary ^. minBox . pY

includesBox :: Ord s
            => Box s
            -> Box s
            -> Bool
includesBox boundary = not . excludesBox boundary

constrainBoundary :: Space s => Box s
constrainBoundary =
    let highValue = 2 ^ 16
        highPoint = pure highValue
    in  Box (negate highPoint) highPoint

constrainPoint p =
    makePoint (min (constrainBoundary ^. rightSide  ) (max (constrainBoundary ^. leftSide) (p ^. pX   )))
              (min (constrainBoundary ^. bottomSide ) (max (constrainBoundary ^. topSide ) (p ^. pY   )))

constrainBox box =
  makeBox (max (constrainBoundary ^. leftSide   ) (box ^. leftSide   ))
          (max (constrainBoundary ^. topSide    ) (box ^. topSide    ))
          (min (constrainBoundary ^. rightSide  ) (box ^. rightSide  ))
          (min (constrainBoundary ^. bottomSide ) (box ^. bottomSide ))
