{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE IncoherentInstances   #-}
-- {-# LANGUAGE TemplateHaskell       #-}
-- {-# LANGUAGE DatatypeContexts      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Box
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple box type for bounding boxes.

module Graphics.Gudni.Figure.Box
  ( Box (..)
  , BoundingBox(..)
  , HasBox(..)
  , widthOf
  , heightOf
  , pattern Box
  , topRightBox
  , bottomLeftBox
  , topLeftBox
  , bottomRightBox
  , leftSide
  , topSide
  , rightSide
  , bottomSide
  , widthBox
  , heightBox
  , mapBox
  , sizeBox
  , areaBox
  , minMaxBox
  , minMaxBoxes
  , pointToBox
  , emptyBox
  , makeBox
  ) where

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point
import Graphics.Gudni.Figure.Transformable

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Util.Debug
import Graphics.Gudni.Util.StorableM

import Control.DeepSeq
import Foreign.C.Types

import Control.Lens
import Control.Applicative

--import Data.ItemInfo.SIMD
import Data.Traversable
import Data.Hashable
import Control.DeepSeq
import qualified Data.Vector.Storable as VS
import Linear.V2

-- | Newtype wrapper for box types.
newtype Box s = Bx {unBx :: V2 (Point2 s)} deriving (Eq, Ord)
-- | Pattern for taking apart boxes
pattern Box topLeft bottomRight = Bx (V2 topLeft bottomRight)

instance (SimpleSpace s) => HasSpace (Box s) where
  type SpaceOf (Box s) = s

-- | Type synonym for bounding boxes
type BoundingBox = Box SubSpace

class HasSpace a => HasBox a where
  boxOf :: a -> Box (SpaceOf a)

instance (SimpleSpace s) => HasBox (Box s) where
  boxOf box = box

instance (SimpleSpace s) => HasBox (Point2 s) where
  boxOf p = Box p p

instance (Functor f, HasSpace (f a), SpaceOf a ~ SpaceOf (f a), Foldable f, HasBox a) => HasBox (f a) where
  boxOf = minMaxBoxes . fmap boxOf



-- | Get the width of a box.
widthOf :: (HasBox a) => a -> X (SpaceOf a)
widthOf a = let box = boxOf a in box ^. rightSide - box ^. leftSide

-- | Get the height of a box.
heightOf :: (Num (SpaceOf a), HasBox a) => a -> Y (SpaceOf a)
heightOf a = let box = boxOf a in box ^. bottomSide - box ^. topSide

-----------------------------------------------------------------------------
-- Box optics.

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

widthBox :: Num s => Lens' (Box s) (X s)
widthBox elt_fn (Box (Point2 left top) (Point2 right bottom)) =
  (\width -> Box (Point2 left top) (Point2 (left + unOrtho width) bottom)) <$> (elt_fn . Ortho) (right - left)

heightBox :: Num s => Lens' (Box s) (Y s)
heightBox elt_fn (Box (Point2 left top) (Point2 right bottom)) =
  (\height -> Box (Point2 left top) (Point2 right (top + unOrtho height))) <$> (elt_fn . Ortho) (bottom - top)
-- | 'Lens' for the left side of a box.
leftSide       :: Lens' (Box s) (X s)
leftSide = topLeftBox . pX
-- | 'Lens' for the right side of a box.
rightSide      :: Lens' (Box s) (X s)
rightSide = bottomRightBox . pX
-- | 'Lens' for the top of a box.
topSide         :: Lens' (Box s) (Y s)
topSide  = topLeftBox . pY
-- | 'Lens' for the bottom of a box.
bottomSide      :: Lens' (Box s) (Y s)
bottomSide = bottomRightBox . pY

-----------------------------------------------------------------------------
-- Functions for creating and manipulating boxes∘

-- | Make a box from its four sides.
makeBox        :: X s -> Y s -> X s -> Y s -> Box s
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

-- | True if the box has zero height and zero width.
isZeroBox :: (HasBox (Box s), Num s, Eq s) => Box s -> Bool
isZeroBox b = (widthOf b == 0) && (heightOf b == 0)
-- | Get a point that represents the height and width of the box.
sizeBox :: (HasBox (Box s), Num s) => Box s -> Point2 s
sizeBox   box = makePoint (widthOf box ) (heightOf box)
-- | Calculate the area of a box.
areaBox :: (HasBox (Box s), Num s) => Box s -> s
areaBox   box = unOrtho (widthOf box) * unOrtho (heightOf box)
-- | Calculate the smallest box that contains two boxes.
minMaxBox :: (Num s, Ord s) => Box s -> Box s -> Box s
minMaxBox a b = makeBox (min (a ^. leftSide ) (b ^. leftSide )) (min (a ^. topSide   ) (b ^. topSide   ))
                        (max (a ^. rightSide) (b ^. rightSide)) (max (a ^. bottomSide) (b ^. bottomSide))

minMaxBoxes :: (HasBox a, Functor t, Foldable t) => t a -> Box (SpaceOf a)
minMaxBoxes = foldl minMaxBox (Box maxPoint minPoint) . fmap boxOf
  where  minPoint = Point2 minBound minBound
         maxPoint = Point2 maxBound maxBound

-- Boxes have an instance for SimpleTransformable but no instance for Transformable.
instance SimpleSpace s => SimpleTransformable (Box s) where
  translateBy p = mapBox (^+^ p)
  stretchBy   p = mapBox (liftA2 (*) p)
  scaleBy     s = mapBox (^* s)

instance (Hashable s) => Hashable (Box s) where
    hashWithSalt s (Box tl br) = s `hashWithSalt` tl `hashWithSalt` br

instance NFData s => NFData (Box s) where
  rnf (Box a b) = a `deepseq` b `deepseq` ()

instance (Storable s) => StorableM (Box s) where
  sizeOfM _ = do sizeOfM (undefined :: Point2 s)
                 sizeOfM (undefined :: Point2 s)
  alignmentM _ = do alignmentM (undefined :: Point2 s)
                    alignmentM (undefined :: Point2 s)
  peekM =
    do topLeft     <- peekM
       bottomRight <- peekM
       return (Box topLeft bottomRight)
  pokeM (Box topLeft bottomRight) =
    do pokeM topLeft
       pokeM bottomRight

instance (Storable s) => Storable (Box s) where
  sizeOf = sizeOfV
  alignment = alignmentV
  peek = peekV
  poke = pokeV

sd = showFl . realToFrac

instance Show s => Show (Box s) where
  show (Box (Point2 left top) (Point2 right bottom)) = "Bx " ++ show left ++ ", " ++ show top ++ ", " ++ show right ++ ", " ++ show bottom

instance {-# OVERLAPS #-} Show BoundingBox where
  show (Box (Point2 left top) (Point2 right bottom)) = "Bx " ++ sd left ++ ", " ++ sd top ++ ", " ++ sd right ++ ", " ++ sd bottom
