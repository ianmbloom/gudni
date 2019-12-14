{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Adjacent
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Top level functions for creating bounding box based layouts.

module Graphics.Gudni.Layout.Adjacent
  ( Alignment (..)
  , Overlappable (..)
  , glyphString
  , paragraph
  , blurb
  , distributeRack
  , distributeStack
  , rack
  , stack
  , overlap
  , combineGlyph
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Glyph
import Graphics.Gudni.Layout.Font
import Graphics.Gudni.Util.Debug
import Linear
import Data.List
import Data.Char
import Data.Maybe

import Control.Lens
import Control.Applicative
import Control.Monad.State

data Alignment = AlignMin | AlignMax | AlignCenter

class MaybeBoxed a where
   maybeBox :: Applicative f
            => (Box (SpaceOf a) -> f (Box (SpaceOf a)))
            -> a -> f a
   blank :: (Box (SpaceOf a)) -> a
instance MaybeBoxed (Glyph a) where
   maybeBox = glyphBox
   blank box = Glyph box Nothing

isBoxed :: MaybeBoxed a => a -> Bool
isBoxed = isJust . (^? maybeBox)

notBoxed :: MaybeBoxed a => a -> Bool
notBoxed = not . isBoxed

class HasEmpty a where
  emptyItem :: a
  isEmpty :: a -> Bool

instance HasEmpty (Glyph (STree o rep)) where
  emptyItem = EmptyGlyph
  isEmpty EmptyGlyph = True
  isEmpty _ = False

instance HasEmpty (STree o rep) where
  emptyItem = SEmpty
  isEmpty SEmpty = True
  isEmpty _ = False

whenBothNotEmpty :: HasEmpty a => a -> a -> (a, a) -> (a, a)
whenBothNotEmpty a b f =
  if isEmpty a || isEmpty b
  then (a, b)
  else f

alignHorizontal :: (Show (SpaceOf rep)
                , HasSpace rep
                , Fractional (SpaceOf rep)
                , SimpleTransformable rep
                , MaybeBoxed rep
                , HasEmpty rep)
                => Alignment
                -> rep
                -> rep
                -> (rep, rep)
alignHorizontal alignment a b =
  whenBothNotEmpty a b $
  let aWidth = a ^?! maybeBox . widthBox
      bWidth = b ^?! maybeBox . widthBox
      width  = max aWidth bWidth
      offsetA = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> width - aWidth
                  AlignCenter -> (width - aWidth) / 2
      newA = tTranslateXY offsetA 0 $ a
      offsetB = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> width - bWidth
                  AlignCenter -> (width - bWidth) / 2
      newB = tTranslateXY offsetB 0 $ b
  in  (newA, newB)

alignVertical :: (Show (SpaceOf rep)
              , HasSpace rep
              , Fractional (SpaceOf rep)
              , SimpleTransformable rep
              , MaybeBoxed rep
              , HasEmpty rep)
              => Alignment
              -> rep
              -> rep
              -> (rep, rep)
alignVertical alignment a b =
  whenBothNotEmpty a b $
  let aHeight = a ^?! maybeBox . heightBox
      bHeight = b ^?! maybeBox . heightBox
      height  = max aHeight bHeight
      offsetA = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> height - aHeight
                  AlignCenter -> (height - aHeight) / 2
      newA = tTranslateXY 0 offsetA $ a
      offsetB = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> height - bHeight
                  AlignCenter -> (height - bHeight) / 2
      newB = tTranslateXY 0 offsetB $ b
  in  (newA, newB)

nextToHorizontal :: ( Show (SpaceOf rep)
                    , HasSpace rep
                    , SimpleTransformable rep
                    , MaybeBoxed rep
                    , HasEmpty rep)
                 => rep -> rep -> (rep, rep)
nextToHorizontal a b =
  whenBothNotEmpty a b $
  let newB = set (maybeBox . widthBox) (b ^?! maybeBox . widthBox ) .
             set (maybeBox . leftSide) (a ^?! maybeBox . rightSide) .
             tTranslateXY (a ^?! maybeBox . rightSide - b ^?! maybeBox . leftSide) 0 $
             b
  in (a, newB)

nextToVertical :: ( Show (SpaceOf rep)
                  , HasSpace rep
                  , SimpleTransformable rep
                  , MaybeBoxed rep
                  , HasEmpty rep)
               => rep -> rep -> (rep, rep)
nextToVertical a b =
  whenBothNotEmpty a b $
    let newB = set (maybeBox . heightBox) (b ^?! maybeBox . heightBox ) .
               set (maybeBox . topSide  ) (a ^?! maybeBox . bottomSide) .
               tTranslateXY 0 (a ^?! maybeBox . bottomSide - b ^?! maybeBox . topSide) $
               b
    in (a, newB)

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe op (Just a) (Just b) = Just (op a b)
combineMaybe op (Just a) Nothing  = Just a
combineMaybe op Nothing  (Just b) = Just b
combineMaybe op Nothing  Nothing  = Nothing


distributeRack :: ( Show (SpaceOf rep)
                  , HasSpace rep
                  , SimpleTransformable rep
                  , MaybeBoxed rep
                  , HasEmpty rep)
               => X (SpaceOf rep)
               -> [rep]
               -> [rep]
distributeRack gap = intersperse (blank (Box zeroPoint (makePoint gap 0)))

distributeStack :: HasSpace rep
                => Y (SpaceOf rep)
                -> [Glyph rep]
                -> [Glyph rep]
distributeStack gap = intersperse (blank (Box zeroPoint (makePoint 0 gap)))

class Overlappable a where
  combine :: a -> a -> a

combineGlyph :: HasSpace rep
             => (STree o rep -> STree o rep -> STree o rep)
             -> Glyph (STree o rep)
             -> Glyph (STree o rep)
             -> Glyph (STree o rep)
combineGlyph op a b =
  if isEmpty a
  then b
  else
  if isEmpty b
  then a
  else
  let tl = minPoint (a ^?! glyphBox . topLeftBox    ) (b ^?! glyphBox . topLeftBox    )
      br = maxPoint (a ^?! glyphBox . bottomRightBox) (b ^?! glyphBox . bottomRightBox)
  in  Glyph (Box tl br) (combineMaybe op (join $ a ^? unGlyph) (join $ b ^? unGlyph))

instance (HasSpace rep, HasDefault o) => Overlappable (Glyph (STree o rep)) where
  combine = combineGlyph (SMeld defaultValue)

instance (HasDefault o) => Overlappable (STree o rep) where
  combine = SMeld defaultValue

instance {-# Overlappable #-} (Applicative f, Overlappable a) => Overlappable (f a) where
  combine = liftA2 (combine :: a -> a -> a)

overlap :: (Overlappable a, HasEmpty a) => [a] -> a
overlap = foldl combine emptyItem

maxPoint :: Ord s => Point2 s -> Point2 s -> Point2 s
minPoint :: Ord s => Point2 s -> Point2 s -> Point2 s
maxPoint (Point2 x0 y0) (Point2 x1 y1) = Point2 (max x0 x1) (max y0 y1)
minPoint (Point2 x0 y0) (Point2 x1 y1) = Point2 (min x0 x1) (min y0 y1)

rack :: forall rep
     .  ( Show rep, Show (SpaceOf rep)
        , HasSpace rep, Fractional (SpaceOf rep)
        , HasSpace rep
        , Overlappable rep
        , SimpleTransformable rep
        , MaybeBoxed rep
        , HasEmpty rep)
     => Alignment
     -> [rep]
     -> rep
rack alignment = foldl (\ a b -> uncurry combine . uncurry nextToHorizontal . uncurry (alignVertical alignment) $ (a, b)) emptyItem

stack :: forall rep
      .  ( Show rep, Show (SpaceOf rep)
         , HasSpace rep, Fractional (SpaceOf rep)
         , HasSpace rep
         , Overlappable rep
         , SimpleTransformable rep
         , MaybeBoxed rep
         , HasEmpty rep)
      => Alignment
      -> [rep]
      -> rep
stack alignment = foldl (\ a b -> uncurry combine . uncurry nextToVertical . uncurry (alignHorizontal alignment) $ (a, b)) emptyItem

paragraph :: forall m . (MonadState FontCache m, Monad m)
          => X SubSpace
          -> Y SubSpace
          -> Alignment
          -> Alignment
          -> String
          -> m (Glyph (CompoundTree SubSpace))
paragraph gapX gapY alignX alignY string =
  do  let stringLines = lines string
      glyphLines <- mapM glyphString stringLines
      let glyphRacks = map (rack alignY . distributeRack gapX) glyphLines
      return . stack alignX . distributeStack gapY $ glyphRacks

blurb :: forall m . (MonadState FontCache m, Monad m)
      => X SubSpace
      -> Alignment
      -> String
      -> m (Glyph (CompoundTree SubSpace))
blurb gapX alignX string =
  do  glyphs <- glyphString string
      return $ rack alignX . distributeRack gapX $ glyphs
