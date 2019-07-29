{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Scaffolding
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Top level functions for creating bounding box based layouts.

module Graphics.Gudni.Layout.Scaffolding
  ( Alignment (..)
  , glyphString
  , paragraph
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

import Control.Lens
import Control.Applicative
import Control.Monad.State

data Alignment = AlignMin | AlignMax | AlignCenter

alignHorizontal :: (Show (SpaceOf rep), HasSpace rep, Fractional (SpaceOf rep))
                => Alignment
                -> Glyph (STree o rep)
                -> Glyph (STree o rep)
                -> (Glyph (STree o rep), Glyph (STree o rep))
alignHorizontal alignment a b =
  let aWidth = a ^. glyphBox . widthBox
      bWidth = b ^. glyphBox . widthBox
      width  = max aWidth bWidth
      offsetA = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> width - aWidth
                  AlignCenter -> (width - aWidth) / 2
      newA = {-set (glyphBox . widthBox) width .-} tTranslateXY offsetA 0 $ a
      offsetB = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> width - bWidth
                  AlignCenter -> (width - bWidth) / 2
      newB = {-set (glyphBox . widthBox) width .-} tTranslateXY offsetB 0 $ b
  in  (newA, newB)

alignVertical :: (Show (SpaceOf rep), HasSpace rep, Fractional (SpaceOf rep))
              => Alignment
              -> Glyph (STree o rep)
              -> Glyph (STree o rep)
              -> (Glyph (STree o rep), Glyph (STree o rep))
alignVertical alignment a b =
  let aHeight = a ^. glyphBox . heightBox
      bHeight = b ^. glyphBox . heightBox
      height  = max aHeight bHeight
      offsetA = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> height - aHeight
                  AlignCenter -> (height - aHeight) / 2
      newA = set (glyphBox . heightBox) height . mapGlyph (tTranslateXY 0 offsetA) $ a
      offsetB = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> height - bHeight
                  AlignCenter -> (height - bHeight) / 2
      newB = set (glyphBox . heightBox) height . mapGlyph (tTranslateXY 0 offsetB) $ b
  in  (newA, newB)

nextToHorizontal :: (Show (SpaceOf rep), HasSpace rep) => Glyph (STree o rep) -> Glyph (STree o rep) -> (Glyph (STree o rep), Glyph (STree o rep))
nextToHorizontal a b =
    let newB = set (glyphBox . widthBox) (b ^. glyphBox . widthBox ) .
               set (glyphBox . leftSide) (a ^. glyphBox . rightSide) .
               mapGlyph (tTranslateXY (a ^. glyphBox . rightSide - b ^. glyphBox . leftSide) 0) $
               b
    in (a, newB)

nextToVertical :: (Show (SpaceOf rep), HasSpace rep) => Glyph (STree o rep) -> Glyph (STree o rep) -> (Glyph (STree o rep), Glyph (STree o rep))
nextToVertical a b =
    let newB = set (glyphBox . heightBox) (b ^. glyphBox . heightBox ) .
               set (glyphBox . topSide  ) (a ^. glyphBox . bottomSide) .
               mapGlyph (tTranslateXY 0 (a ^. glyphBox . bottomSide - b ^. glyphBox . topSide)) $
               b
    in (a, newB)

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe op (Just a) (Just b) = Just (op a b)
combineMaybe op (Just a) Nothing  = Just a
combineMaybe op Nothing  (Just b) = Just b
combineMaybe op Nothing  Nothing  = Nothing

betweenList :: a -> [a] -> [a]
betweenList b [a]    = [a]
betweenList b (a:as) = a : b : betweenList b as
betweenList b []     = []

distributeRack :: HasSpace rep
               => X (SpaceOf rep)
               -> [Glyph rep]
               -> [Glyph rep]
distributeRack gap = betweenList (Glyph (Box zeroPoint (makePoint gap 0)) Nothing)

distributeStack :: HasSpace rep
                => Y (SpaceOf rep)
                -> [Glyph rep]
                -> [Glyph rep]
distributeStack gap = betweenList (Glyph (Box zeroPoint (makePoint 0 gap)) Nothing)

class Overlappable a where
  combine :: a -> a -> a

combineGlyph :: HasSpace rep
             => (STree o rep -> STree o rep -> STree o rep)
             -> Glyph (STree o rep)
             -> Glyph (STree o rep)
             -> Glyph (STree o rep)
combineGlyph op a b =
  let tl = minPoint (a ^. glyphBox . topLeftBox    ) (b ^. glyphBox . topLeftBox    )
      br = maxPoint (a ^. glyphBox . bottomRightBox) (b ^. glyphBox . bottomRightBox)
  in  Glyph (Box tl br) (combineMaybe op (a ^. unGlyph) (b ^. unGlyph))

instance (HasSpace rep, HasDefault o) => Overlappable (Glyph (STree o rep)) where
  combine = combineGlyph (SMeld defaultValue)

instance (HasDefault o) => Overlappable (STree o rep) where
  combine = SMeld defaultValue

instance {-# Overlappable #-} (Applicative f, Overlappable a) => Overlappable (f a) where
  combine = liftA2 (combine :: a -> a -> a)

overlap :: (Overlappable a) => [a] -> a
overlap = foldl1 combine

maxPoint :: Ord s => Point2 s -> Point2 s -> Point2 s
minPoint :: Ord s => Point2 s -> Point2 s -> Point2 s
maxPoint (Point2 x0 y0) (Point2 x1 y1) = Point2 (max x0 x1) (max y0 y1)
minPoint (Point2 x0 y0) (Point2 x1 y1) = Point2 (min x0 x1) (min y0 y1)

rack :: forall o rep
     .  (Show rep, Show (SpaceOf rep), Show o,
         HasSpace rep, Fractional (SpaceOf rep), HasDefault o)
     => Alignment
     -> [Glyph (STree o rep)]
     -> Glyph (STree o rep)
rack alignment = foldl1 (\ a b -> uncurry (combineGlyph (SMeld defaultValue)) . uncurry nextToHorizontal . uncurry (alignVertical alignment) $ (a, b))

stack :: forall o rep
      .  (Show rep, Show (SpaceOf rep), Show o,
          HasSpace rep, Fractional (SpaceOf rep), HasDefault o)
      => Alignment
      -> [Glyph (STree o rep)]
      -> Glyph (STree o rep)
stack alignment = foldl1 (\ a b -> uncurry (combineGlyph (SMeld defaultValue)) . uncurry nextToVertical . uncurry (alignHorizontal alignment) $ (a, b))

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
