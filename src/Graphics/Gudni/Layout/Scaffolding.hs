{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- Work in progress, top level datastructure for alignment and text layout.
module Graphics.Gudni.Layout.Scaffolding
  ( Alignment (..)
  , glyphString
  , paragraph
  , distributeRack
  , distributeStack
  , rack
  , stack
  , overlap
  , combineBoxed
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Boxed
import Graphics.Gudni.Layout.Glyph
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
                -> Boxed (STree o rep)
                -> Boxed (STree o rep)
                -> (Boxed (STree o rep), Boxed (STree o rep))
alignHorizontal alignment a b =
  let aWidth = a ^. boxAround . widthBox
      bWidth = b ^. boxAround . widthBox
      width  = max aWidth bWidth
      offsetA = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> width - aWidth
                  AlignCenter -> (width - aWidth) / 2
      newA = {-set (boxAround . widthBox) width .-} tTranslateXY offsetA 0 $ a
      offsetB = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> width - bWidth
                  AlignCenter -> (width - bWidth) / 2
      newB = {-set (boxAround . widthBox) width .-} tTranslateXY offsetB 0 $ b
  in  (newA, newB)

alignVertical :: (Show (SpaceOf rep), HasSpace rep, Fractional (SpaceOf rep))
              => Alignment
              -> Boxed (STree o rep)
              -> Boxed (STree o rep)
              -> (Boxed (STree o rep), Boxed (STree o rep))
alignVertical alignment a b =
  let aHeight = a ^. boxAround . heightBox
      bHeight = b ^. boxAround . heightBox
      height  = max aHeight bHeight
      offsetA = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> height - aHeight
                  AlignCenter -> (height - aHeight) / 2
      newA = set (boxAround . heightBox) height . mapBoxed (tTranslateXY 0 offsetA) $ a
      offsetB = case alignment of
                  AlignMin    -> 0
                  AlignMax    -> height - bHeight
                  AlignCenter -> (height - bHeight) / 2
      newB = set (boxAround . heightBox) height . mapBoxed (tTranslateXY 0 offsetB) $ b
  in  (newA, newB)

nextToHorizontal :: (Show (SpaceOf rep), HasSpace rep) => Boxed (STree o rep) -> Boxed (STree o rep) -> (Boxed (STree o rep), Boxed (STree o rep))
nextToHorizontal a b =
    let newB = set (boxAround . widthBox) (b ^. boxAround . widthBox ) .
               set (boxAround . leftSide) (a ^. boxAround . rightSide) .
               mapBoxed (tTranslateXY (a ^. boxAround . rightSide - b ^. boxAround . leftSide) 0) $
               b
    in (a, newB)

nextToVertical :: (Show (SpaceOf rep), HasSpace rep) => Boxed (STree o rep) -> Boxed (STree o rep) -> (Boxed (STree o rep), Boxed (STree o rep))
nextToVertical a b =
    let newB = set (boxAround . heightBox) (b ^. boxAround . heightBox ) .
               set (boxAround . topSide  ) (a ^. boxAround . bottomSide) .
               mapBoxed (tTranslateXY 0 (a ^. boxAround . bottomSide - b ^. boxAround . topSide)) $
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
               -> [Boxed rep]
               -> [Boxed rep]
distributeRack gap = betweenList (Boxed (Box zeroPoint (makePoint gap 0)) Nothing)

distributeStack :: HasSpace rep
                => Y (SpaceOf rep)
                -> [Boxed rep]
                -> [Boxed rep]
distributeStack gap = betweenList (Boxed (Box zeroPoint (makePoint 0 gap)) Nothing)

class Overlappable a where
  combine :: a -> a -> a

combineBoxed :: HasSpace rep
             => (STree o rep -> STree o rep -> STree o rep)
             -> Boxed (STree o rep)
             -> Boxed (STree o rep)
             -> Boxed (STree o rep)
combineBoxed op a b =
  let tl = minPoint (a ^. boxAround . topLeftBox    ) (b ^. boxAround . topLeftBox    )
      br = maxPoint (a ^. boxAround . bottomRightBox) (b ^. boxAround . bottomRightBox)
  in  Boxed (Box tl br) (combineMaybe op (a ^. unBoxed) (b ^. unBoxed))

instance (HasSpace rep, HasDefault o) => Overlappable (Boxed (STree o rep)) where
  combine = combineBoxed (SMeld defaultValue)

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
     -> [Boxed (STree o rep)]
     -> Boxed (STree o rep)
rack alignment = foldl1 (\ a b -> uncurry (combineBoxed (SMeld defaultValue)) . uncurry nextToHorizontal . uncurry (alignVertical alignment) $ (a, b))

stack :: forall o rep
      .  (Show rep, Show (SpaceOf rep), Show o,
          HasSpace rep, Fractional (SpaceOf rep), HasDefault o)
      => Alignment
      -> [Boxed (STree o rep)]
      -> Boxed (STree o rep)
stack alignment = foldl1 (\ a b -> uncurry (combineBoxed (SMeld defaultValue)) . uncurry nextToVertical . uncurry (alignHorizontal alignment) $ (a, b))

paragraph :: forall m . (MonadState GlyphCache m, Monad m)
          => X SubSpace
          -> Y SubSpace
          -> Alignment
          -> Alignment
          -> String
          -> m (Boxed (CompoundTree SubSpace))
paragraph gapX gapY alignX alignY string =
  do  let stringLines = lines string
      glyphLines <- mapM glyphString stringLines
      let glyphRacks = map (rack alignY . distributeRack gapX) glyphLines
      return . stack alignX . distributeStack gapY $ glyphRacks
