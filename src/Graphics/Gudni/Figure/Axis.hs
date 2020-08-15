{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric       #-}

module Graphics.Gudni.Figure.Axis
  ( Vertical(..)
  , Horizontal(..)
  , Axis(..)
  , along
  , EitherAxis(..)
  , SwitchAxis(..)
  , fromEitherAxis
  )
where

import Linear

import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Point

import Data.Kind

import Control.Lens
import Control.Applicative
import Text.PrettyPrint.GenericPretty

data Vertical   = Vertical deriving (Show, Generic)
instance Out Vertical
data Horizontal = Horizontal deriving (Show, Generic)
instance Out Horizontal

class (Show a, Show (NextAxis a), Axis (NextAxis a)) => Axis a where
  type NextAxis a :: Type
  with :: a -> Lens' (Point2 s) s
  nextAxis :: a -> NextAxis a
  isVertical :: a -> Bool
  isHorizontal :: a -> Bool

along :: Axis a => a -> Lens' (Point2 s) s
along = with . nextAxis

instance Axis Vertical where
  type NextAxis Vertical = Horizontal
  with Vertical = pX
  nextAxis Vertical = Horizontal
  isVertical   _ = True
  isHorizontal _ = False

instance Axis Horizontal where
  type NextAxis Horizontal = Vertical
  with Horizontal = pY
  nextAxis Horizontal = Vertical
  isVertical   _ = False
  isHorizontal _ = True

type EitherAxis = Either Horizontal Vertical

class SwitchAxis axis where
  eitherAxis :: axis -> EitherAxis

instance SwitchAxis Horizontal where
  eitherAxis axis = Left axis

instance SwitchAxis Vertical where
  eitherAxis axis = Right axis

fromEitherAxis :: c -> c -> EitherAxis -> c
fromEitherAxis hori vert axis =
  case axis of
    Left Horizontal -> hori
    Right Vertical  -> vert
