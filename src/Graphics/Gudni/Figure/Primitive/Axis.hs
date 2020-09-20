{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveGeneric       #-}

module Graphics.Gudni.Figure.Primitive.Axis
  ( Vertical(..)
  , Horizontal(..)
  , Axis(..)
  , athwart
  , EitherAxis(..)
  , SwitchAxis(..)
  , fromEitherAxis
  , withEitherAxis
  )
where

import Linear

import Graphics.Gudni.Figure.Primitive.Space
import Graphics.Gudni.Figure.Primitive.Point

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
  along :: a -> Lens' (Point2 s) s
  nextAxis :: a -> NextAxis a
  isVertical :: a -> Bool
  isHorizontal :: a -> Bool
  showSymbol :: a -> String

athwart :: Axis a => a -> Lens' (Point2 s) s
athwart = along . nextAxis

instance Axis Vertical where
  type NextAxis Vertical = Horizontal
  along Vertical = pY
  nextAxis Vertical = Horizontal
  isVertical   _ = True
  isHorizontal _ = False
  showSymbol _ = "V"

instance Axis Horizontal where
  type NextAxis Horizontal = Vertical
  along Horizontal = pX
  nextAxis Horizontal = Vertical
  isVertical   _ = False
  isHorizontal _ = True
  showSymbol _ = "H"

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

withEitherAxis :: (Horizontal -> c)
               -> (Vertical -> c)
               -> EitherAxis
               -> c
withEitherAxis hori vert axis =
  case axis of
    Left Horizontal -> hori Horizontal
    Right Vertical  -> vert Vertical
