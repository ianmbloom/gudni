{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Figure.Principle.Axis
  ( Vertical(..)
  , Horizontal(..)
  , IsAxis(..)
  , EitherAxis(..)
  , ToEitherAxis(..)
  , fromEitherAxis
  , withEitherAxis
  , Ax(..)
  , toAlong
  , fromAlong
  , perpendicular
  , toAthwart
  , fromAthwart
  , Along(..)
  , Athwart(..)
  )
where

import Linear

import Data.Kind

import Control.Lens
import Control.Applicative
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint hiding ((<>))

data Vertical   = Vertical deriving (Show, Generic)
instance Out Vertical

data Horizontal = Horizontal deriving (Show, Generic)
instance Out Horizontal

class (Show a, Show (PerpendicularTo a), IsAxis (PerpendicularTo a)) => IsAxis a where
  type PerpendicularTo a :: Type
  perpendicularTo :: a -> PerpendicularTo a
  isVertical :: a -> Bool
  isHorizontal :: a -> Bool
  showSymbol :: a -> String

instance IsAxis Vertical where
  type PerpendicularTo Vertical = Horizontal
  perpendicularTo Vertical = Horizontal
  isVertical   _ = True
  isHorizontal _ = False
  showSymbol _ = "V"

instance IsAxis Horizontal where
  type PerpendicularTo Horizontal = Vertical
  perpendicularTo Horizontal = Vertical
  isVertical   _ = False
  isHorizontal _ = True
  showSymbol _ = "H"

type EitherAxis = Either Horizontal Vertical

class ToEitherAxis axis where
  eitherAxis :: axis -> EitherAxis

instance ToEitherAxis Horizontal where
  eitherAxis axis = Left axis

instance ToEitherAxis Vertical where
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

newtype Ax axis s = Ax {unAxis :: s} deriving (Generic, Eq, Ord, Num, Bounded, Enum, Fractional, Floating, Real, Integral)

-- deriving instance Num s => Num (Along axis s)
-- deriving instance Bounded s => Bounded (Along axis s)
-- deriving instance Enum s => Enum (Along axis s)


instance (Show s, IsAxis axis) => Show (Ax axis s) where
    show (Ax s) = showSymbol (undefined :: axis) ++ show s

instance (Out s, IsAxis axis, Show s) => Out (Ax axis s) where
    doc (Ax x) = text (showSymbol (undefined :: axis)) <> (text . show $ x)
    docPrec _ = doc

type Along   axis s = Ax axis s
type Athwart axis s = Ax (PerpendicularTo axis) s

toAx :: IsAxis axis => axis -> s -> Along axis s
toAx axis s = Ax s

fromAx :: IsAxis axis => axis -> Along axis s -> s
fromAx axis (Ax s) = s

toAlong :: IsAxis axis => axis -> s -> Along axis s
toAlong = toAx

fromAlong :: IsAxis axis => axis -> Along axis s -> s
fromAlong = fromAx

perpendicular :: IsAxis axis => Along axis s -> Athwart axis s
perpendicular (Ax s) = Ax s

toAthwart :: IsAxis axis => axis -> s -> Athwart axis s
toAthwart axis = toAlong (perpendicularTo axis)

fromAthwart :: IsAxis axis => axis -> Athwart axis s -> s
fromAthwart axis = fromAlong (perpendicularTo axis)
