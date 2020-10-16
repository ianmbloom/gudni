{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.Gudni.Figure.Substance.Gradient
    ( RadialGradient(..)
    , LinearGradient(..)
    )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Substance.Color

import Graphics.Gudni.Util.StorableM
import Control.Lens

data RadialGradient s = RadialGradient
    { gradientCenter       :: Point2 s
    , gradientInnerRadius  :: s
    , gradientOuterRadius  :: s
    , gradientInsideColor  :: Color
    , gradientOutsideColor :: Color
    } deriving (Show)

instance Space s => HasSpace (RadialGradient s) where
    type SpaceOf (RadialGradient s) = s

data LinearGradient s = LinearGradient
    { gradientStart      :: Point2 s
    , gradientEnd        :: Point2 s
    , gradientStartColor :: Color
    , gradientEndColor   :: Color
    } deriving (Show)

instance Space s => HasSpace (LinearGradient s) where
    type SpaceOf (LinearGradient s) = s
