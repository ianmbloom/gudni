{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}

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
    , gradientInsideColor  :: Color s
    , gradientOutsideColor :: Color s
    }

deriving instance (Show s, Show (Color s)) => Show (RadialGradient s)

instance Space s => HasSpace (RadialGradient s) where
    type SpaceOf (RadialGradient s) = s

data LinearGradient s = LinearGradient
    { gradientStart      :: Point2 s
    , gradientEnd        :: Point2 s
    , gradientStartColor :: Color s
    , gradientEndColor   :: Color s
    }

deriving instance (Show s, Show (Color s)) => Show (LinearGradient s)

instance Space s => HasSpace (LinearGradient s) where
    type SpaceOf (LinearGradient s) = s
