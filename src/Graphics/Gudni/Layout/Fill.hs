{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Fill
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Typeclass for assigning textures to masks.

module Graphics.Gudni.Layout.Fill
  ( withColor
  , withTexture
  , withRadialGradient
  , withLinearGradient
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Token
import Graphics.Gudni.Layout.Style
import Graphics.Gudni.Layout.Type
import Graphics.Gudni.Layout.Combinators
import Graphics.Gudni.Raster.TextureReference


import Control.Lens

withColor :: (IsStyle style)
          => Color (SpaceOf style)
          -> Layout Mono style
          -> Layout Rgba style
withColor color layout = maskWith layout (constantLayer color)

withTexture :: (IsStyle style)
            => NamedTexture
            -> Layout Mono style
            -> Layout Rgba style
withTexture texture layout = maskWith layout (textureLayer texture)

withRadialGradient :: (IsStyle style)
                   => Point2 (SpaceOf style)
                   -> SpaceOf style
                   -> Color (SpaceOf style)
                   -> SpaceOf style
                   -> Color (SpaceOf style)
                   -> Layout Mono style
                   -> Layout Rgba style
withRadialGradient center innerRadius innerColor outerRadius outerColor layout =
    maskWith layout (translateBy center .
                     scaleBy outerRadius .
                     undefined -- innerRadius innerColor outerColor
                     sqrtOf $
                     quadranceLayer
                     )

withLinearGradient :: (IsStyle style)
                   => Point2 (SpaceOf style)
                   -> Color (SpaceOf style)
                   -> Point2 (SpaceOf style)
                   -> Color (SpaceOf style)
                   -> Layout Mono style
                   -> Layout Rgba style
withLinearGradient start startColor end endColor layout =
    maskWith layout (withColor startColor . translateBy start $ {- start end startColor endColor-} linearLayer)
