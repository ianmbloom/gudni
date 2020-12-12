{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Dag.Fabric.Ray.Answer
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- An Answer is an abstraction of the return value of passing a ray through a scene.
-- The most basic Answer is the color of a pixel.

module Graphics.Gudni.Raster.Dag.Fabric.Ray.Answer
  ( Answer(..)
  , ColorStack(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Apply
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
import Graphics.Gudni.Raster.Dag.Fabric.Filter.Type
import Graphics.Gudni.Raster.Dag.TagTypes

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Raster.TextureReference
import Control.Applicative

import Linear.Metric
import Control.Lens
import Control.Monad.IO.Class
import Foreign.Storable

class Storable (SpaceOf q) => Answer q where
    outsideShape    :: q
    insideShape     :: q
    traverseStop    :: FCombineType -> q -> Bool
    applyCombine :: FCombineType -> q -> q -> q
    applyFilter     :: FFilter -> q -> q
    linearQuery     :: Point2 (SpaceOf q) -> q
    quadranceQuery  :: Point2 (SpaceOf q) -> q
    constantQuery   :: (MonadIO m) => FabricTag -> DagMonad (SpaceOf q) m q
    textureQuery    :: (MonadIO m) => FabricTag -> Point2 (SpaceOf q) -> DagMonad (SpaceOf q) m q

instance (Storable s, Space s) => Answer (Color s) where
    outsideShape = clearBlack
    insideShape  = opaqueWhite
    traverseStop op color =
      case op of
        -- in the case of a composite combination we can short circuit the combination if the first value is opaque. (alpha ~ 1)
        FComposite -> isOpaque color
        -- conversely in the case of a mask we can short circut the combination if the first value is clear. (alpha ~ 0)
        FMask      -> isClear  color
        -- any other type of combination can't short circuit.
        _          -> False
    applyCombine combiner a b = combineColor combiner a b
    applyFilter filt =
        case filt of
            FSqrt   -> Color . fmap sqrt        . view unColor
            FInvert -> Color . fmap (1-)        . view unColor
            FCos    -> Color . fmap cos         . view unColor
            FSin    -> Color . fmap sin         . view unColor
            FClamp  -> Color . fmap (clamp 0 1) . view unColor
    linearQuery    = Color . pure . realToFrac . fromAlong Vertical . view pY
    quadranceQuery = Color . pure . realToFrac . quadrance
    constantQuery  = loadColorS
    textureQuery   = loadPixelS

newtype ColorStack s = ColorStack [Color s] deriving (Show, Generic)

instance Out s => Out (ColorStack s)

instance Space s => HasSpace (ColorStack s) where
  type SpaceOf (ColorStack s) = s

(+++) (ColorStack a) (ColorStack b) = ColorStack (a ++ b)

compositeStack :: Space s => [Color s] -> Color s
compositeStack ts = if null ts then clearBlack else foldl1 composite ts

multiplyColor (Color a) (Color b) = Color $ a * b

instance (Storable s, Space s) => Answer (ColorStack s) where
    outsideShape =  ColorStack []
    insideShape = ColorStack []
    traverseStop _ _ = False -- never stop, no short circuits
    applyCombine fCombine (ColorStack a) (ColorStack b) =
        ColorStack $
            case fCombine of
                FMask -> a ++ b --  fmap (multiplyColor (compositeStack a)) b
                FComposite -> a ++ b
                _ -> a ++ b
    applyFilter filt = id
    linearQuery    = ColorStack . pure . linearQuery
    quadranceQuery = ColorStack . pure . quadranceQuery
    constantQuery  tag   = ColorStack . pure <$> constantQuery tag
    textureQuery   tag point = ColorStack . pure <$> textureQuery tag point
