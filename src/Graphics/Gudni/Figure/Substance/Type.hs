{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Graphics.Gudni.Figure.Substance.Type
  ( Substance(..)
  , NamedTexture(..)
  , substanceIsConstant
  , mapMSubstanceTexture
  )
where

import Graphics.Gudni.Figure.Principle
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Figure.Transform

import Graphics.Gudni.Figure.Substance.Color
import Graphics.Gudni.Figure.Substance.Picture
import Graphics.Gudni.Figure.Substance.Gradient

import Graphics.Gudni.Util.Debug

-- | Type of filling for overlapping shapes.
data Substance tex s
    = Solid Color
    | Texture tex
    | Linear (LinearGradient s)
    | Radial (RadialGradient s)

data NamedTexture
  = NewTexture PictureName Picture
  | SharedTexture PictureName
  deriving (Show)

substanceIsConstant :: Substance tex s -> Bool
substanceIsConstant (Solid {}) = True
substanceIsConstant _ = False

mapMSubstanceTexture :: Monad m =>  (a -> m b) -> Substance a s -> m (Substance b s)
mapMSubstanceTexture f substance =
  case substance of
    Texture a -> Texture <$> f a
    Solid color -> return $ Solid color
    Linear linearGradient -> return $ Linear linearGradient
    Radial radialGradient -> return $ Radial radialGradient

instance Space s => HasSpace (Substance n s) where
  type SpaceOf (Substance n s) = s

instance (Space s, Show n, Show s) => Show (Substance n s) where
  show (Solid color) = "Solid " ++ show color
  show (Texture tex) = "Texture " ++ show tex
  show (Linear linearGradient)  = show linearGradient
  show (Radial radialGradient)  = show radialGradient
