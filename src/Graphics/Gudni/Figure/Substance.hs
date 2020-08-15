{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Graphics.Gudni.Figure.Substance
  ( Substance(..)
  , NamedTexture(..)
  , substanceIsConstant
  , breakdownSubstance
  , mapMSubstanceTexture
  )
where

import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Picture
import Graphics.Gudni.Figure.Box
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Gradient
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.BezierSpace
import Graphics.Gudni.Figure.Picture
import Graphics.Gudni.Util.Debug
import Control.DeepSeq



-- | Type of filling for overlapping shapes.
data Substance tex s
    = Solid Color
    | Texture tex
    | Linear (LinearGradient s)
    | Radial (RadialGradient s)
    | TransformSubstance (Transformer s) (Substance tex s)

data NamedTexture
  = NewTexture PictureName Picture
  | SharedTexture PictureName
  deriving (Show)


breakdownSubstance :: forall tex s . (Show tex, Space s) => Substance tex s -> (Transformer s, Substance tex s)
breakdownSubstance substance = go (Simple IdentityTransform) substance
  where
  go :: Transformer s  -> Substance tex s -> (Transformer s, Substance tex s)
  go trans substance =
     case substance of
        TransformSubstance newTrans sub -> go (CombineTransform newTrans trans) sub
        x -> (trans, x)

substanceIsConstant :: Substance tex s -> Bool
substanceIsConstant (Solid {}) = True
substanceIsConstant _ = False

mapMSubstanceTexture :: Monad m =>  (a -> m b) -> Substance a s -> m (Substance b s)
mapMSubstanceTexture f substance =
  case substance of
    TransformSubstance newTrans sub -> TransformSubstance newTrans <$> mapMSubstanceTexture f sub
    Texture a -> Texture <$> f a
    Solid color -> return $ Solid color
    Linear linearGradient -> return $ Linear linearGradient
    Radial radialGradient -> return $ Radial radialGradient

instance Space s => HasSpace (Substance n s) where
  type SpaceOf (Substance n s) = s

instance Space s => SimpleTransformable (Substance n s) where
  translateBy p = TransformSubstance (Simple $ Translate p)
  stretchBy   p = TransformSubstance (Simple $ Stretch p)

instance Space s => Transformable (Substance n s) where
  rotateBy    a = TransformSubstance (Rotate a)

instance Space s => Projectable (Substance n s) where
  projectOnto path = TransformSubstance (Project path)

instance (Space s, Show n, Show s) => Show (Substance n s) where
  show (Solid color) = "Solid " ++ show color
  show (Texture tex) = "Texture " ++ show tex
  show (Linear linearGradient)  = show linearGradient
  show (Radial radialGradient)  = show radialGradient
  show (TransformSubstance trans sub) = "TransfromSubstamce" ++ show trans ++ " " ++ show sub

instance (NFData n, NFData s) => NFData (Substance n s) where
  rnf (Solid color)            = color `deepseq` ()
  rnf (Texture texture)        = texture `deepseq` ()
  rnf (Linear linearGradient)  = linearGradient `deepseq` ()
  rnf (Radial radialGradient)  = radialGradient `deepseq` ()
  rnf (TransformSubstance a b) = a `deepseq` b `deepseq` ()

instance NFData NamedTexture where
    rnf (NewTexture name _  ) = name `deepseq` ()
    rnf (SharedTexture name ) = name `deepseq` ()
