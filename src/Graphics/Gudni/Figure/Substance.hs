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
  , breakdownSubstance
  , mapMSubstanceTexture
  )
where

import Graphics.Gudni.Figure.Color
import Graphics.Gudni.Figure.Space
import Graphics.Gudni.Figure.Picture
import Graphics.Gudni.Figure.Facet
import Graphics.Gudni.Figure.OpenCurve
import Graphics.Gudni.Figure.Gradient
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Projection
import Graphics.Gudni.Figure.Picture
import Graphics.Gudni.Util.Debug
import Control.DeepSeq

-- | Type of filling for overlapping shapes.
data Substance textureLabel s
    = Solid Color
    | Texture textureLabel
    | Linear (LinearGradient s)
    | Radial (RadialGradient s)
    | TransformSubstance (Transformer s) (Substance textureLabel s)

data NamedTexture
  = NewTexture PictureName Picture
  | SharedTexture PictureName
  deriving (Show)


breakdownSubstance :: forall tex s . (Show tex, Space s) => Substance tex s -> (Transformer s, Substance tex s)
breakdownSubstance substance = go IdentityTransform substance
  where
  go :: Transformer s  -> Substance tex s -> (Transformer s, Substance tex s)
  go trans substance =
     case substance of
        TransformSubstance newTrans sub -> go (CombineTransform newTrans trans) sub
        Radial gradient -> (IdentityTransform, tr "Radial" $ Radial $ applyTransformer (tr "trans" trans) gradient)
        Linear gradient -> (IdentityTransform, tr "Linear" $ Linear $ applyTransformer (tr "trans" trans) gradient)
        x -> (trans, x)

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
  translateBy p = TransformSubstance (Translate p)
  scaleBy     s = TransformSubstance (Scale s)
  stretchBy   p = TransformSubstance (Stretch p)

instance Space s => Transformable (Substance n s) where
  rotateBy    a = TransformSubstance (Rotate a)

instance Space s => CanProject (BezierSpace s) (Substance n s) where
  projectionWithStepsAccuracy debug _ _  c = TransformSubstance (Project debug c)


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
