{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE StandaloneDeriving    #-}

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
import Graphics.Gudni.Figure.Transformer
import Graphics.Gudni.Figure.Transformable
import Graphics.Gudni.Figure.Projection
import Control.DeepSeq

-- | Type of filling for overlapping shapes.
data Substance textureLabel s
    = Solid Color
    | Texture textureLabel
    | TransformSubstance (Transformer s) (Substance textureLabel s)

data NamedTexture
  = NewTexture PictureName Picture
  | SharedTexture PictureName
  deriving (Show)


breakdownSubstance :: Substance tex s -> (Transformer s, Substance tex s)
breakdownSubstance substance = go IdentityTransform substance
  where
  go trans substance =
     case substance of
        TransformSubstance newTrans sub -> go (CombineTransform newTrans trans) sub
        x -> (trans, x)

mapMSubstanceTexture :: Monad m =>  (a -> m b) -> Substance a s -> m (Substance b s)
mapMSubstanceTexture f substance =
  case substance of
    TransformSubstance newTrans sub -> TransformSubstance newTrans <$> mapMSubstanceTexture f sub
    Solid color -> return $ Solid color
    Texture a -> Texture <$> f a

instance Space s => HasSpace (Substance n s) where
  type SpaceOf (Substance n s) = s

instance Space s => SimpleTransformable (Substance n s) where
  translateBy p = TransformSubstance (Translate p)
  scaleBy     s = TransformSubstance (Scale s)
  stretchBy   p = TransformSubstance (Stretch p)

instance Space s => Transformable (Substance n s) where
  rotateBy    a = TransformSubstance (Rotate a)

instance Space s => CanProject (BezierSpace s) (Substance n s) where
  projection  c = TransformSubstance (Project c)


instance (Space s, Show n, Show s) => Show (Substance n s) where
  show (Solid color) = "Solid " ++ show color
  show (Texture tex) = "Texture " ++ show tex
  show (TransformSubstance trans sub) = "TransfromSubstamce" ++ show trans ++ " " ++ show sub

instance (NFData n, NFData s) => NFData (Substance n s) where
  rnf (Solid color) = color `deepseq` ()
  rnf (Texture texture) = texture `deepseq` ()
  rnf (TransformSubstance a b) = a `deepseq` b `deepseq` ()

instance NFData NamedTexture where
    rnf (NewTexture name _  ) = name `deepseq` ()
    rnf (SharedTexture name ) = name `deepseq` ()
