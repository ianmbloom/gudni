{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ConstraintKinds      #-}

module Graphics.Gudni.Raster.Dag.Fabric.Substance.Query
  ( querySubstanceColor
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.Fabric.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.TextureReference

import Control.Lens
import Control.Monad.State
import Linear.Metric

getPixelS :: (DagConstraints s m)
          => PictureMemoryReference
          -> Point2 PixelSpace
          -> RayMonad s m (Color s)
getPixelS memRef p = do pixelPile <- use dagPixelPile
                        lift $ getPixelColor pixelPile memRef p

querySubstanceColor :: ( DagConstraints (SpaceOf i) m
                       , FTex i ~ PictureMemoryReference
                       , FQuery i ~ Color (SpaceOf i)
                       )
                    => FSubstance i
                    -> Point2 (SpaceOf i)
                    -> RayMonad (SpaceOf i) m (Color (SpaceOf i))
querySubstanceColor substance ray =
    case substance of
        FConst     q      -> return q
        FTexture   memRef -> let pixel = fmap floor ray
                             in  getPixelS memRef pixel
        FLinear           -> return . Color . pure . realToFrac . fromAlong Vertical $ ray ^. pY
        FQuadrance        -> return . Color . pure . realToFrac . quadrance          $ ray
