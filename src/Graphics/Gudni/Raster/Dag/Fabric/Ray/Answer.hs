{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}

module Graphics.Gudni.Raster.Dag.Fabric.Ray.Answer
  ( Answer(..)
  , ColorStack(..)
  )
where

import Graphics.Gudni.Base
import Graphics.Gudni.Figure
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Type
import Graphics.Gudni.Raster.Dag.Fabric.Substance.Query
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Type
import Graphics.Gudni.Raster.Dag.Fabric.Combine.Query
import Graphics.Gudni.Raster.Dag.Storage
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Class
import Graphics.Gudni.Raster.Dag.Fabric.Ray.Filter

import Graphics.Gudni.Util.Util
import Graphics.Gudni.Raster.TextureReference
import Control.Applicative

import Control.Lens
import Control.Monad.IO.Class

class Answer q where
    emptyQuery      :: q
    insideShape     :: q
    traverseStop    :: FCombineType -> q -> Bool
    traverseCombine :: FCombineType -> q -> q -> q
    fromSubstance   :: ( DagConstraints (SpaceOf i) m
                       , FTex i ~ PictureMemoryReference
                       , FQuery i ~ Color (SpaceOf i)
                       , SpaceOf q ~ SpaceOf i
                       )
                    => FSubstance i
                    -> Point2 (SpaceOf q)
                    -> RayMonad (SpaceOf q) m q
    applyFilter     :: FFilter -> q -> q


instance Space s => Answer (Color s) where
    emptyQuery = clearBlack
    insideShape = opaqueWhite
    traverseStop op color =
      case op of
        -- in the case of a composite combination we can short circuit the combination if the first value is opaque. (alpha ~ 1)
        FComposite -> isOpaque color
        -- conversely in the case of a mask we can short circut the combination if the first value is clear. (alpha ~ 0)
        FMask      -> isClear  color
        -- any other type of combination can't short circuit.
        _          -> False
    fromSubstance substance ray = querySubstanceColor substance ray
    traverseCombine combiner a b = combineColor combiner a b
    applyFilter filt =
        case filt of
            FSqrt   -> Color . fmap sqrt        . view unColor
            FInvert -> Color . fmap (1-)        . view unColor
            FCos    -> Color . fmap cos         . view unColor
            FSin    -> Color . fmap sin         . view unColor
            FClamp  -> Color . fmap (clamp 0 1) . view unColor

newtype ColorStack s = ColorStack [Color s] deriving (Show, Generic)

instance Out s => Out (ColorStack s)

instance Space s => HasSpace (ColorStack s) where
  type SpaceOf (ColorStack s) = s

(+++) (ColorStack a) (ColorStack b) = ColorStack (a ++ b)

compositeStack :: Space s => [Color s] -> Color s
compositeStack ts = if null ts then clearBlack else foldl1 composite ts

multiplyColor (Color a) (Color b) = Color $ a * b

instance Space s => Answer (ColorStack s) where
    emptyQuery =  ColorStack [] -- [clearBlack]
    insideShape = ColorStack [] -- [opaqueWhite]
    traverseStop _ _ = False -- never stop, no short circuits
    traverseCombine fCombine (ColorStack a) (ColorStack b) =
        ColorStack $
        case fCombine of
            FMask -> a ++ b --  fmap (multiplyColor (compositeStack a)) b
            FComposite -> a ++ b
            _ -> a ++ b
    fromSubstance substance ray = ColorStack . pure <$> querySubstanceColor substance ray
    applyFilter filt = id
