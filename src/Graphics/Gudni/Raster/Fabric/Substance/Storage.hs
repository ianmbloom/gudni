{-# LANGUAGE UndecidableInstances  #-} -- Show (SpaceOf leaf)
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Raster.Fabric.Substance.Storage
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for storing and loading substance tags and their corresponding
-- Description data.

module Graphics.Gudni.Raster.Fabric.Substance.Storage
  ( storeSubstance
  , loadSubstance
  )
where

import Graphics.Gudni.Figure.Principle

import Graphics.Gudni.Raster.Serial.Reference
import Graphics.Gudni.Raster.Serial.Pile
import Graphics.Gudni.Raster.Serial.BytePile
import Graphics.Gudni.Raster.Fabric.Substance.Type
import Graphics.Gudni.Raster.Fabric.Tag
import Graphics.Gudni.Raster.TagTypes

import Control.Lens
import Control.Monad
import Control.Monad.State
import Foreign.Storable
import Control.Monad.IO.Class

storeSubstance :: ( MonadIO m
                  --, Storable (SpaceOf (FSubstance i))
                  , Storable (FTex i)
                  , Storable (FQuery i)
                  )
               => FSubstance i
               -> StateT BytePile m FabricTag
storeSubstance substance =
  case substance of
    FConst query -> do queryId <- addToPileS id (AsBytes query)
                       return $ makeFabTagConstant queryId
    FTexture tex -> do pictureRef <- addToPileS id (AsBytes tex)
                       return $ makeFabTagTexture pictureRef
    FLinear      -> return $ makeFabTagLinear
    FQuadrance   -> return $ makeFabTagQuadrance

loadSubstance :: ( MonadIO m
                 , Storable (FTex i)
                 , Storable (FQuery i)
                 )
              => FabricTag
              -> StateT BytePile m (FSubstance i)
loadSubstance tag
   | fabTagIsConstant  tag = do (AsBytes query) <- fromPileS id (Ref . fromIntegral . fabTagSubstanceRef $ tag)
                                return $ FConst query
   | fabTagIsTexture   tag = do (AsBytes tex) <- fromPileS id (Ref . fromIntegral . fabTagSubstanceRef  $ tag)
                                return $ FTexture tex
   | fabTagIsLinear    tag = return FLinear
   | fabTagIsQuadrance tag = return FQuadrance