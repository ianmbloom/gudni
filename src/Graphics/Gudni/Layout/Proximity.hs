{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE StandaloneDeriving    #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Layout.Proximity
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Data structures for describing proximity relationships

module Graphics.Gudni.Layout.Proximity
  ( Proximity(..)
  , ProximityMeld(..)
  , proxStyle
  , proxType
  , proxMeld
  , ProximityCompoundTree(..)
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.WithBox
import Graphics.Gudni.Layout.Alignment

import Control.Lens

data Proximity
    = NextTo
    { _adjNextTo :: EitherAxis
    , _adjAlign  :: Maybe Alignment
    }
    | OnTopOf
    { _adjOverlapAlignVertical   :: Maybe Alignment
    , _adjOverlapAlignHorizontal :: Maybe Alignment
    }
    deriving (Show)
makeLenses ''Proximity

instance HasDefault Proximity where
  defaultValue = OnTopOf Nothing Nothing

noProximity :: Proximity
noProximity = defaultValue

data ProximityMeld style meld
    = ProximityMeld
    { _proxType  :: Proximity
    , _proxStyle :: style
    , _proxMeld  :: meld
    } deriving (Show)
makeLenses ''ProximityMeld

instance (HasDefault style, HasDefault meld) => HasDefault (ProximityMeld style meld) where
    defaultValue = ProximityMeld defaultValue defaultValue defaultValue

type ProximityCompoundTree style = TransTree (ProximityMeld style Compound) (Maybe (WithBox (Shape (SpaceOf style))))
