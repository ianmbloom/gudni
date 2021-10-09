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
{-# LANGUAGE DeriveGeneric         #-}
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
  , noProximity
  , proximityWillTransform
  , ProximityMeld(..)
  , proxStyle
  , proxType
  , proxMeld
  )
where

import Graphics.Gudni.Base
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
    deriving (Show, Generic)
makeLenses ''Proximity

instance HasDefault Proximity where
  defaultValue = OnTopOf Nothing Nothing

noProximity :: Proximity
noProximity = defaultValue

proximityWillTransform (OnTopOf Nothing Nothing) = False
proximityWillTransform _ = True

data ProximityMeld style meld
    = ProximityMeld
    { _proxType  :: Proximity
    , _proxStyle :: style
    , _proxMeld  :: meld
    } deriving (Show, Generic)
makeLenses ''ProximityMeld

instance (HasDefault style, HasDefault meld) => HasDefault (ProximityMeld style meld) where
    defaultValue = ProximityMeld defaultValue defaultValue defaultValue

instance Out Proximity where
  doc prox =
    case prox of
      OnTopOf v h -> text "OnTop " <+> doc v <+> doc h
      NextTo axis align -> text "NextTo" <+> doc axis <+> doc align
  docPrec _ = doc

instance (Out style, Out meld) => Out (ProximityMeld style meld) where
  doc meld = text "Prox" <+> parens (doc (meld ^. proxType)) <+> doc (meld ^. proxStyle) <+> doc (meld ^. proxMeld)
  docPrec _ = doc
