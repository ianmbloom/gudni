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
-- Module      :  Graphics.Gudni.Layout.WithBox
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Data Type for tracking bounding boxes.

module Graphics.Gudni.Layout.WithBox
  ( WithBox(..)
  , withItem
  , withBox
  )
where

import Graphics.Gudni.Figure
import Graphics.Gudni.Layout.Empty

import Control.Lens
import Control.Applicative
import Graphics.Gudni.Util.Util

data WithBox a
   = WithBox { _withItem :: a
             , _withBox :: Box (SpaceOf a)
             }
makeLenses ''WithBox

deriving instance (Show a, Show (SpaceOf a)) => Show (WithBox a)


instance HasSpace a => HasSpace (WithBox a) where
  type SpaceOf (WithBox a) = SpaceOf a
