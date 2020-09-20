{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Gudni.Figure.Token
-- Copyright   :  (c) Ian Bloom 2019
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  Ian Bloom
-- Stability   :  experimental
-- Portability :  portable
--
-- Typeclass for identifying shapes with tokens.

module Graphics.Gudni.Layout.Token
  ( HasToken(..)
  , Tokenized(..)
  , setToken
  )
where

class Show (TokenOf a) => HasToken a where
  type TokenOf a

class HasToken a => Tokenized a where
  overToken :: (Maybe (TokenOf a) -> Maybe (TokenOf a)) -> a -> a

setToken :: Tokenized a => TokenOf a -> a -> a
setToken = overToken . const . Just
