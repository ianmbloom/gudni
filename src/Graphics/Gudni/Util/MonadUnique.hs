{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Gudni.Util.MonadUnique
  ( UniqueT,
    Unique,
    MonadUnique,
    fresh,
    evalUniqueT,
    evalUnique )
where

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

newtype UniqueT m a = UniqueT (StateT Integer m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

newtype Unique a = Unique (UniqueT Identity a)
    deriving (Functor, Applicative, Monad, MonadUnique)

class Monad m => MonadUnique m where
    fresh :: m Integer

instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
                n <- get
                put (succ n)
                return n

evalUniqueT (UniqueT s) = evalStateT s 0
evalUnique (Unique s) = runIdentity (evalUniqueT s)
