{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{- | This module contains the 'MonadST' type class, which encapsulates a monad capable of lifting an ST computation.  This type class is /only/ intended to be implemented by the 'ST' monad and any stack of monad transformers over an 'ST' monad.

Presence of a MonadST instance implies that

 * The monad is single-threaded: performing an 'ST' computation will not cause loss of referential transparency, and only one copy of its state thread will be available at any time.

 * Monad transformers can demand an underlying 'MonadST' instance and use its state thread for their own safe computation in the 'ST' monad.

/Note/: Most monad type classes cannot pass instances up through any instance of 'MonadTrans', because a transformer farther out may wish to override the inner instance.  However, in 'MonadST' we very specifically only want one state thread for any stack of transformers, and specifically one at the very bottom level.  This justifies the very general 'MonadST' propagation instance, @(MonadST m, MonadTrans t, Monad m, Monad (t m)) => MonadST (t m)@ (not shown in Haddock for unknown reasons).

/Do not/ implement 'MonadST' propagation if you also provide a 'MonadTrans' instance.-}
module Graphics.Gudni.Util.MonadST where --Control.Monad.ST.Class where

import Control.Monad.ST
import Control.Monad.Trans

-- | Type class of monads that can perform lifted computation in the 'ST' monad.
class MonadST m where
  -- | The type of the state thread used in this monad, never actually instantiated.
  type StateThread m
  liftST :: ST (StateThread m) a -> m a

instance MonadST (ST s) where
  type StateThread (ST s) = s
  liftST m = m

instance MonadST IO where
  type StateThread IO = RealWorld
  liftST = stToIO

instance (MonadST m, MonadTrans t, Monad m) => MonadST (t m) where
  type StateThread (t m) = StateThread m
  liftST = lift . liftST

-- instance (MonadST m, Monad m) => MonadST (LazyS.StateT s m) where
-- 	type StateThread (LazyS.StateT s m) = StateThread m
-- 	liftST = lift . liftST
--
-- instance (MonadST m, Monad m) => MonadST (StrictS.StateT s m) where
-- 	type StateThread (StrictS.StateT s m) = StateThread m
-- 	liftST = lift . liftST
--
-- instance (MonadST m, Monad m) => MonadST (ReaderT r m) where
-- 	type StateThread (ReaderT r m) = StateThread m
-- 	liftST = lift . liftST
--
-- instance (MonadST m, Monad m, Monoid w) => MonadST (LazyW.WriterT w m) where
-- 	type StateThread (LazyW.WriterT w m) = StateThread m
-- 	liftST = lift . liftST
--
-- instance (MonadST m, Monad m, Monoid w) => MonadST (StrictW.WriterT w m) where
-- 	type StateThread (StrictW.WriterT w m) = StateThread m
-- 	liftST = lift . liftST
--
-- instance (MonadST m, Monad m) => MonadST (MaybeT m) where
-- 	type StateThread (MaybeT m) = StateThread m
-- 	liftST = lift . liftST
