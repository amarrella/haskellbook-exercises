{-# LANGUAGE InstanceSigs #-}
module StateT where

import Control.Monad
import Control.Monad.Trans.Class

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) =
    StateT $ \s -> 
      let mas = smas s
      in fmap ((\b -> (b, s)) . f . fst) mas

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (StateT fsmas) <*> (StateT smas) = 
    StateT $ \s -> do
      (a, s')  <- smas s
      (f, s'') <- fsmas s'
      return (f a, s'')

instance (Monad m)
      => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = 
    StateT $ \s -> do
      (a, s') <- sma s
      runStateT (f a) s'

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)