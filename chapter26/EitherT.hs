import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT em) =
    EitherT $ (fmap . fmap) f em

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))
  (EitherT fab) <*> (EitherT mma) =
    EitherT $ (<*>) <$> fab <*> mma

instance Monad m 
      => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = 
    EitherT $ do
      v <- ma
      case v of 
        Left e -> return (Left e)
        Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT ema) =
  EitherT $ swapEither <$> ema

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT amb) = do
  v <- amb
  case v of 
    Left a -> f a
    Right b -> g b

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO