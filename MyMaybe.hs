{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyMaybe where

import Control.Monad.IO.Class
import Control.Monad

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m)
      => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma
  
instance (Applicative m)
      => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma
    
instance (Monad m)
      => Monad (MaybeT m) where
    return = pure
    (>>=) :: MaybeT m a
          -> (a -> MaybeT m b)
          -> MaybeT m b
    (MaybeT ma) >>= f =
      MaybeT $ do
        v <- ma
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
            
instance (MonadIO m)
      => MonadIO (MaybeT m) where
      liftIO = mylift . liftIO
{-    liftIO :: IO a -> MaybeT m a
    liftIO ioa =
        let ma = liftIO ioa :: m a
            mma = fmap return ma :: m (Maybe a)
            mtma = MaybeT mma :: MaybeT m a
        in mtma
-}

mylift :: (Monad m) => m a -> MaybeT m a
--mylift = MaybeT . fmap return
--mylift ma = MaybeT $ fmap return ma
mylift ma = MaybeT $ liftM Just ma -- liftM = fmap for monads; fmap Just ma ~ m (Maybe a) ~ fmap return ma
