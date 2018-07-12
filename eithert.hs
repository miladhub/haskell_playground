module Either2 where

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) =
    let meb = (fmap . fmap) f mea
    in EitherT meb

instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure (Right a)
  (EitherT emf) <*> (EitherT ema) = EitherT $ fmap (<*>) emf <*> ema

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ema) >>= f = EitherT $ do
    ea <- ema
    case ea of
      Left e -> return (Left e)
      Right a -> runEitherT (f a)

{-
let e = EitherT $ [Right 1, Left "foo"]
let f = EitherT $ [Right (+1)]
runEitherT $ f <*> e
-}


