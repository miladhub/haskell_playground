foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

