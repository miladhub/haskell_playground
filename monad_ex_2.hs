module MonadEx2 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhhhbbtttEither b a =
  Leftie a
  | Rightie b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where 
  fmap f (Leftie a) = Leftie $ f a
  fmap _ (Rightie b) = (Rightie b)

instance Applicative (PhhhbbtttEither b) where 
  pure = Leftie
  (Leftie fa) <*> (Leftie a) = Leftie $ fa a
  (Leftie _) <*> (Rightie b) = Rightie b
  (Rightie b) <*> (Rightie _) = Rightie b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Leftie a) >>= f = f a
  (Rightie b) >>= _ = Rightie b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary =
    frequency [
      (1, Leftie <$> arbitrary),
      (1, Rightie <$> arbitrary)
    ]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: PhhhbbtttEither String (String, String, Int)
  in do
    quickBatch $ functor xs
    quickBatch $ applicative xs
    quickBatch $ monad xs
