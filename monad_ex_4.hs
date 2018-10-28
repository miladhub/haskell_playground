module MyListMonad where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = (Cons $ f a) $ fmap f l

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  (Cons f t) <*> l = (fmap f l) <> (t <*> l)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil l = l
  mappend (Cons a t) l = Cons a (mappend t l)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a t) >>= f = (f a) <> (t >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [
      (1, return Nil),
      (1, Cons <$> arbitrary <*> arbitrary)
    ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: List (String, String, Int)
  in do
    quickBatch $ functor xs
    quickBatch $ monoid xs
    quickBatch $ applicative xs
    quickBatch $ monad xs

