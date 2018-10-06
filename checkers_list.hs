module MyList where

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
  let xs = Cons ("b", "w", 1 :: Int) Nil
  in quickBatch $ applicative xs

