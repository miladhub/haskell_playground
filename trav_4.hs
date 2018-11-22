module Trav4 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap f (Cons a l) = (Cons $ f a) (fmap f l)
  fmap _ _ = Nil

instance Foldable List where
  foldr f z (Cons a l) = f a (foldr f z l)
  foldr _ z Nil        = z

instance Traversable List where
  sequenceA (Cons a l) = Cons <$> a <*> (sequenceA l)
  sequenceA Nil        = pure Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [
      (1, Cons <$> arbitrary <*> arbitrary),
      (1, return Nil)
    ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: List (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
