module Trav6 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a b =
  Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f z (Pair a b) = f b z

instance Traversable (Pair a) where
  sequenceA (Pair a b) = (Pair a) <$> b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary =
    Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Pair Int (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
