module Trav5 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three a b c =
  Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
  sequenceA (Three a b c) = (Three a b) <$> c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary =
    Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Three Int Int (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
