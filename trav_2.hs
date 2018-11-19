module Trav2 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ z (Constant _) = z

instance Traversable (Constant a) where
  sequenceA (Constant a) = pure (Constant a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Constant Int (Int, Int, [Int])
  in do
    quickBatch $ functor xs
    quickBatch $ traversable xs
