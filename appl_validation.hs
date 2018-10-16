module MyValidation where

import Data.Monoid
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success a) = Success $ f a
  fmap _ (Failure e) = Failure e

instance Monoid e =>
  Applicative (Validation e) where
  pure = Success
  (Success f) <*> (Success a) = Success $ f a
  (Failure e1) <*> (Failure e2) = Failure $ e1 <> e2
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary =
    frequency [
      (1, Success <$> arbitrary),
      (1, Failure <$> arbitrary)
    ]

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = Success $ ("b", "w", 1 :: Int) :: Validation String (String, String, Int)
  in quickBatch $ applicative xs

data Errors =
  DividedByZero
  | StackOverflow
  | MooglesChewedWires deriving (Eq, Show)


