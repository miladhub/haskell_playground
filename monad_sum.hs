module MonadSum where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where 
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

instance Applicative (Sum a) where 
  pure = Second
  (Second fb) <*> (Second b) = Second $ fb b
  (First a) <*> _ = First a
  (Second fb) <*> (First a) = First a

instance Monad (Sum a) where
  return = pure
  (Second b) >>= f = f b
  (First a) >>= _ = First a 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary =
    frequency [
      (1, First <$> arbitrary),
      (1, Second <$> arbitrary)
    ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main =
  let xs = First ("b", "w", 1 :: Int) :: Sum (String, String, Int) (String, Int, String)
  in do
    quickBatch $ functor xs
    quickBatch $ applicative xs
    quickBatch $ monad xs
