module MonadEx3 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where 
  pure = Identity
  (Identity fa) <*> (Identity a) = Identity $ fa a

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Identity (String, Int, String)
  in do
    quickBatch $ functor xs
    quickBatch $ applicative xs
    quickBatch $ monad xs
