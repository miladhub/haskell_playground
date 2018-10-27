module MonadEx1 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where 
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where 
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

main :: IO ()
main =
  let xs = undefined :: Nope (String, String, Int)
  in do
    quickBatch $ functor xs
    quickBatch $ applicative xs
    quickBatch $ monad xs
