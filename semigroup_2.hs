module MySemigroup where

import Data.Semigroup
import Test.QuickCheck

newtype Identity a = Identity a deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a') <> (Identity a'') = (Identity $ a' <> a'')

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: IdentityAssoc)

