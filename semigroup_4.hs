module MySemigroup where

import Data.Semigroup
import Test.QuickCheck

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a' b' c') <> (Three a'' b'' c'') = Three (a' <> a'') (b' <> b'') (c' <> c'')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type ThreeAssoc = (Three String String String) -> (Three String String String) -> (Three String String String) -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: ThreeAssoc)

