module MySemigroup where

import Data.Semigroup
import Test.QuickCheck

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Snd b) = Snd b
  (Snd b) <> _ = Snd b
  (Fst a') <> (Fst a'') = Fst a''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [(1, Fst <$> arbitrary), (1, Snd <$> arbitrary)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type MyAssoc = Or String String -> Or String String -> Or String String -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: MyAssoc)

