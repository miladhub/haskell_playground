module MySemigroup where

import Data.Semigroup
import Test.QuickCheck

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return $ BoolConj True), (1, return $ BoolConj False)]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type OneAssoc = BoolConj
type ThreeAssoc = OneAssoc -> OneAssoc -> OneAssoc -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: ThreeAssoc)

