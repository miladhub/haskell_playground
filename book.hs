module Work where

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
 TisAnInt x == TisAnInt x' = x == x'
 TisAString x == TisAString x' = x == x'
 _ == _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
 Pair x y == Pair x' y' = x == x' && y == y'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
 Tuple a b == Tuple a' b' = a == a' && b == b'
 