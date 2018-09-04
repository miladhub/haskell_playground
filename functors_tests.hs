{-# LANGUAGE ViewPatterns #-}

module FunctorsTests where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- type IntFC = [Int] -> (Fun Int Int) -> (Fun Int Int) -> Bool
-- quickCheck (functorCompose :: IntFC)

newtype Identity a = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityIntFC = Identity Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
-- quickCheck (functorCompose :: IdentityIntFC)

data Pair a = Pair a a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a' a) = Pair (f a') (f a)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

type PairIntFC = Pair Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
-- quickCheck (functorCompose :: PairIntFC)

data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoIntFC = Two Int Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
-- quickCheck (functorCompose :: TwoIntFC)

data Three a b c = Three a b c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

type ThreeIntFC = Three Int Int Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
-- quickCheck (functorCompose :: ThreeIntFC)

data Three' a b = Three' a b b
  deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b' b'') = Three' a (f b') (f b'')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

type ThreeIntFC' = Three' String Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
-- quickCheck (functorCompose :: ThreeIntFC')

data Four a b c d = Four a b c d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourIntFC = Four String String String Int -> (Fun Int String) -> (Fun String Int) -> Bool
-- quickCheck (functorCompose :: FourIntFC)

data Four' a b = Four' a a a b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a' a'' a''' b) = Four' a' a'' a''' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type FourIntFC' = Four' String Int -> (Fun Int Int) -> (Fun Int Int) -> Bool
-- quickCheck (functorCompose :: FourIntFC')


