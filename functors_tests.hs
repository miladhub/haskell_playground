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
