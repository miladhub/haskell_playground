module MyZipListApplicative where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _   = Nil
take' n (Cons a t) = Cons a (take' (n - 1) t)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = (Cons $ f a) $ fmap f l

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  (Cons f t) <*> l = (fmap f l) <> (t <*> l)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil l = l
  mappend (Cons a t) l = Cons a (mappend t l)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' a = Cons a $ repeat' a

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  ZipList' a <*> ZipList' b = ZipList' $ zipWithList ($) a b

zipWithList :: (a -> b -> c) -> List a -> List b -> List c
zipWithList f (Cons a ta) (Cons b tb) = Cons (f a b) $ zipWithList f ta tb
zipWithList _ _           _           = Nil

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [
      (1, return Nil),
      (1, Cons <$> arbitrary <*> arbitrary)
    ]

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

main :: IO ()
main =
  let xs = ZipList' $ Cons ("b", "w", 1 :: Int) Nil
  in quickBatch $ applicative xs
