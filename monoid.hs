module MyMonoid where

import Data.Monoid
import Control.Monad
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
  => Monoid (Optional a) where
  mempty = Only $ mempty
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only a') = Only $ mappend a a'
  mappend Nada Nada = Nada

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv, " as he jumped into his car ",
  noun, " and drove off with his ", adj, " wife."]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

{-
type S = String
type B = Bool
quickCheck (monoidAssoc :: S -> S -> S -> B)
quickCheck (monoidLeftIdentity :: String -> Bool)
quickCheck (monoidRightIdentity :: String -> Bool)
-}

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

--instance Semigroup Bull where
--  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mlr :: Bull -> Bool)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)
