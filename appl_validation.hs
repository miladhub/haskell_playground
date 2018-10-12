module MyValidation where

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap = undefined

instance Monoid e =>
  Applicative (Validation e) where
  pure = undefined
  (<*>) = undefined

