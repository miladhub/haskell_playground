module MyApp where

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
  
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f ca = Constant $ getConstant ca

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty 
  cab <*> _ = Constant $ getConstant cab


