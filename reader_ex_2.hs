{-# LANGUAGE InstanceSigs #-}

module MyReaderEx2 where

import Control.Applicative

newtype Reader r a =
  Reader { runReader :: r -> a }

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b)
    -> Reader r a
    -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> (rab r) (ra r)
    --OR: Reader $ rab <*> ra

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
    -> (a -> Reader r b)
    -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> ( runReader . aRb . ra $ r) r

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address = 
  Address String 
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName 
    , dogName :: DogName
    , address :: Address
  } deriving (Eq, Show)

data Dog = 
  Dog {
    dogsName :: DogName
    , dogsAddress :: Address 
  } deriving (Eq, Show)

p :: Person
p = Person
  ( HumanName "Foo" )
  ( DogName "Fufy" )
  ( Address "Here" )

d :: Dog
d = Dog
  ( DogName "Dogy" )
  ( Address "There" )

getDogApp :: Person -> Dog
getDogApp =
  Dog <$> dogName <*> address

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addr <- address
  return $ Dog name addr

