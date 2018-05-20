{-# LANGUAGE InstanceSigs #-}

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

{-
f <$> fa :: f(b -> c)
f(b -> c) <*> fb :: fc

(r -> a -> b) - > (r -> a) -> (r -> b)

Dog <$> getName <*> getAddress


(n -> a -> d) <$> (p -> n) :: p -> a -> d

(p -> a -> d) <*> (p -> a) :: p -> d
-}

newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader $ id

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap :: (a -> b)
        -> Reader r a
        -> Reader r b
  fmap f (Reader ra) =
    Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\r -> a)

  (<*>) :: Reader r (a -> b)
        -> Reader r a
        -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> ((rab r) <$> ra) r
    
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r ->
      (runReader . aRb . ra) r $ r

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
  
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address
  
getDogRM :: Person -> Dog
getDogRM = do
    namy <- dogName
    addy <- address
    return $ Dog namy addy

  
pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")
