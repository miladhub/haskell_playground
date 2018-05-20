class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show)
    
instance TooMany Goats where
    tooMany (Goats n) = tooMany n

newtype IntString = MkIntString (Int, String)

instance TooMany IntString where
    tooMany (MkIntString (i,s)) = i > 30

newtype IntInt = IntInt (Int, Int)

instance TooMany IntInt where
    tooMany (IntInt (i, j)) = (i + j) > 25

newtype NumTooMany a = NumTooMany (a, a) deriving (Eq, Show)

instance (Num a, TooMany a) => TooMany (NumTooMany a) where
    tooMany (NumTooMany (a1, a2)) = (tooMany a1) && (tooMany a2)


