toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0 = []
toDigitsRev n = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [n] = [n]
doubleEveryOtherRev (x : y : xs) = x : (2*y) : (doubleEveryOtherRev xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = (sum (toDigits x)) + sumDigits xs

validate :: Integer -> Bool
validate x = y `mod` 10 == 0 where
    y = (sumDigits . doubleEveryOther . toDigits) x

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a

data Thing = Shoe 
           | Ship 
           | SealingWax 
           | Cabbage 
           | King
  deriving Show
  
shoe :: Thing
shoe = Shoe

data FailableDouble = Failure
                    | OK Double
  deriving Show
